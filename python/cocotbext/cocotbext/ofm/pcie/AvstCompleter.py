import cocotb
import cocotb.queue
from cocotb.triggers import Timer, Event, RisingEdge
from cocotb.clock import Clock

import traceback
import sys
from ..utils import concat, deconcat, SerializableHeader


class RequestHeaderEmpty(SerializableHeader):
    items = list(zip([], []))

class RequestHeader(SerializableHeader):
    items = list(zip(['addr', 'fbe', 'lbe', 'tag_l', 'req_id', 'length', 'addr_t', 'att_l', 'res1', 'attr_h', 'tag_m', 'traff_cls', 'tag_h', 'req_t'],
                [64, 4, 4, 8, 16, 10, 2, 2, 4, 1, 1, 3, 1, 8]))

class CompletionHeader(SerializableHeader):
    items = list(zip(['tlp_prfx', 'lower_addr', 'res1', 'tag_l', 'req_id', 'byte_cnt', 'res2', 'compl_stat', 'compl_id', 'dwords', 'addr_type', 'attr_l', 'res3', 'attr_h', 'tag_m', 'traff_cls', 'tag_h', 'tlp_type', 'fmt'],
                     [32, 7, 1, 8, 16, 12, 1, 3, 16, 10, 2, 2, 4, 1, 1, 3, 1, 5, 3]))

class AvstCompleter:
    def __init__(self, cq_driver, cc_driver, cc_monitor):
        self._cq = cq_driver
        self._cc = cc_driver
        self._ccm = cc_monitor
        self._queue_send = cocotb.queue.Queue()
        self._queue_recv = cocotb.queue.Queue()
        self._avst_width = len(self._cq.bus.DATA) // 8

        self._cc_inframe = None
        self._completions = {}
        self._read_requests = {}
        self._tag_queue = cocotb.queue.PriorityQueue()
        [self._tag_queue.put_nowait(i) for i in range(2**5)]

        cc_monitor.add_callback(self._handle_cc_transaction)
        cocotb.start_soon(self._cq_loop())


    async def _cq_loop(self):
        re = RisingEdge(self._cq.clock)
        await re

        while True:
            if self._queue_send.empty():
                await re
                continue

            item, trigger = self._queue_send.get_nowait()
            tag = None
            if item[2] == 0:  # req_type=0 => read
                tag = await self._tag_queue.get()
                self._read_requests[tag] = (trigger, item, [])
            if tag == None:
                trigger.set()

            await self._cq_req(*item, tag=tag, sync=False)


    async def _cq_req(self, addr, byte_count, req_type=0, data=[], tag=None, sync=True):
        header_empty = RequestHeaderEmpty()
        header = RequestHeader()

        if req_type == 1:
            assert len(data) == byte_count
            data = [0] * (addr % 4) + data + [0] * ((addr + byte_count) % 4)

        addr_l, addr_h = deconcat([addr, 32, 32])
        header.addr = concat([(addr_h,32),(addr_l,32)]) & ~3
        header.fbe = [0xF, 0xE, 0xC, 0x8][addr % 4]
        dwords = (addr % 4 + byte_count + 3) // 4
        header.lbe = [0xF, 0x1, 0x3, 0x7][(addr + byte_count) % 4] if dwords > 1 else 0
        if tag != None:
            header.tag_l, header.tag_m, header.tag_h = deconcat([tag, 8, 1, 1])
        header.req_t = req_type << 6
        header.length = dwords

        header = header.serialize()
        sop = 1
        eop = 0
        empty = 0
        while len(data) > self._avst_width:
            data_word = concat(list(zip(data[:self._avst_width], [8]*self._avst_width)))
            await self._cq.write({"DATA": data_word, "HDR": header, "SOP": sop, "EOP": eop, "EMPTY": empty, "PREFIX": 0, "BAR_RANGE": 0}, sync=sync)

            header = header_empty
            sop = 0
            data = data[self._avst_width:]

        data_word = concat(list(zip(data[:len(data)], [8]*len(data))))
        eop = 1
        empty = self._avst_width - len(data) // 4 if len(data) > 0 else 0
        await self._cq.write_cq({"DATA": data_word, "HDR": header, "SOP": sop, "EOP": eop, "EMPTY": empty, "PREFIX": 0, "BAR_RANGE": 0}, sync=sync)


    def _handle_cc_transaction(self, transaction):
        header_bytes, data_bytes = transaction
        hdr = CompletionHeader.deserialize(int.from_bytes(header_bytes, byteorder='big'))
        data = list(data_bytes)

        # Process only if it is a completition (MI RD response)
        if hdr.tlp_type == int("0b01010", base=0) and (hdr.fmt) in [0, 2]:

            tag = concat([(hdr.tag_l, 8), (hdr.tag_m, 1), (hdr.tag_h, 1)])

            trigger, item, req_data = self._read_requests[tag]
            addr, byte_count, req_type, orig_data = item
            req_data = data[:byte_count]
            # firstBe = [0xF, 0xE, 0xC, 0x8][addr % 4]
            # lastBe = [0xF, 0x1, 0x3, 0x7][(addr + byte_count) % 4]
            # fixme: BE & length & completed = 0

            del self._read_requests[tag]
            trigger.set(req_data)
            self._tag_queue.put_nowait(tag)


    async def read(self, addr, byte_count):
        # TODO: split big reads to more transactions
        e = Event()
        await self._queue_send.put(((addr, byte_count, 0, []), e))
        await e.wait()
        return e.data


    async def write(self, addr, data):
        # TODO: split big writes to more transactions
        e = Event()
        await self._queue_send.put(((addr, len(data), 1, data), e))
        data = await e.wait()


    async def read64(self, addr):
        rawdata = await self.read(addr, 8)
        return int.from_bytes(bytes(rawdata), byteorder="little")


    async def read32(self, addr):
        rawdata = await self.read(addr, 4)
        return int.from_bytes(bytes(rawdata), byteorder="little")


    async def write32(self, addr, val):
        await self.write(addr, list(val.to_bytes(4, byteorder="little")))


    async def write64(self, addr, val):
        await self.write(addr, list(val.to_bytes(8, byteorder="little")))
