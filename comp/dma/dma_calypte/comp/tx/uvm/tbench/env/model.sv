//-- model.sv: Model of implementation
//-- Copyright (C) 2022 CESNET z. s. p. o.
//-- Author(s): Daniel Kriz <danielkriz@cesnet.cz>

//-- SPDX-License-Identifier: BSD-3-Clause

class discard #(CHANNELS) extends uvm_component;
    `uvm_component_param_utils(uvm_dma_ll::discard #(CHANNELS))

    uvm_tlm_analysis_fifo #(uvm_logic_vector::sequence_item#(1)) analysis_imp_rx_dma;

    function new(string name, uvm_component parent = null);
        super.new(name, parent);
        analysis_imp_rx_dma = new("analysis_imp_rx_dma", this);
    endfunction

    task get_tr(output logic drop);
        uvm_logic_vector::sequence_item#(1) drop_tr;
        analysis_imp_rx_dma.get(drop_tr);
        drop = drop_tr.data;
    endtask
endclass

//model
class model #(CHANNELS, USR_ITEM_WIDTH, USER_META_WIDTH, CQ_ITEM_WIDTH, DATA_ADDR_W) extends uvm_component;
    `uvm_component_param_utils(uvm_dma_ll::model #(CHANNELS, USR_ITEM_WIDTH, USER_META_WIDTH, CQ_ITEM_WIDTH, DATA_ADDR_W))

    localparam DATA_ADDR_MASK = 2**DATA_ADDR_W-1;

    uvm_tlm_analysis_fifo #(uvm_common::model_item#(uvm_logic_vector_array::sequence_item#(CQ_ITEM_WIDTH)))                            analysis_imp_rx_data;
    uvm_tlm_analysis_fifo #(uvm_common::model_item#(uvm_logic_vector::sequence_item#(sv_pcie_meta_pack::PCIE_CQ_META_WIDTH)))          analysis_imp_rx_meta;
    uvm_analysis_port     #(uvm_common::model_item#(uvm_logic_vector_array::sequence_item#(USR_ITEM_WIDTH)))                           analysis_port_tx_data;
    uvm_analysis_port     #(uvm_common::model_item#(uvm_logic_vector::sequence_item#(USER_META_WIDTH)))                                analysis_port_tx_meta;
    local uvm_dma_regs::regmodel#(CHANNELS)                                                                   m_regmodel;

    uvm_dma_ll::discard#(CHANNELS) discard_comp;
    protected int unsigned  pcie_transactions;
    protected int unsigned  dma_transactions;
    protected int unsigned  drop_transactions;



    typedef struct{
        int unsigned pcie_transactions;
        int unsigned dma_transactions;
        int unsigned drop_transactions;

        time  infs[string];

        logic [8-1:0] memory [2**DATA_ADDR_W];
    } channel_info_t;
    protected channel_info_t channel_info[CHANNELS];


    function new (string name, uvm_component parent = null);
        super.new(name, parent);
        analysis_imp_rx_data  = new("analysis_imp_rx_data", this);
        analysis_imp_rx_meta  = new("analysis_imp_rx_meta", this);
        analysis_port_tx_data = new("analysis_port_tx_data", this);
        analysis_port_tx_meta = new("analysis_port_tx_meta", this);

        pcie_transactions = 0;
        dma_transactions  = 0;
        drop_transactions  = 0;
        for (int unsigned it = 0; it < CHANNELS; it++) begin
            channel_info[it].pcie_transactions = 0;
            channel_info[it].dma_transactions = 0;
            channel_info[it].drop_transactions = 0;
        end
    endfunction

    function void regmodel_set(uvm_dma_regs::regmodel#(CHANNELS) m_regmodel);
        this.m_regmodel = m_regmodel;
    endfunction

    function void time_add (int unsigned channel, time inf_time[string], int unsigned id);
        foreach(inf_time[it]) begin
           channel_info[channel].infs[$sformatf("%s(%0d)", it, id)] = inf_time[it];
        end
    endfunction

    function int unsigned used();
        int unsigned ret = 0;
        ret |= (analysis_imp_rx_data.used() != 0);
        ret |= (analysis_imp_rx_meta.used() != 0);
        return ret;
    endfunction

    function void build_phase(uvm_phase phase);
        discard_comp = uvm_dma_ll::discard#(CHANNELS)::type_id::create("discard_comp", this);
    endfunction

    function int unsigned encode_fbe(logic [CQ_ITEM_WIDTH/8-1 : 0] be);
        int unsigned it = 0;

        if (be != 0) begin
            while (it < CQ_ITEM_WIDTH/8 && be[it] == 0) begin
                it++;
            end
        end
        return it;
    endfunction

    function int unsigned encode_lbe(logic [CQ_ITEM_WIDTH/8-1 : 0] be);
        int unsigned it  = CQ_ITEM_WIDTH/8;

        if (be != 0) begin
            while (it > 0 && be[it-1] == 0) begin
                it--;
            end;
        end
        return it;
    endfunction


    task run_phase(uvm_phase phase);

        uvm_common::model_item #(uvm_logic_vector_array::sequence_item#(CQ_ITEM_WIDTH))                   in_data_tr;
        uvm_common::model_item #(uvm_logic_vector::sequence_item#(sv_pcie_meta_pack::PCIE_CQ_META_WIDTH)) in_meta_tr;
        uvm_common::model_item #(uvm_logic_vector_array::sequence_item#(USR_ITEM_WIDTH)) out_data_tr;
        uvm_common::model_item #(uvm_logic_vector::sequence_item#(USER_META_WIDTH))      out_meta_tr;

        string debug_msg;
        logic  drop;
        logic [DATA_ADDR_W-1:2] addr;
        int unsigned dword_cnt;
        logic [$clog2(CHANNELS)-1:0] channel;
        logic [1-1:0]           hdr_inf;
        logic                   res;
        int unsigned fbe;
        int unsigned lbe;

        forever begin
            int unsigned start_it;
            logic [64-1:0] pcie_addr;
            //GET PCIE TRANSACTION
            analysis_imp_rx_data.get(in_data_tr);
            analysis_imp_rx_meta.get(in_meta_tr);
            //drop = 0;
            discard_comp.get_tr(drop);

            dword_cnt  = in_data_tr.item.data[2][11-1 : 0];
            if (dword_cnt == 0) begin
                dword_cnt  = 1024;
            end

            pcie_addr  = {in_data_tr.item.data[1], in_data_tr.item.data[0]};
            addr       = pcie_addr[DATA_ADDR_W-1 : 2];
            channel    = pcie_addr[(DATA_ADDR_W+1+$clog2(CHANNELS))-1 : DATA_ADDR_W+1];
            hdr_inf    = pcie_addr[(DATA_ADDR_W+1+$clog2(CHANNELS))];

            pcie_transactions++;
            channel_info[channel].pcie_transactions++;
            debug_msg = "\n";
            debug_msg = { debug_msg, $sformatf("================================================================================= \n")};
            debug_msg = { debug_msg, $sformatf("MODEL INPUT PCIe TRANSACTION %0d\n", pcie_transactions)};
            debug_msg = { debug_msg, $sformatf("================================================================================= \n")};
            debug_msg = { debug_msg, $sformatf("CHANNEL     : %0d\n", channel)};
            debug_msg = { debug_msg, $sformatf("TRANSACTION : %0d\n", channel_info[channel].pcie_transactions)};
            debug_msg = { debug_msg, $sformatf("DROP        : %0d\n", drop)};
            debug_msg = { debug_msg, $sformatf("ADDR        : %0d\n", {addr, 2'b00})};
            debug_msg = { debug_msg, $sformatf("HDR FLAG  : %0b\n", (hdr_inf != 1'b0))};
            debug_msg = { debug_msg, $sformatf("DW CNT    : %0d\n", dword_cnt)};
            debug_msg = { debug_msg, $sformatf("FBE       : %b\n", in_meta_tr.item.data[167-1 : 163])};
            debug_msg = { debug_msg, $sformatf("LBE       : %b\n", dword_cnt > 1 ? in_meta_tr.item.data[171-1 : 167] : in_meta_tr.item.data[167-1 : 163])};
            debug_msg = { debug_msg, $sformatf("DATA      : %s\n", in_data_tr.convert2string())};
            debug_msg = { debug_msg, $sformatf("================================================================================= \n")};
            `uvm_info(this.get_full_name(), debug_msg, UVM_FULL);

            //if PCIE transaction is not DMA HEADER
            if (hdr_inf == 1'b0) begin
                fbe = encode_fbe(in_meta_tr.item.data[167-1 : 163]);
                if (dword_cnt <= 1) begin
                    lbe = encode_lbe(in_meta_tr.item.data[167-1 : 163]);
                    for (int unsigned it = fbe; it < lbe; it++) begin
                         channel_info[channel].memory[{addr, 2'b00} + it] = in_data_tr.item.data[4][(it+1)*8-1 -: 8];
                    end
                end else begin
                    logic [DATA_ADDR_W-1:0] addr_act = {addr, 2'b00};
                    lbe = encode_lbe(in_meta_tr.item.data[171-1 : 167]);
                    //peeling start
                    for (int unsigned it = fbe; it < 4; it++) begin
                         channel_info[channel].memory[addr_act + it] = in_data_tr.item.data[4][(it+1)*8-1 -: 8];
                    end
                    addr_act = (addr_act + 4) & DATA_ADDR_MASK;
                    //Main loop
                    for (int unsigned it = 1; (it+1) < dword_cnt; it++) begin
                         {<<8{channel_info[channel].memory[addr_act +: 4]}} = in_data_tr.item.data[4 + it];
                         addr_act = (addr_act + 4) & DATA_ADDR_MASK;
                    end
                    //peeling end
                    for (int unsigned it = 0; it < lbe; it++) begin
                         channel_info[channel].memory[addr_act + it] = in_data_tr.item.data[4 + dword_cnt-1][(it+1)*8-1 -: 8];
                    end
                end

                this.time_add (channel, in_data_tr.start, pcie_transactions);
                this.time_add (channel, in_meta_tr.start, pcie_transactions);
            end else begin
            //if PCIE transaction is DMA HEADER then create output packečt and send it
                logic [16-1 : 0] packet_size;
                logic [24-1 : 0] dma_meta;
                logic [DATA_ADDR_W-1 : 0] frame_pointer;

                packet_size   = in_data_tr.item.data[4][16-1 : 0];
                frame_pointer = in_data_tr.item.data[4][32-1 : 16];
                dma_meta      = in_data_tr.item.data[5][32-1 : 8];

                if (drop == 1'b0) begin
                    out_data_tr      = uvm_common::model_item #(uvm_logic_vector_array::sequence_item #(USR_ITEM_WIDTH))::type_id::create("out_data_tr", this);
                    out_data_tr.item = uvm_logic_vector_array::sequence_item #(USR_ITEM_WIDTH)::type_id::create("out_data_tr.item", this);
                    out_meta_tr      = uvm_common::model_item #(uvm_logic_vector::sequence_item #(USER_META_WIDTH))::type_id::create("out_meta_tr", this);
                    out_meta_tr.item = uvm_logic_vector::sequence_item #(USER_META_WIDTH)::type_id::create("out_meta_tr.item", this);

                    out_data_tr.start = channel_info[channel].infs;
                    out_data_tr.time_array_add(in_data_tr.start);
                    out_data_tr.time_array_add(in_meta_tr.start);
                    out_meta_tr.start = channel_info[channel].infs;
                    out_meta_tr.time_array_add(in_data_tr.start);
                    out_meta_tr.time_array_add(in_meta_tr.start);

                    out_data_tr.item.data = new[packet_size];
                    for (int unsigned it = 0; it < packet_size; it++) begin
                        int unsigned dma_mem_addr = (frame_pointer + it) & DATA_ADDR_MASK;
                        out_data_tr.item.data[it] = channel_info[channel].memory[dma_mem_addr];
                    end
                    out_meta_tr.item.data = {packet_size, channel, dma_meta};

                    channel_info[channel].dma_transactions++;

                    debug_msg = "\n";
                    debug_msg = {debug_msg, $sformatf("================================================================================= \n")};
                    debug_msg = {debug_msg, $sformatf("MODEL OUTPUT DMA TRANSACTION %0d\n", dma_transactions)};
                    debug_msg = {debug_msg, $sformatf("================================================================================= \n")};
                    debug_msg = {debug_msg, $sformatf("CHANNEL              : %0d\n", channel)};
                    debug_msg = {debug_msg, $sformatf("TRANSACTION          : %0d\n", channel_info[channel].dma_transactions)};
                    debug_msg = {debug_msg, $sformatf("FRAME POINTER        : %0d\n", frame_pointer)};
                    debug_msg = {debug_msg, $sformatf("SIZE IN BYTES        : %0d\n", packet_size)};
                    debug_msg = {debug_msg, $sformatf("================================================================================= \n")};
                    debug_msg = {debug_msg, $sformatf("OUT META: %s\n", out_meta_tr.convert2string())};
                    debug_msg = {debug_msg, $sformatf("OUT DATA: %s\n", out_data_tr.convert2string())};
                    debug_msg = {debug_msg, $sformatf("================================================================================= \n")};
                    `uvm_info(this.get_full_name(), debug_msg, UVM_HIGH)

                    analysis_port_tx_data.write(out_data_tr);
                    analysis_port_tx_meta.write(out_meta_tr);
                end else begin
                    drop_transactions++;

                    debug_msg = "\n";
                    debug_msg = {debug_msg, $sformatf("================================================================================= \n")};
                    debug_msg = {debug_msg, $sformatf("MODEL DROP %0d\n", drop_transactions)};
                    debug_msg = {debug_msg, $sformatf("================================================================================= \n")};
                    debug_msg = {debug_msg, $sformatf("CHANNEL              : %0d\n", channel)};
                    debug_msg = {debug_msg, $sformatf("TRANSACTION          : %0d\n", channel_info[channel].drop_transactions)};
                    debug_msg = {debug_msg, $sformatf("FRAME POINTER        : %0d\n", frame_pointer)};
                    debug_msg = {debug_msg, $sformatf("SIZE IN BYTES        : %0d\n", packet_size)};
                    debug_msg = {debug_msg, $sformatf("================================================================================= \n")};
                    debug_msg = {debug_msg, $sformatf("OUT META: %s\n", out_meta_tr.convert2string())};
                    debug_msg = {debug_msg, $sformatf("OUT DATA: %s\n", out_data_tr.convert2string())};
                    debug_msg = {debug_msg, $sformatf("================================================================================= \n")};
                    `uvm_info(this.get_full_name(), debug_msg, UVM_HIGH)

                end

                channel_info[channel].infs.delete();
            end
        end
    endtask
endclass
