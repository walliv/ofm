// base.sv: Basic test
// Copyright (C) 2021-2024 CESNET z. s. p. o.
// Author(s): Daniel Kriz <danielkriz@cesnet.cz>
//            Vladislav Válek <valekv@cesnet.cz>

// SPDX-License-Identifier: BSD-3-Clause

class base extends uvm_test;
    typedef uvm_component_registry#(test::base, "test::base") type_id;

    uvm_tx_dma_calypte::env #(
        DEVICE,
        MI_WIDTH,

        USR_TX_MFB_REGIONS,
        USR_TX_MFB_REGION_SIZE,
        USR_TX_MFB_BLOCK_SIZE,
        USR_TX_MFB_ITEM_WIDTH,

        PCIE_CQ_MFB_REGIONS,
        PCIE_CQ_MFB_REGION_SIZE,
        PCIE_CQ_MFB_BLOCK_SIZE,
        PCIE_CQ_MFB_ITEM_WIDTH,

        CHANNELS,
        DATA_POINTER_WIDTH,
        PKT_SIZE_MAX,
        PCIE_LEN_MAX
    ) m_env;

    uvm_reg_data_t pkt_cnt          [CHANNELS];
    uvm_reg_data_t byte_cnt         [CHANNELS];
    uvm_reg_data_t discard_pkt_cnt  [CHANNELS];
    uvm_reg_data_t discard_byte_cnt [CHANNELS];
    uvm_status_e   status_r;

    // ------------------------------------------------------------------------
    // Functions
    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction

    static function type_id get_type();
        return type_id::get();
    endfunction

    function string get_type_name();
        return get_type().get_type_name();
    endfunction

    function void build_phase(uvm_phase phase);
                                  USR_TX_MFB_REGIONS, USR_TX_MFB_REGION_SIZE, USR_TX_MFB_BLOCK_SIZE, USR_TX_MFB_ITEM_WIDTH,
                                  PCIE_CQ_MFB_REGIONS, PCIE_CQ_MFB_REGION_SIZE, PCIE_CQ_MFB_BLOCK_SIZE, PCIE_CQ_MFB_ITEM_WIDTH,
                                  CHANNELS, DATA_POINTER_WIDTH, PKT_SIZE_MAX, PCIE_LEN_MAX )::type_id::create("m_env", this);
        m_env = uvm_tx_dma_calypte::env #(DEVICE, MI_WIDTH,
    endfunction

    // ------------------------------------------------------------------------
    // Create environment and Run sequences o their sequencers
    virtual task run_phase(uvm_phase phase);
        time time_start;
        virt_seq #(USR_TX_MFB_REGIONS, USR_TX_MFB_REGION_SIZE, USR_TX_MFB_BLOCK_SIZE, USR_TX_MFB_ITEM_WIDTH,
                   CHANNELS, HDR_META_WIDTH, PKT_SIZE_MAX) m_virt_seq;

        //CREATE SEQUENCES
        m_virt_seq = virt_seq #(USR_TX_MFB_REGIONS, USR_TX_MFB_REGION_SIZE, USR_TX_MFB_BLOCK_SIZE, USR_TX_MFB_ITEM_WIDTH,
                                CHANNELS, HDR_META_WIDTH, PKT_SIZE_MAX)::type_id::create("m_virt_seq");

        //RISE OBJECTION
        phase.raise_objection(this);

        m_virt_seq.init();
        m_virt_seq.randomize();
        m_virt_seq.start(m_env.m_sequencer);

        time_start = $time();
        while((time_start + 500us) > $time() && m_env.sc.used()) begin
            #(600ns);
        end

        for (int unsigned chan = 0; chan < CHANNELS; chan++) begin
            m_env.m_regmodel.m_regmodel.channel[chan].sent_packets.write(status_r, {32'h1, 32'h1});
            m_env.m_regmodel.m_regmodel.channel[chan].sent_packets.read(status_r, pkt_cnt[chan]);
            m_env.m_regmodel.m_regmodel.channel[chan].sent_bytes.write(status_r, {32'h1, 32'h1});
            m_env.m_regmodel.m_regmodel.channel[chan].sent_bytes.read(status_r, byte_cnt[chan]);

            m_env.m_regmodel.m_regmodel.channel[chan].discarded_packets.write(status_r, {32'h1, 32'h1});
            m_env.m_regmodel.m_regmodel.channel[chan].discarded_packets.read(status_r, discard_pkt_cnt[chan]);
            m_env.m_regmodel.m_regmodel.channel[chan].discarded_bytes.write(status_r, {32'h1, 32'h1});
            m_env.m_regmodel.m_regmodel.channel[chan].discarded_bytes.read(status_r, discard_byte_cnt[chan]);

            m_env.sc.byte_cnt[chan]         = byte_cnt[chan];
            m_env.sc.pkt_cnt[chan]          = pkt_cnt[chan];
            m_env.sc.discard_byte_cnt[chan] = discard_byte_cnt[chan];
            m_env.sc.discard_pkt_cnt[chan]  = discard_pkt_cnt[chan];
        end

        phase.drop_objection(this);
    endtask

    virtual function void end_of_elaboration_phase(uvm_phase phase);
        super.end_of_elaboration_phase(phase);
        uvm_top.print_topology();
    endfunction

    function void report_phase(uvm_phase phase);
        `uvm_info(this.get_full_name(), {"\n\tTEST : ", this.get_type_name(), " END\n"}, UVM_NONE);
    endfunction

endclass
