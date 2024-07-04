// sequence.sv:  virtual sequence
// Copyright (C) 2022-2024 CESNET z. s. p. o.
// Author(s): Daniel Kriz <danielkriz@cesnet.cz>
//            Vladislav Valek <valekv@cesnet.cz>

// SPDX-License-Identifier: BSD-3-Clause

class virt_seq #(
    USR_MFB_REGIONS,
    USR_MFB_REGION_SIZE,
    USR_MFB_BLOCK_SIZE,
    USR_MFB_ITEM_WIDTH,
    CHANNELS,
    HDR_META_WIDTH,
    PKT_SIZE_MAX
) extends uvm_sequence;

    `uvm_object_param_utils(test::virt_seq #(
        USR_MFB_REGIONS,
        USR_MFB_REGION_SIZE,
        USR_MFB_BLOCK_SIZE,
        USR_MFB_ITEM_WIDTH,
        CHANNELS,
        HDR_META_WIDTH,
        PKT_SIZE_MAX
    ))

    `uvm_declare_p_sequencer(uvm_tx_dma_calypte::sequencer #(
        USR_MFB_REGIONS,
        USR_MFB_REGION_SIZE,
        USR_MFB_BLOCK_SIZE,
        USR_MFB_ITEM_WIDTH,
        CHANNELS,
        PKT_SIZE_MAX
    ))

    localparam USR_MFB_META_WIDTH = HDR_META_WIDTH + $clog2(PKT_SIZE_MAX+1) + $clog2(CHANNELS);

    uvm_reset::sequence_start                                                                                                                  m_reset_seq;
    uvm_sequence #(uvm_mfb::sequence_item #(USR_MFB_REGIONS, USR_MFB_REGION_SIZE, USR_MFB_BLOCK_SIZE, USR_MFB_ITEM_WIDTH, USR_MFB_META_WIDTH)) m_pcie_seq;
    uvm_tx_dma_calypte::sequence_simple                                                                                                        m_channel_seq [CHANNELS];

    local logic [CHANNELS-1:0] m_done;

    function new (string name = "virt_seq");
        super.new(name);
    endfunction

    virtual function void init();
        uvm_mfb::sequence_lib_tx#(USR_MFB_REGIONS, USR_MFB_REGION_SIZE, USR_MFB_BLOCK_SIZE, USR_MFB_ITEM_WIDTH, USR_MFB_META_WIDTH) m_pcie_seq_lib;

        m_reset_seq = uvm_reset::sequence_start::type_id::create("rst_seq");

        for (int unsigned it = 0; it < CHANNELS; it++) begin
            m_channel_seq[it].packet_size_max = PKT_SIZE_MAX;
            m_channel_seq[it] = uvm_tx_dma_calypte::sequence_simple::type_id::create($sformatf("channel_%0d", it));
        end

        m_pcie_seq_lib = uvm_mfb::sequence_lib_tx#(USR_MFB_REGIONS, USR_MFB_REGION_SIZE, USR_MFB_BLOCK_SIZE, USR_MFB_ITEM_WIDTH, USR_MFB_META_WIDTH)::type_id::create("m_pcie_seq_lib");
        m_pcie_seq_lib.init_sequence();
        m_pcie_seq = m_pcie_seq_lib;   // NOTE: WHY????!
    endfunction

    virtual task run_mfb();
        forever begin
            assert(m_pcie_seq.randomize());
            m_pcie_seq.start(p_sequencer.m_pcie_sqcr);
        end
    endtask

    virtual task run_reset();
        m_reset_seq.randomize();
        m_reset_seq.start(p_sequencer.m_reset_sqcr);
    endtask

    virtual task run_channels();
        uvm_common::sequence_cfg_transactions seq_cfg;

        seq_cfg = new();
        seq_cfg.transactions_min =  5000;
        seq_cfg.transactions_max = 30000;
        seq_cfg.randomize();
        #(200ns);

        for (int unsigned it = 0; it < CHANNELS; it++) begin
            fork
                automatic int unsigned index = it;
                begin
                    uvm_config_db#(uvm_common::sequence_cfg)::set(p_sequencer.m_packet[index], "", "state", seq_cfg);

                    m_channel_seq[index].randomize();
                    m_channel_seq[index].start(p_sequencer.m_packet[index]);
                    m_done[index] = 1;
                end
            join_none
        end
    endtask

    task body();
        m_done = 0;

        fork
            run_reset();
            run_channels();
        join_none

        #(200ns);

        fork
            run_mfb();
        join_none

        wait((& m_done) == 1);
    endtask
endclass
