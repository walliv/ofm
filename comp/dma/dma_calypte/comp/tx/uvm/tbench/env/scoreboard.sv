// scoreboard.sv: Scoreboard for verification
// Copyright (C) 2022-2024 CESNET z. s. p. o.
// Author(s): Daniel Kriz <danielkriz@cesnet.cz>
//            Vladislav Valek <valekv@cesnet.cz>

// SPDX-License-Identifier: BSD-3-Clause

class data_comparer #(ITEM_WIDTH) extends uvm_common::comparer_ordered #(uvm_logic_vector_array::sequence_item #(ITEM_WIDTH));
    `uvm_component_param_utils(uvm_tx_dma_calypte::data_comparer #(ITEM_WIDTH))

    function new(string name = "uvm_tx_dma_calypte.data_comparer", uvm_component parent = null);
        super.new(name, parent);
    endfunction

    virtual function string message(uvm_common::model_item #(MODEL_ITEM) tr_model, uvm_common::dut_item #(DUT_ITEM) tr_dut);
        string msg = "";
        int unsigned newline_break_cntr = 0;
        int unsigned last_wrong_byte_idx = 0;

        $swrite(msg, "%s\n\tDUT PACKET %s\n\n", msg, tr_dut.convert2string());
        $swrite(msg, "%s\n\tMODEL PACKET%s\n",  msg, tr_model.convert2string());

        $swrite(msg, "%s\n\tWRONG_BYTES:\n",  msg);

        if (tr_model.item.size() != tr_dut.in_item.size()) begin
            $write(msg, "%s\tTransaction lengths do not match: MODEL: %0d, DUT: %0d\n. Unable to compare!\n", msg, tr_model.item.data.size(), tr_dut.in_item.data.size());
        end else begin
            msg = $sformatf("%s\tTransaction lengths do match!\n", msg);
            msg = $sformatf("%s\t", msg);
           
            foreach (tr_dut.in_item.data[it]) begin
                if (tr_dut.in_item.data[it] != tr_model.item.data[it]) begin
                    if (last_wrong_byte_idx != (it -1))
                        msg = $sformatf("%s\n\n\t", msg);

                    msg = $sformatf("%s%0d: (%2h, %2h), ", msg, it, tr_dut.in_item.data[it], tr_model.item.data[it]);
                    newline_break_cntr++;
                    last_wrong_byte_idx = it;

                    if (newline_break_cntr >= 10) begin
                        msg = $sformatf("%s\n\t", msg);
                        newline_break_cntr = 0;
                    end

                end
            end
        end

        return msg;
    endfunction
endclass

class scoreboard #(USR_MFB_ITEM_WIDTH, PCIE_CQ_MFB_ITEM_WIDTH, CHANNELS, DATA_POINTER_WIDTH, USR_MFB_META_WIDTH) extends uvm_scoreboard;
    `uvm_component_param_utils(uvm_tx_dma_calypte::scoreboard #(USR_MFB_ITEM_WIDTH, PCIE_CQ_MFB_ITEM_WIDTH, CHANNELS, DATA_POINTER_WIDTH, USR_MFB_META_WIDTH))

    //INPUT TO DUT
    uvm_common::subscriber #(uvm_logic_vector_array::sequence_item #(PCIE_CQ_MFB_ITEM_WIDTH))          m_pcie_cq_data_subs;
    uvm_common::subscriber #(uvm_logic_vector::sequence_item #(sv_pcie_meta_pack::PCIE_CQ_META_WIDTH)) m_pcie_cq_meta_subs;
    uvm_analysis_export    #(uvm_logic_vector::sequence_item #(1))                                     m_pkt_drop_analysis_export;

    //DUT OUTPUT
    uvm_analysis_export #(uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH)) m_usr_data_analysis_export;
    uvm_analysis_export #(uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH))       m_usr_meta_analysis_export;

    model #(USR_MFB_ITEM_WIDTH, PCIE_CQ_MFB_ITEM_WIDTH, CHANNELS, DATA_POINTER_WIDTH, USR_MFB_META_WIDTH) m_model;

    local uvm_tx_dma_calypte_regs::regmodel_top #(CHANNELS)                                     m_regmodel_top;
    data_comparer #(USR_MFB_ITEM_WIDTH)                                                         m_data_cmp;
    uvm_common::comparer_ordered #(uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH))       m_meta_cmp;

    uvm_reg_data_t pkt_cnt          [CHANNELS];
    uvm_reg_data_t byte_cnt         [CHANNELS];
    uvm_reg_data_t discard_pkt_cnt  [CHANNELS];
    uvm_reg_data_t discard_byte_cnt [CHANNELS];
    uvm_status_e   status_r;

    local uvm_common::stats  m_delay;

    // Contructor of scoreboard.
    function new(string name, uvm_component parent);
        super.new(name, parent);
        m_usr_data_analysis_export  = new("m_usr_data_analysis_export", this);
        m_usr_meta_analysis_export  = new("m_usr_meta_analysis_export", this);
        m_pkt_drop_analysis_export  = new("m_pkt_drop_analysis_export", this);

        //LOCAL VARIABLES
        m_delay        = new();
    endfunction

    function int unsigned used();
        int unsigned ret = 0;
        ret |= m_model.used() != 0;
        ret |= m_data_cmp.used() != 0;
        ret |= m_meta_cmp.used() != 0;
        return ret;
    endfunction

    function void regmodel_set(uvm_tx_dma_calypte_regs::regmodel_top#(CHANNELS) m_regmodel);
        this.m_regmodel_top = m_regmodel;
        m_model.regmodel_set(m_regmodel);
    endfunction

    //build phase
    function void build_phase(uvm_phase phase);
        m_model    = model #(USR_MFB_ITEM_WIDTH, PCIE_CQ_MFB_ITEM_WIDTH, CHANNELS, DATA_POINTER_WIDTH, USR_MFB_META_WIDTH)::type_id::create("m_model",    this);
        m_data_cmp   = data_comparer #(USR_MFB_ITEM_WIDTH)                                                                ::type_id::create("m_data_cmp", this);
        m_meta_cmp   = uvm_common::comparer_ordered #(uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH))              ::type_id::create("m_meta_cmp", this);

        m_pcie_cq_data_subs = uvm_common::subscriber #(uvm_logic_vector_array::sequence_item #(PCIE_CQ_MFB_ITEM_WIDTH))         ::type_id::create("m_pcie_cq_data_subs",this);
        m_pcie_cq_meta_subs = uvm_common::subscriber #(uvm_logic_vector::sequence_item #(sv_pcie_meta_pack::PCIE_CQ_META_WIDTH))::type_id::create("m_pcie_cq_meta_subs",this);
    endfunction

    function void connect_phase(uvm_phase phase);
        m_pcie_cq_data_subs.port.connect(m_model.m_cq_data_analysis_fifo.analysis_export);
        m_pcie_cq_meta_subs.port.connect(m_model.m_cq_meta_analysis_fifo.analysis_export);

        m_model.m_usr_data_analysis_port.connect(m_data_cmp.analysis_imp_model);
        m_model.m_usr_meta_analysis_port.connect(m_meta_cmp.analysis_imp_model);

        m_usr_data_analysis_export.connect(m_data_cmp.analysis_imp_dut);
        m_usr_meta_analysis_export.connect(m_meta_cmp.analysis_imp_dut);
        m_pkt_drop_analysis_export.connect(m_model.m_discard_comp.m_internal_meta_analysis_fifo.analysis_export);
    endfunction

    function void print_counters(ref string msg, input string cntr_name, int unsigned dut_cntr, int unsigned model_cntr);
        $swrite(msg, "%s %s\n", msg, cntr_name);
        $swrite(msg, "%s DUT:   %0d\n", msg, dut_cntr);
        $swrite(msg, "%s MODEL: %0d\n", msg, model_cntr);
        $swrite(msg, "%s --------------------\n", msg);
        $swrite(msg, "%s DIFF:  %0d\n", msg, dut_cntr - model_cntr);
    endfunction

    function void report_phase(uvm_phase phase);
        real min;
        real max;
        real avg;
        real std_dev;
        real median;
        real modus;
        string msg = "\n";

        if (this.get_report_verbosity_level() >= UVM_LOW) begin
            m_delay.count(min, max, avg, std_dev);
            $swrite(msg, "%s\tDelay statistic (SOF to SOF) => min : %0dns, max : %0dns, average : %0dns, standard deviation : %0dns, median : %0dns, modus : %0dns\n", msg, min, max, avg, std_dev, median, modus);
        end

        if (this.used() == 0) begin

            for (int chan = 0; chan < CHANNELS; chan++) begin

                $swrite(msg, "%s\n=================================================================================\n", msg);
                $swrite(msg, "%s CHANNEL %0d\n", msg, chan);
                $swrite(msg, "%s=================================================================================\n", msg);

                if (byte_cnt[chan] != m_model.m_channel_info[chan].dma_transactions_bytes &&
                    pkt_cnt[chan]  != m_model.m_channel_info[chan].dma_transactions &&
                    discard_byte_cnt[chan] != m_model.m_channel_info[chan].drop_transactions_bytes &&
                    discard_pkt_cnt[chan]  != m_model.m_channel_info[chan].drop_transactions) begin

                    $swrite(msg, "%sPACKET COUNTERS DO NOT EQUAL!\n", msg);
                end

                if (pkt_cnt[chan]  != m_model.m_channel_info[chan].dma_transactions)
                    print_counters(msg, "SEND_PACKETS",    pkt_cnt[chan],          m_model.m_channel_info[chan].dma_transactions);

                if (byte_cnt[chan] != m_model.m_channel_info[chan].dma_transactions_bytes)
                    print_counters(msg, "SEND_BYTES",      byte_cnt[chan],         m_model.m_channel_info[chan].dma_transactions_bytes);

                if (discard_pkt_cnt[chan] != m_model.m_channel_info[chan].drop_transactions)
                    print_counters(msg, "DISCARD_PACKETS", discard_pkt_cnt[chan],  m_model.m_channel_info[chan].drop_transactions);

                if (discard_byte_cnt[chan] != m_model.m_channel_info[chan].drop_transactions_bytes)
                    print_counters(msg, "DISCARD_BYTES",   discard_byte_cnt[chan], m_model.m_channel_info[chan].drop_transactions_bytes);

                $swrite(msg, "%s\n----MODEL COUNTERS----\n", msg                                                   );
                $swrite(msg, "%sPKT_CNT            %d\n", msg, m_model.m_channel_info[chan].dma_transactions       );
                $swrite(msg, "%sBYTE_CNT           %d\n", msg, m_model.m_channel_info[chan].dma_transactions_bytes );
                $swrite(msg, "%sDISCARD_PKT_CNT    %d\n", msg, m_model.m_channel_info[chan].drop_transactions      );
                $swrite(msg, "%sDISCARD_BYTE_CNT   %d\n", msg, m_model.m_channel_info[chan].drop_transactions_bytes);

                $swrite(msg, "%s\n----DUT COUNTERS----\n", msg                       );
                $swrite(msg, "%sPKT_CNT            %d\n", msg, pkt_cnt[chan]         );
                $swrite(msg, "%sBYTE_CNT           %d\n", msg, byte_cnt[chan]        );
                $swrite(msg, "%sDISCARD_PKT_CNT    %d\n", msg, discard_pkt_cnt[chan] );
                $swrite(msg, "%sDISCARD_BYTE_CNT   %d\n", msg, discard_byte_cnt[chan]);
            end

            $swrite(msg, "%s================================================================================= \n", msg);

            `uvm_info(get_type_name(), {msg, "\n\n\t---------------------------------------\n\t----     VERIFICATION SUCCESS      ----\n\t---------------------------------------"}, UVM_NONE)
        end else begin
            string msg = "";
            `uvm_info(get_type_name(), {msg, "\n\n\t---------------------------------------\n\t----     VERIFICATION FAILED       ----\n\t---------------------------------------"}, UVM_NONE)
        end
    endfunction
endclass
