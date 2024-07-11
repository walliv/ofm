// scoreboard.sv: Scoreboard for verification
// Copyright (C) 2022-2024 CESNET z. s. p. o.
// Author(s): Daniel Kriz <danielkriz@cesnet.cz>
//            Vladislav Valek <valekv@cesnet.cz>

// SPDX-License-Identifier: BSD-3-Clause

class compare #(USR_MFB_ITEM_WIDTH, CHANNELS, USR_MFB_META_WIDTH) extends uvm_component;
    `uvm_component_utils(uvm_tx_dma_calypte::compare #(USR_MFB_ITEM_WIDTH, CHANNELS, USR_MFB_META_WIDTH))

    // NOTE: This is a strange declaration
    uvm_tlm_analysis_fifo #(uvm_common::model_item #(uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH))) m_model_data_analysis_fifo;
    uvm_tlm_analysis_fifo #(uvm_logic_vector      ::sequence_item                          #(USR_MFB_META_WIDTH))  m_model_meta_analysis_fifo;
    uvm_tlm_analysis_fifo #(uvm_logic_vector_array::sequence_item                          #(USR_MFB_ITEM_WIDTH))  m_dut_data_analysis_fifo;
    uvm_tlm_analysis_fifo #(uvm_logic_vector      ::sequence_item                          #(USR_MFB_META_WIDTH))  m_dut_meta_analysis_fifo;

    int unsigned      m_errors;
    int unsigned      m_compared;
    uvm_common::stats m_delay;

    function new(string name, uvm_component parent);
        super.new(name, parent);
        m_model_data_analysis_fifo = new("m_model_data_analysis_fifo", this);
        m_model_meta_analysis_fifo = new("m_model_meta_analysis_fifo", this);
        m_dut_data_analysis_fifo   = new("m_dut_data_analysis_fifo",   this);
        m_dut_meta_analysis_fifo   = new("m_dut_meta_analysis_fifo",   this);
        m_errors     = 0;
        m_compared   = 0;
    endfunction

    task print_meta_compare_error(uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH) tr_model_meta,
                                  uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH) tr_dut_meta);

        string msg;

        $swrite(msg, "%s\n\nMETA COMPARISON CHANNEL %d", msg, tr_model_meta.data[$clog2(CHANNELS)+24-1 : 24]);
        $swrite(msg, "%s\n===============================================\n", msg);
        $swrite(msg, "%s\n Comparison failed at meta number %d! \n\tModel meta:\n%s\n\tDUT meta:\n%s\n", msg, m_compared, tr_model_meta.convert2string(), tr_dut_meta.convert2string());
        $swrite(msg, "%s\n DMA MODEL META %h", msg, tr_model_meta.data[23 : 0]);
        $swrite(msg, "%s\n DMA MODEL CHANNEL %d", msg, tr_model_meta.data[$clog2(CHANNELS)+24-1 : 24]);
        $swrite(msg, "%s\n DMA MODEL SIZE %d\n", msg, tr_model_meta.data[USR_MFB_META_WIDTH-1 : $clog2(CHANNELS)+24]);
        $swrite(msg, "%s\n DMA DUT META %h", msg, tr_dut_meta.data[23 : 0]);
        $swrite(msg, "%s\n DMA DUT CHANNEL %d", msg, tr_dut_meta.data[$clog2(CHANNELS)+24-1 : 24]);
        $swrite(msg, "%s\n DMA DUT SIZE %d\n", msg, tr_dut_meta.data[USR_MFB_META_WIDTH-1 : $clog2(CHANNELS)+24]);
        $swrite(msg, "%s\n===============================================\n", msg);
        `uvm_error(this.get_full_name(), msg);
    endtask

    task print_data_compare_error(uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH) tr_dut_mfb,
                                  uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH) tr_model_mfb,
                                  uvm_logic_vector::sequence_item       #(USR_MFB_META_WIDTH) tr_model_meta);

        string msg;
        int unsigned             bad_tr_pos[$];

        foreach(tr_dut_mfb.data[it]) begin
            if (it < tr_model_mfb.data.size() && tr_dut_mfb.data[it] != tr_model_mfb.data[it]) begin
                bad_tr_pos.push_back(it);
            end
        end

        $display("WRONG BYTES POS: %p\n", bad_tr_pos);

        $swrite(msg, "%s\n\nDATA COMPARISON CHANNEL %d", msg, tr_model_meta.data[$clog2(CHANNELS)+24-1 : 24]);
        $swrite(msg, "%s\n=============================================================================================================================\n", msg);
        $swrite(msg, "%s\n\t Comparison failed at data number %d! \n\tModel data:\n%s\n\tDUT data:\n%s\n", msg, m_compared, tr_model_mfb.convert2string(), tr_dut_mfb.convert2string());
        $swrite(msg, "%s\n=============================================================================================================================\n", msg);
        `uvm_error(this.get_full_name(), msg);
    endtask

    task run_phase(uvm_phase phase);
        uvm_common::model_item #(uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH)) tr_model_mfb;
        uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH)                                 tr_model_meta;
        uvm_common::model_item #(uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH)) tr_dut_mfb;
        uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH)                                 tr_dut_meta;

        uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH)                           tr_dut_mfb_comp;
        uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH)                           tr_model_mfb_comp;
        uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH)                                 tr_dut_meta_comp;
        uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH)                                 tr_model_meta_comp;

        uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH)                           tr_model_mfb_fifo  [CHANNELS][$];
        uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH)                                 tr_model_meta_fifo [CHANNELS][$];
        uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH)                           tr_dut_mfb_fifo    [CHANNELS][$];
        uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH)                                 tr_dut_meta_fifo   [CHANNELS][$];

        int unsigned                   bad_tr_pos[$];
        logic [$clog2(CHANNELS)-1 : 0] model_channel;
        logic [$clog2(CHANNELS)-1 : 0] dut_channel;
        string                         debug_msg;

        forever begin
            tr_dut_mfb = uvm_common::model_item #(uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH))::type_id::create("tr_dut_mfb");

            m_model_data_analysis_fifo.get(tr_model_mfb);
            m_model_meta_analysis_fifo.get(tr_model_meta);
            m_dut_data_analysis_fifo  .get(tr_dut_mfb.item);
            tr_dut_mfb.time_add("dut mfb out", $time());
            m_dut_meta_analysis_fifo  .get(tr_dut_meta);

            model_channel = tr_model_meta.data[$clog2(CHANNELS)+24-1 : 24];
            dut_channel   = tr_dut_meta.data[$clog2(CHANNELS)+24-1 : 24];

            tr_model_mfb_fifo[int'(model_channel)] .push_back(tr_model_mfb.item);
            tr_model_meta_fifo[int'(model_channel)].push_back(tr_model_meta);
            tr_dut_mfb_fifo[int'(dut_channel)]     .push_back(tr_dut_mfb.item);
            tr_dut_meta_fifo[int'(dut_channel)]    .push_back(tr_dut_meta);

            m_compared++;

            for (int unsigned chan = 0; chan < CHANNELS; chan++) begin

                // Comparing METADATA of the output transaction
                if (tr_model_meta_fifo[chan].size() != 0 && tr_dut_meta_fifo[chan].size() != 0) begin
                    tr_dut_meta_comp   = tr_dut_meta_fifo[chan].pop_front();
                    tr_model_meta_comp = tr_model_meta_fifo[chan].pop_front();

                    if (tr_model_meta_comp.compare(tr_dut_meta_comp) == 0) begin
                        m_errors++;
                        print_meta_compare_error(tr_model_meta_comp, tr_dut_meta_comp);
                    end
                end

                // Comparing DATA of the output transaction
                if (tr_model_mfb_fifo[chan].size() != 0 && tr_dut_mfb_fifo[chan].size() != 0) begin
                    tr_dut_mfb_comp   = tr_dut_mfb_fifo[chan].pop_front();
                    tr_model_mfb_comp = tr_model_mfb_fifo[chan].pop_front();

                    if (tr_model_mfb_comp.compare(tr_dut_mfb_comp) == 0) begin
                        m_errors++;
                        print_data_compare_error(tr_dut_mfb_comp, tr_model_mfb_comp, tr_model_meta_comp);
                    end
                end
            end

            debug_msg = "\n";
            $swrite(debug_msg, "%s================================================================================= \n", debug_msg);
            $swrite(debug_msg, "%SDUT TRANSACTION %0d COMPARED!\n", debug_msg, m_compared);
            $swrite(debug_msg, "%s================================================================================= \n", debug_msg);
            $swrite(debug_msg, "%sCHANNEL : %0d\n", debug_msg, dut_channel);
            $swrite(debug_msg, "%sDATA    : %s\n",  debug_msg, tr_dut_mfb.item.convert2string());
            `uvm_info(this.get_full_name(),         debug_msg, UVM_MEDIUM)

            //count stats
            //Count delay if you get first data packet.
            m_delay.next_val((tr_dut_mfb.start["dut mfb out"] - tr_model_mfb.start["model mfb out"])/1ns);
        end
    endtask
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
    uvm_common::comparer_ordered #(uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH)) m_data_cmp;
    uvm_common::comparer_ordered #(uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH))       m_meta_cmp;
    // compare #(USR_MFB_ITEM_WIDTH, CHANNELS, USR_MFB_META_WIDTH)                                tr_compare;

    uvm_reg_data_t pkt_cnt          [CHANNELS];
    uvm_reg_data_t byte_cnt         [CHANNELS];
    uvm_reg_data_t discard_pkt_cnt  [CHANNELS];
    uvm_reg_data_t discard_byte_cnt [CHANNELS];
    uvm_status_e   status_r;

    local uvm_common::stats                                                                        m_input_speed;
    local uvm_common::stats                                                                        m_delay;
    local uvm_common::stats                                                                        m_output_speed;
    // local uvm_tlm_analysis_fifo #(uvm_common::model_item #(uvm_logic_vector_array::sequence_item #(PCIE_CQ_MFB_ITEM_WIDTH))) rx_speed_meter;
    // local uvm_tlm_analysis_fifo #(uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH))                               tx_speed_meter;

    // Contructor of scoreboard.
    function new(string name, uvm_component parent);
        super.new(name, parent);
        m_usr_data_analysis_export  = new("m_usr_data_analysis_export", this);
        m_usr_meta_analysis_export  = new("m_usr_meta_analysis_export", this);
        m_pkt_drop_analysis_export  = new("m_pkt_drop_analysis_export", this);

        //LOCAL VARIABLES
        // rx_speed_meter = new("rx_speed_meter", this);
        // tx_speed_meter = new("tx_speed_meter", this);
        m_delay        = new();
        // m_output_speed = new();
        // m_input_speed  = new();
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
        m_data_cmp   = uvm_common::comparer_ordered #(uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH))        ::type_id::create("m_data_cmp", this);
        m_meta_cmp   = uvm_common::comparer_ordered #(uvm_logic_vector::sequence_item #(USR_MFB_META_WIDTH))              ::type_id::create("m_meta_cmp", this);
        // tr_compare = compare #(USR_MFB_ITEM_WIDTH, CHANNELS, USR_MFB_META_WIDTH)                                          ::type_id::create("tr_compare", this);

        m_pcie_cq_data_subs = uvm_common::subscriber #(uvm_logic_vector_array::sequence_item #(PCIE_CQ_MFB_ITEM_WIDTH))         ::type_id::create("m_pcie_cq_data_subs",this);
        m_pcie_cq_meta_subs = uvm_common::subscriber #(uvm_logic_vector::sequence_item #(sv_pcie_meta_pack::PCIE_CQ_META_WIDTH))::type_id::create("m_pcie_cq_meta_subs",this);
    endfunction

    function void connect_phase(uvm_phase phase);
        m_pcie_cq_data_subs.port.connect(m_model.m_cq_data_analysis_fifo.analysis_export);
        // m_pcie_cq_data_subs.port.connect(rx_speed_meter.analysis_export);
        m_pcie_cq_meta_subs.port.connect(m_model.m_cq_meta_analysis_fifo.analysis_export);

        m_model.m_usr_data_analysis_port.connect(m_data_cmp.analysis_imp_model);
        m_model.m_usr_meta_analysis_port.connect(m_meta_cmp.analysis_imp_model);

        m_usr_data_analysis_export.connect(m_data_cmp.analysis_imp_dut);
        // m_usr_data_analysis_export.connect(tx_speed_meter.analysis_export);
        m_usr_meta_analysis_export.connect(m_meta_cmp.analysis_imp_dut);
        // tr_compare.m_delay = m_delay;
        m_pkt_drop_analysis_export.connect(m_model.m_discard_comp.m_internal_meta_analysis_fifo.analysis_export);
    endfunction

    // task run_output();
    //     uvm_logic_vector_array::sequence_item #(USR_MFB_ITEM_WIDTH) tr_dut;

    //     int unsigned speed_packet_size = 0;
    //     time         speed_start_time  = 0ns;

    //     forever begin
    //         time time_act;
    //         time speed_metet_duration;

    //         tx_speed_meter.get(tr_dut);
    //         time_act = $time();

    //         speed_packet_size += tr_dut.data.size();
    //         speed_metet_duration = time_act - speed_start_time;
    //         if (speed_metet_duration >= 10us) begin
    //             real speed;
    //             speed =  real'(speed_packet_size) / (speed_metet_duration/1ns); //result is in GB/s
    //             m_output_speed.next_val(speed);
    //             speed_start_time  = time_act;
    //             speed_packet_size = 0;
    //             `uvm_info(this.get_full_name(), $sformatf("\n\tCurrent output speed (PCIE TX) is %0.3fGb/s in time [%0d:%0d]us", speed*8, speed_start_time/1us, time_act/1us), UVM_LOW);
    //         end
    //     end
    // endtask

    // task run_input();
    //     int unsigned speed_packet_size = 0;
    //     time         speed_start_time  = 0ns;

    //     forever begin
    //         uvm_common::model_item #(uvm_logic_vector_array::sequence_item #(PCIE_CQ_MFB_ITEM_WIDTH)) tr;
    //         time time_act;
    //         time speed_metet_duration;
    //         rx_speed_meter.get(tr);
    //         time_act = $time();

    //         speed_packet_size += tr.item.data.size();
    //         speed_metet_duration = time_act - speed_start_time;
    //         if (speed_metet_duration >= 10us) begin
    //             real speed;
    //             speed =  real'(speed_packet_size) / (speed_metet_duration/1ns); //result is in GB/s
    //             m_input_speed.next_val(speed);
    //             speed_start_time  = time_act;
    //             speed_packet_size = 0;
    //             `uvm_info(this.get_full_name(), $sformatf("\n\tCurrent input speed (MFB RX) is %0.3fGb/s in time [%0d:%0d]us", speed*8, speed_start_time/1us, time_act/1us), UVM_LOW);
    //         end
    //     end
    // endtask

    // task run_phase(uvm_phase phase);
    //     fork
    //         run_output();
    //         run_input();
    //     join_none
    // endtask

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

            // $swrite(msg, "%s================================================================================= \n", msg);
            // $swrite(msg, "%s\nEXPORT USED                        \n", msg                                             );
            // $swrite(msg, "%s================================================================================= \n", msg);
            // $swrite(msg, "%sMODEL_DATA_FIFO.USED  %d\n", msg, tr_compare.m_model_data_analysis_fifo.used()            );
            // $swrite(msg, "%sMODEL_META_FIFO.USED  %d\n", msg, tr_compare.m_model_meta_analysis_fifo.used()            );
            // $swrite(msg, "%sDUT_DATA_FIFO.USED    %d\n", msg, tr_compare.m_dut_data_analysis_fifo.used()              );
            // $swrite(msg, "%sDUT_META_FIFO.USED    %d\n", msg, tr_compare.m_dut_meta_analysis_fifo.used()              );
            // $swrite(msg, "%s================================================================================= \n", msg);

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
