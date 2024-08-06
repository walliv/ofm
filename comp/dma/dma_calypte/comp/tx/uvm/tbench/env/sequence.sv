// sequence.sv: Virtual sequence
// Copyright (C) 2024 CESNET z. s. p. o.
// Author(s): Radek Iša <isa@cesnet.cz>
//            Vladislav Válek <valekv@cesnet.cz>

// SPDX-License-Identifier: BSD-3-Clause

class sequence_simple extends uvm_sequence#(uvm_tx_dma_calypte_cq::sequence_item);
    `uvm_object_param_utils(uvm_tx_dma_calypte::sequence_simple)
    `uvm_declare_p_sequencer(uvm_tx_dma_calypte_cq::sequencer)

    int unsigned m_packet_size_min = 60;
    int unsigned m_packet_size_max = 2048;

    function new(string name = "uvm_tx_dma_calypte::sequence_simple");
        super.new(name);
    endfunction

    task send_packets(uvm_common::sequence_cfg m_state, int unsigned min, int unsigned max);
        int unsigned it;
        int unsigned transaction_count;

        std::randomize(transaction_count) with {
            transaction_count dist {[1:100] :/ 10, [100:1000] :/ 20, [1000:5000] :/ 60, [5000:50000] :/ 10};
            transaction_count >= min;
            transaction_count <= max;
        };

        //RUN DATA
        it = 0;
        while(it < transaction_count && (m_state == null || m_state.next())) begin
            start_item(req);
            assert(req.randomize() with {req.m_packet.size() inside {[m_packet_size_min:m_packet_size_max-1]};}) else `uvm_fatal(m_sequencer.get_full_name(), "\n\tCannot randomize packet");
            finish_item(req);

            it++;
        end
    endtask

    task body();
        uvm_common::sequence_cfg                   m_state;
        uvm_tx_dma_calypte_regs::start_channel_seq m_start_chan_seq;
        uvm_tx_dma_calypte_regs::stop_channel_seq  m_stop_chan_seq;

        m_start_chan_seq = uvm_tx_dma_calypte_regs::start_channel_seq::type_id::create("m_start_chan_seq", m_sequencer);
        m_stop_chan_seq  = uvm_tx_dma_calypte_regs::stop_channel_seq ::type_id::create("m_stop_chan_seq",  m_sequencer);
        m_start_chan_seq.m_regmodel_channel = p_sequencer.m_regmodel_channel;
        m_stop_chan_seq.m_regmodel_channel  = p_sequencer.m_regmodel_channel;

        req = uvm_tx_dma_calypte_cq::sequence_item::type_id::create("req", m_sequencer);

        if(!uvm_config_db#(uvm_common::sequence_cfg)::get(m_sequencer, "", "state", m_state)) begin
            m_state = null;
        end

        m_start_chan_seq.start(null);

        while(m_state == null || !m_state.stopped()) begin
            int unsigned idle_time;
            int unsigned time_till_stop;
            string       msg = "";

            // Time for which the channel is turned off
            std::randomize(idle_time)      with {idle_time      dist {[1: 10] :/ 10, [10:100] :/ 40, [1000:5000] :/ 50};};
            //Time until the next stop of a channel
            std::randomize(time_till_stop) with {time_till_stop dist {[1: 10] :/ 10, [10:100] :/ 40, [1000:5000] :/ 50};};

            $swrite(msg, "\n%s\t Time until stop: %0d us\n", msg, time_till_stop*0.1);
            $swrite(msg,   "%s\t Idle time:       %0d us\n", msg, idle_time*0.1);
            `uvm_info(this.get_full_name(), msg, UVM_LOW);

            fork
                send_packets(m_state, 0, 100000);

                begin
                    #(time_till_stop*100ns);
                    m_stop_chan_seq.start(null);
                    #(idle_time*100ns)
                    m_start_chan_seq.start(null);
                end
            join

        end
    endtask
endclass
