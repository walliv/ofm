// test.sv: Verification test
// Copyright (C) 2022 CESNET z. s. p. o.
// Author(s): Daniel Kříž <xkrizd01@vutbr.cz>

// SPDX-License-Identifier: BSD-3-Clause

class virt_seq_full_speed#(MIN_SIZE, PKT_MTU, DATA_SIZE_MAX, MFB_REGIONS, MFB_REGION_SIZE, MFB_BLOCK_SIZE, MFB_ITEM_WIDTH, HEADER_SIZE, MVB_ITEM_WIDTH) extends virt_sequence #(MIN_SIZE, PKT_MTU, DATA_SIZE_MAX, MFB_REGIONS, MFB_REGION_SIZE, MFB_BLOCK_SIZE, MFB_ITEM_WIDTH, HEADER_SIZE, MVB_ITEM_WIDTH);
    `uvm_object_param_utils(test::virt_seq_full_speed#(MIN_SIZE, PKT_MTU, DATA_SIZE_MAX, MFB_REGIONS, MFB_REGION_SIZE, MFB_BLOCK_SIZE, MFB_ITEM_WIDTH, HEADER_SIZE, MVB_ITEM_WIDTH))
    `uvm_declare_p_sequencer(uvm_superunpacketer::virt_sequencer#(MFB_REGIONS, MFB_REGION_SIZE, MFB_BLOCK_SIZE, MFB_ITEM_WIDTH, HEADER_SIZE, MVB_ITEM_WIDTH, HEADER_SIZE))

    function new (string name = "virt_seq_full_speed");
        super.new(name);
    endfunction

    virtual function void init(uvm_phase phase);
        super.init(phase);
        m_mfb_seq = uvm_mfb::sequence_full_speed_tx #(MFB_REGIONS, MFB_REGION_SIZE, MFB_BLOCK_SIZE, MFB_ITEM_WIDTH, HEADER_SIZE+MVB_ITEM_WIDTH)::type_id::create("m_mfb_seq");
        m_mvb_seq = uvm_mvb::sequence_full_speed_tx #(MFB_REGIONS, HEADER_SIZE+MVB_ITEM_WIDTH)::type_id::create("m_mvb_seq");
    endfunction
endclass

class mfb_rx_speed#(MFB_REGIONS, MFB_REGION_SIZE, MFB_ITEM_WIDTH, MFB_BLOCK_SIZE, META_WIDTH) extends uvm_logic_vector_array_mfb::sequence_lib_rx#(MFB_REGIONS, MFB_REGION_SIZE, MFB_ITEM_WIDTH, MFB_BLOCK_SIZE, 0);
  `uvm_object_param_utils(test::mfb_rx_speed#(MFB_REGIONS, MFB_REGION_SIZE, MFB_ITEM_WIDTH, MFB_BLOCK_SIZE, META_WIDTH))
  `uvm_sequence_library_utils(test::mfb_rx_speed#(MFB_REGIONS, MFB_REGION_SIZE, MFB_ITEM_WIDTH, MFB_BLOCK_SIZE, META_WIDTH))

    function new(string name = "mfb_rx_speed");
        super.new(name);
        init_sequence_library();
    endfunction

    virtual function void init_sequence(uvm_logic_vector_array_mfb::config_sequence param_cfg = null);
        if (param_cfg == null) begin
            this.cfg = new();
        end else begin
            this.cfg = param_cfg;
        end
        this.add_sequence(uvm_logic_vector_array_mfb::sequence_full_speed_rx #(MFB_REGIONS, MFB_REGION_SIZE, MFB_ITEM_WIDTH, MFB_BLOCK_SIZE, META_WIDTH)::get_type());
    endfunction
endclass

class speed extends uvm_test;
     typedef uvm_component_registry#(test::speed, "test::speed") type_id;

    // declare the Environment reference variable
    uvm_superunpacketer::env #(MFB_REGIONS, MFB_REGION_SIZE, MFB_BLOCK_SIZE, MFB_ITEM_WIDTH, HEADER_SIZE, MVB_ITEM_WIDTH, VERBOSITY, PKT_MTU, MIN_SIZE, META_OUT_MODE, UNPACKING_STAGES) m_env;
    int unsigned timeout;

    // ------------------------------------------------------------------------
    // Functions
    // Constrctor of the test object
    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction

    static function type_id get_type();
        return type_id::get();
    endfunction

    function string get_type_name();
        return get_type().get_type_name();
    endfunction

    // Build phase function, e.g. the creation of test's internal objects
    function void build_phase(uvm_phase phase);
        uvm_logic_vector_array_mfb::sequence_lib_rx#(MFB_REGIONS, MFB_REGION_SIZE, MFB_BLOCK_SIZE, MFB_ITEM_WIDTH, 0)::type_id::set_inst_override(mfb_rx_speed#(MFB_REGIONS, MFB_REGION_SIZE, MFB_BLOCK_SIZE, MFB_ITEM_WIDTH, 0)::get_type(),
        {this.get_full_name(), ".m_env.m_env_rx.*"});

        // Initializing the reference to the environment
        m_env = uvm_superunpacketer::env #(MFB_REGIONS, MFB_REGION_SIZE, MFB_BLOCK_SIZE, MFB_ITEM_WIDTH, HEADER_SIZE, MVB_ITEM_WIDTH, VERBOSITY, PKT_MTU, MIN_SIZE, META_OUT_MODE, UNPACKING_STAGES)::type_id::create("m_env", this);
    endfunction

    // ------------------------------------------------------------------------
    // Create environment and Run sequences on their sequencers
    virtual task run_phase(uvm_phase phase);
        virt_seq_full_speed #(MIN_SIZE, PKT_MTU, DATA_SIZE_MAX, MFB_REGIONS, MFB_REGION_SIZE, MFB_BLOCK_SIZE, MFB_ITEM_WIDTH, HEADER_SIZE, MVB_ITEM_WIDTH) m_vseq;
        m_vseq = virt_seq_full_speed #(MIN_SIZE, PKT_MTU, DATA_SIZE_MAX, MFB_REGIONS, MFB_REGION_SIZE, MFB_BLOCK_SIZE, MFB_ITEM_WIDTH, HEADER_SIZE, MVB_ITEM_WIDTH)::type_id::create("m_vseq");

        phase.raise_objection(this);

        m_vseq.init(phase);

        //RUN MFB RX SEQUENCE
        m_vseq.randomize();
        m_vseq.start(m_env.vscr);

        timeout = 1;
        fork
            test_wait_timeout(1000);
            test_wait_result();
        join_any;

        phase.drop_objection(this);

    endtask

    task test_wait_timeout(int unsigned time_length);
        #(time_length*1us);
    endtask

    task test_wait_result();
        do begin
            #(600ns);
        end while (m_env.sc.used() != 0);
        timeout = 0;
    endtask

    function void report_phase(uvm_phase phase);
        `uvm_info(this.get_full_name(), {"\n\tTEST : ", this.get_type_name(), " END\n"}, UVM_NONE);
        if (timeout) begin
            `uvm_error(this.get_full_name(), "\n\t===================================================\n\tTIMEOUT SOME PACKET STUCK IN DESIGN\n\t===================================================\n\n");
        end
    endfunction
endclass
