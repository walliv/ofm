//-- env.sv: Verification environment
//-- Copyright (C) 2022 CESNET z. s. p. o.
//-- Author:   Daniel Kříž <xkrizd01@vutbr.cz>

//-- SPDX-License-Identifier: BSD-3-Clause

// Environment for functional verification of encode.
// This environment containts two mii agents.
class env #(ITEMS, LUT_WIDTH, REG_DEPTH) extends uvm_env;

    `uvm_component_param_utils(uvm_pipe::env #(ITEMS, LUT_WIDTH, REG_DEPTH));

    uvm_logic_vector_mvb::env_rx #(ITEMS, REG_DEPTH) rx_env;
    uvm_logic_vector_mvb::config_item                 cfg_rx;
    uvm_logic_vector_mvb::env_tx #(ITEMS, LUT_WIDTH) tx_env;
    uvm_logic_vector_mvb::config_item                 cfg_tx;

    uvm_pipe::virt_sequencer#(ITEMS, LUT_WIDTH, REG_DEPTH) vscr;
    uvm_reset::agent         m_reset;
    uvm_reset::config_item   m_config_reset;
    uvm_mi::regmodel_config  m_mi_config;

    scoreboard #(LUT_WIDTH, REG_DEPTH) m_scoreboard;

    uvm_mvb::coverage #(ITEMS, REG_DEPTH) m_cover_rx;
    uvm_mvb::coverage #(ITEMS, LUT_WIDTH) m_cover_tx;

    uvm_mi::regmodel#(regmodel#(REG_DEPTH), LUT_WIDTH, REG_DEPTH) m_regmodel;

    // Constructor of environment.
    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction

    // Create base components of environment.
    function void build_phase(uvm_phase phase);

        m_cover_rx = new("m_cover_rx");
        m_cover_tx = new("m_cover_tx");
        cfg_tx = new;
        cfg_rx = new;

        cfg_tx.active = UVM_ACTIVE;
        cfg_rx.active = UVM_ACTIVE;

        cfg_tx.interface_name = "vif_tx";
        cfg_rx.interface_name = "vif_rx";

        cfg_rx.seq_cfg = new();
        cfg_rx.seq_cfg.space_size_set(0, 5);

        m_config_reset                = new;
        m_config_reset.active         = UVM_ACTIVE;
        m_config_reset.interface_name = "vif_reset";

        m_mi_config                      = new();
        m_mi_config.addr_base            = 'h0;
        m_mi_config.agent.active         = UVM_ACTIVE;
        m_mi_config.agent.interface_name = "vif_mi";
        uvm_config_db#(uvm_mi::regmodel_config)::set(this, "m_regmodel", "m_config", m_mi_config);
        m_regmodel = uvm_mi::regmodel#(regmodel#(REG_DEPTH), LUT_WIDTH, REG_DEPTH)::type_id::create("m_regmodel", this);

        uvm_config_db #(uvm_reset::config_item)::set(this, "m_reset", "m_config", m_config_reset);
        m_reset = uvm_reset::agent::type_id::create("m_reset", this);

        uvm_config_db #(uvm_logic_vector_mvb::config_item)::set(this, "tx_env", "m_config", cfg_tx);
        uvm_config_db #(uvm_logic_vector_mvb::config_item)::set(this, "rx_env", "m_config", cfg_rx);

        tx_env    = uvm_logic_vector_mvb::env_tx #(ITEMS, LUT_WIDTH)::type_id::create("tx_env", this);
        rx_env    = uvm_logic_vector_mvb::env_rx #(ITEMS, REG_DEPTH)::type_id::create("rx_env", this);

        m_scoreboard  = scoreboard #(LUT_WIDTH, REG_DEPTH)::type_id::create("m_scoreboard", this);
        vscr   = uvm_pipe::virt_sequencer#(ITEMS, LUT_WIDTH, REG_DEPTH)::type_id::create("vscr",this);
    endfunction

    // Connect agent's ports with ports from scoreboard.
    function void connect_phase(uvm_phase phase);

        rx_env.analysis_port.connect(m_scoreboard.analysis_imp_mvb_rx);
        tx_env.analysis_port.connect(m_scoreboard.analysis_imp_mvb_tx);

        m_reset.sync_connect(rx_env.reset_sync);
        m_reset.sync_connect(tx_env.reset_sync);
        vscr.m_regmodel = m_regmodel.m_regmodel;

        rx_env.m_mvb_agent.analysis_port.connect(m_cover_rx.analysis_export);
        tx_env.m_mvb_agent.analysis_port.connect(m_cover_tx.analysis_export);
        m_scoreboard.regmodel_set(m_regmodel.m_regmodel);

        vscr.m_reset            = m_reset.m_sequencer;
        vscr.m_logic_vector_scr = rx_env.m_sequencer;
        vscr.m_tx_sqr           = tx_env.m_sequencer;

    endfunction
endclass
