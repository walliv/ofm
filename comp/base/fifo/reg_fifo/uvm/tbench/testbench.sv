// tbench.sv: Testbench
// Copyright (C) 2024 CESNET z. s. p. o.
// Author(s): Vladislav Valek <valekv@cesnet.cz>

// SPDX-License-Identifier: BSD-3-Clause

import uvm_pkg::*;
`include "uvm_macros.svh"
import test::*;

module testbench;

    // -------------------------------------------------------------------------------------------------------------------------------------------------------------------
    // Signals
    logic CLK = 0;

    // -------------------------------------------------------------------------------------------------------------------------------------------------------------------
    // Interfaces
    reset_if reset (CLK);

    mvb_if #(1, DATA_WIDTH) mvb_rx(CLK);
    mvb_if #(1, DATA_WIDTH) mvb_tx(CLK);

    always #(CLK_PERIOD) CLK = ~CLK;

    // -------------------------------------------------------------------------------------------------------------------------------------------------------------------
    // Start of tests
    initial begin
        uvm_root m_root;

        // Configuration of database
        uvm_config_db #(virtual reset_if)::set(null, "", "vif_reset", reset);

        uvm_config_db #(virtual mvb_if #(1, DATA_WIDTH))::set(null, "", "vif_mvb_rx", mvb_rx);
        uvm_config_db #(virtual mvb_if #(1, DATA_WIDTH))::set(null, "", "vif_mvb_tx", mvb_tx);

        m_root = uvm_root::get();
        m_root.finish_on_completion = 0;
        m_root.set_report_id_action_hier("ILLEGALNAME", UVM_NO_ACTION);

        uvm_config_db #(int)            ::set(null, "", "recording_detail", 0);
        uvm_config_db #(uvm_bitstream_t)::set(null, "", "recording_detail", 0);

        run_test();
        $stop(2);
    end

    DUT DUT_U (
        .CLK (CLK),
        .RST (reset.RESET),

        .mvb_rx     (mvb_rx),
        .mvb_tx     (mvb_tx)
    );

    reg_fifo_property #(
        .DATA_WIDTH   (DATA_WIDTH)
    )
    PROPERTY_CHECK (
        .RESET (reset.RESET),

        .mvb_rx     (mvb_rx),
        .mvb_tx     (mvb_tx)
    );
endmodule
