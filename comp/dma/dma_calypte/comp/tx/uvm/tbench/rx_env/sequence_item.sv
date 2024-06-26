//-- sequence.sv
//-- Copyright (C) 2024 CESNET z. s. p. o.
//-- Author(s): Radek Iša <isa@cesnet.cz>

//-- SPDX-License-Identifier: BSD-3-Clause

// This low level sequence define bus functionality


class sequence_item extends uvm_sequence_item;
    `uvm_object_param_utils(uvm_dma_ll_rx::sequence_item)

    localparam ITEM_WIDTH = 8;
    localparam META_WIDTH = 24;

    rand logic [ITEM_WIDTH-1:0] packet[];
    rand logic [META_WIDTH-1:0] meta;

    function new(string name = "dma_ll_rx::sequence_item");
        super.new(name);
    endfunction


    // Properly copy all transaction attributes.
    function void do_copy(uvm_object rhs);
        sequence_item rhs_;

        if(!$cast(rhs_, rhs)) begin
            `uvm_fatal( "do_copy:", "Failed to cast transaction object.")
            return;
        end
        // Now copy all attributes
        super.do_copy(rhs);
        packet   = rhs_.packet;
        meta = meta;
    endfunction: do_copy

    // Properly compare all transaction attributes representing output pins.
    function bit do_compare(uvm_object rhs, uvm_comparer comparer);
        sequence_item rhs_;
        bit ret;

        if(!$cast(rhs_, rhs)) begin
            `uvm_fatal("do_compare:", "Failed to cast transaction object.")
            return 0;
        end

        ret = super.do_compare(rhs, comparer);
        ret = (packet === rhs_.packet);
        ret = (meta  === meta);
        // Using simple equivalence operator (faster).
        return ret;
    endfunction: do_compare

    // Convert transaction into human readable form.
    function string convert2string();
        return convert2block(8);
    endfunction

    function string convert2block(int unsigned region_width);
        string ret;

        ret = $sformatf("%s\n\tdma_rx_ll_rx::sequence_item meta %h size %0d", super.convert2string(), meta, packet.size());
        for (int unsigned it = 0; it < packet.size(); it++) begin
            if (it % (region_width*4) == 0) begin
                ret = {ret, $sformatf("\n\t\t%x", packet[it])};
            end else if (it % (region_width) == 0) begin
                ret = {ret, $sformatf("    %x", packet[it])};
            end else begin
                ret = {ret, $sformatf(" %x", packet[it])};
            end
        end
        return ret;
    endfunction

    function int unsigned size();
        return packet.size();
    endfunction
endclass

