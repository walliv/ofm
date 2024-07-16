/*
 * file       : comparer_base_ordered.sv
 * Copyright (C) 2023 CESNET z. s. p. o.
 * description: this component compares two transactions out of order. IF component stays
 *              too long in fifo then errors are going to occur.
 * date       : 2023
 * author     : Radek Iša <isa@cesnet.cz>
 *
 * SPDX-License-Identifier: BSD-3-Clause
*/

virtual class comparer_base_disordered #(type MODEL_ITEM, type DUT_ITEM = MODEL_ITEM) extends comparer_base#(MODEL_ITEM, DUT_ITEM);

    int unsigned dut_sends;
    model_item#(MODEL_ITEM) model_items[$];
    dut_item#(DUT_ITEM) dut_items[$];
    int unsigned compared;
    int unsigned errors;

    function new(string name, uvm_component parent = null);
        super.new(name, parent);
        dut_sends = 0;
        compared  = 0;
        errors    = 0;
    endfunction

    virtual function int unsigned success();
        return (errors == 0);
    endfunction

    virtual function void flush();
        model_items.delete();
        dut_items.delete();
    endfunction

    virtual function int unsigned used();
        return (model_items.size() != 0) | (dut_items.size() != 0);
    endfunction

    virtual function void write_model(model_item#(MODEL_ITEM) tr);
        int unsigned w_end = 0;
        int unsigned it    = 0;
        //try get item from DUT

        `uvm_info(this.get_full_name(), $sformatf("\n\tReceived transactions from Model\n%s", tr.convert2string()), UVM_FULL);
        while (it < dut_items.size() && w_end == 0) begin
            w_end = compare(tr, dut_items[it]);
            if (w_end == 0) begin
                it++;
            end else begin
                compared++;
                dut_items.delete(it);
            end
        end

        if (w_end == 0) begin
            model_items.push_back(tr);
        end
    endfunction

    virtual function void write_dut(DUT_ITEM tr);
        int unsigned w_end = 0;
        int unsigned it    = 0;
        dut_item#(DUT_ITEM) tmp_tr = new(dut_sends, $time(), tr);

        dut_sends += 1;
        //try get item from DUT
        `uvm_info(this.get_full_name(), $sformatf("\n\tReceived transactions from DUT\n%s", tr.convert2string()), UVM_FULL);
        while (it < model_items.size() && w_end == 0) begin
            w_end = compare(model_items[it], tmp_tr);
            if (w_end == 0) begin
                it++;
            end else begin
                compared++;
                model_items.delete(it);
            end
        end

        if (w_end == 0) begin
            dut_items.push_back(tmp_tr);
        end
    endfunction

    function string dut_tr_get(model_item#(MODEL_ITEM) tr, time tr_time);
        string msg = "";
        for (int unsigned it = 0; it < dut_items.size(); it++) begin
            msg = {msg, $sformatf("\n\nOutput time %0dns (%0dns) \n%s", dut_items[it].in_time/1ns, (dut_items[it].in_time - tr_time)/1ns, this.message(tr, dut_items[it]))};
        end
        return msg;
    endfunction

    function string model_tr_get(dut_item #(DUT_ITEM) tr);
        string msg = "";
        for (int unsigned it = 0; it < model_items.size(); it++) begin
            msg = {msg, $sformatf("\n\n%s\n%s", model_items[it].convert2string_time(), this.message(model_items[it], tr))};
        end
        return msg;
    endfunction

    task run_model_delay_check();
        time delay;
        forever begin
            wait(model_items.size() > 0);
            delay = $time() - model_items[0].time_last();
            if (delay >= dut_tr_timeout) begin
                errors++;
               `uvm_error(this.get_full_name(), $sformatf("\n\tTransaction from DUT is delayed %0dns. Probably stuck.\n\tErrors/Compared %0d/%0d\n%s\n\nDUT transactions:\n%s",
                                                         delay/1ns, errors, compared, model_items[0].convert2string(),
                                                         this.dut_tr_get(model_items[0], model_items[0].time_last())));
                model_items.delete(0);
            end else begin
                #(dut_tr_timeout - delay);
            end
        end
    endtask

    task run_dut_delay_check();
        time delay;
        forever begin
            wait(dut_items.size() > 0);
            delay = $time() - dut_items[0].in_time;
            if (delay >= model_tr_timeout) begin
                errors++;
                `uvm_error(this.get_full_name(), $sformatf("\n\tTransaction %0d from DUT is unexpected.\n\tErrors/Compared %0d/%0d Output time %0dns. Delay %0dns. Probably unexpected transaction.\n%s\n\n%s",
                                                           dut_items[0].in_id, errors, compared, dut_items[0].in_time/1ns, delay/1ns,
                                                           dut_items[0].in_item.convert2string(), this.model_tr_get(dut_items[0])));
                dut_items.delete(0);
            end else begin
                #(model_tr_timeout - delay);
            end
        end
    endtask

    virtual function string info(logic data = 0);
        string msg ="";
        msg = $sformatf("\n\tErrors %0d Compared %0d Wait for tramsaction DUT(%0d) MODEL(%0d)", errors, compared, dut_items.size(), model_items.size());
        if (data == 1) begin
            for (int unsigned it = 0; it < model_items.size(); it++) begin
                msg = {msg, $sformatf("\n\nModels transaction : %0d", it) , model_items[it].convert2string()};
            end
            msg = {msg, "\n\n"};
            for (int unsigned it = 0; it < dut_items.size(); it++) begin
                msg = {msg, $sformatf("\n\nDUT transactions : %0d", it), dut_items[it].convert2string()};
            end
        end
        return msg;
    endfunction
endclass


