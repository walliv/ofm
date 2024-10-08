# Modules.tcl: Components include script
# Copyright (C) 2020 CESNET z. s. p. o.
# Author(s): Jan Kubalek <kubalek@cesnet.cz>
#
# SPDX-License-Identifier: BSD-3-Clause

set SV_MVB_BASE   "$OFM_PATH/comp/mvb_tools/ver"
set SV_MFB_BASE   "$OFM_PATH/comp/mfb_tools/ver"
set SV_MI_BASE    "$OFM_PATH/comp/mi_tools/ver"

set COMPONENTS [list \
    [ list "SV_MVB"   $SV_MVB_BASE  "FULL"] \
    [ list "SV_MFB"   $SV_MFB_BASE  "FULL"] \
    [ list "SV_MI"    $SV_MI_BASE   "FULL"] \
]
set MOD "$MOD $ENTITY_BASE/tbench/test_pkg.sv"
set MOD "$MOD $ENTITY_BASE/tbench/dut.sv"
set MOD "$MOD $ENTITY_BASE/tbench/test.sv"
