# ver_settings.py
# Copyright (C) 2022 CESNET z. s. p. o.
# Author(s): Daniel Kříž <xkrizd01@vutbr.cz>

SETTINGS = {
    "default" : { # The default setting of verification
        "DMA_PORTS"            : "2"                    ,
        "MVB_UP_ITEMS"         : "2"                    ,
        "DMA_MFB_UP_REGIONS"   : "MFB_UP_REGIONS"       ,
        "MFB_UP_REGIONS"       : "2"                    ,
        "MFB_UP_REG_SIZE"      : "1"                    ,
        "MFB_UP_BLOCK_SIZE"    : "8"                    ,
        "MFB_UP_ITEM_WIDTH"    : "32"                   ,
        "MVB_DOWN_ITEMS"       : "2"                    ,
        "DMA_MFB_DOWN_REGIONS" : "MFB_DOWN_REGIONS"     ,
        "MFB_DOWN_REGIONS"     : "2"                    ,
        "MFB_DOWN_REG_SIZE"    : "1"                    ,
        "MFB_DOWN_BLOCK_SIZE"  : "8"                    ,
        "MFB_DOWN_ITEM_WIDTH"  : "32"                   ,
        "RQ_TUSER_WIDTH"       : "137"                  ,
        "RC_TUSER_WIDTH"       : "161"                  ,
        "ENDPOINT_TYPE"        : "\\\"P_TILE\\\""       ,
        "DEVICE"               : "\\\"STRATIX10\\\""    ,
        "CLK_PERIOD"           : "2.22222222ns"         ,
        "CLK_DMA_PERIOD"       : "5ns"                  ,
        "AUTO_ASSIGN_TAGS"     : "1"                    ,
        "RCB_SIZE"             : "1'b0"                 ,
        "TR_MIN"               : "100"                  ,
        "TR_MAX"               : "300"                  ,
        "ONLY_READ"            : "0"                    ,
        "TEST_NAME"            : "\\\"test::ex_test\\\"",
    },
    "s10_256_port" : {
        "DMA_PORTS"            : "1"                 ,
        "MVB_UP_ITEMS"         : "1"                 ,
        "MFB_UP_REGIONS"       : "1"                 ,
        "MFB_UP_REG_SIZE"      : "1"                 ,
        "MFB_UP_BLOCK_SIZE"    : "8"                 ,
        "MFB_UP_ITEM_WIDTH"    : "32"                ,
        "MVB_DOWN_ITEMS"       : "1"                 ,
        "MFB_DOWN_REGIONS"     : "1"                 ,
        "MFB_DOWN_REG_SIZE"    : "1"                 ,
        "MFB_DOWN_BLOCK_SIZE"  : "8"                 ,
        "MFB_DOWN_ITEM_WIDTH"  : "32"                ,
        "ENDPOINT_TYPE"        : "\\\"P_TILE\\\""    ,
        "DEVICE"               : "\\\"STRATIX10\\\"" ,
    },
    "agilex_1_port_300mhz" : {
        "DMA_PORTS"            : "1"                 ,
        "MVB_UP_ITEMS"         : "2"                 ,
        "MFB_UP_REGIONS"       : "2"                 ,
        "MFB_UP_REG_SIZE"      : "1"                 ,
        "MFB_UP_BLOCK_SIZE"    : "8"                 ,
        "MFB_UP_ITEM_WIDTH"    : "32"                ,
        "MVB_DOWN_ITEMS"       : "2"                 ,
        "MFB_DOWN_REGIONS"     : "2"                 ,
        "MFB_DOWN_REG_SIZE"    : "1"                 ,
        "MFB_DOWN_BLOCK_SIZE"  : "8"                 ,
        "MFB_DOWN_ITEM_WIDTH"  : "32"                ,
        "ENDPOINT_TYPE"        : "\\\"R_TILE\\\""    ,
        "DEVICE"               : "\\\"AGILEX\\\""    ,
        "CLK_PERIOD"           : "3.33333333ns"      ,
    },
    "agilex_1_port_500mhz" : {
        "DMA_PORTS"            : "1"                 ,
        "MVB_UP_ITEMS"         : "2"                 ,
        "MFB_UP_REGIONS"       : "2"                 ,
        "MFB_UP_REG_SIZE"      : "1"                 ,
        "MFB_UP_BLOCK_SIZE"    : "8"                 ,
        "MFB_UP_ITEM_WIDTH"    : "32"                ,
        "MVB_DOWN_ITEMS"       : "2"                 ,
        "MFB_DOWN_REGIONS"     : "2"                 ,
        "MFB_DOWN_REG_SIZE"    : "1"                 ,
        "MFB_DOWN_BLOCK_SIZE"  : "8"                 ,
        "MFB_DOWN_ITEM_WIDTH"  : "32"                ,
        "ENDPOINT_TYPE"        : "\\\"R_TILE\\\""    ,
        "DEVICE"               : "\\\"AGILEX\\\""    ,
        "CLK_PERIOD"           : "2ns"               ,
    },
    "agilex_4p_gen5x16" : {
        "DMA_PORTS"            : "4"                 ,
        "MVB_UP_ITEMS"         : "4"                 ,
        "MFB_UP_REGIONS"       : "4"                 ,
        "MFB_UP_REG_SIZE"      : "1"                 ,
        "MFB_UP_BLOCK_SIZE"    : "8"                 ,
        "MFB_UP_ITEM_WIDTH"    : "32"                ,
        "DMA_MFB_UP_REGIONS"   : "2"                 ,
        "MVB_DOWN_ITEMS"       : "4"                 ,
        "MFB_DOWN_REGIONS"     : "4"                 ,
        "MFB_DOWN_REG_SIZE"    : "1"                 ,
        "MFB_DOWN_BLOCK_SIZE"  : "8"                 ,
        "MFB_DOWN_ITEM_WIDTH"  : "32"                ,
        "DMA_MFB_DOWN_REGIONS" : "2"                 ,
        "ENDPOINT_TYPE"        : "\\\"R_TILE\\\""    ,
        "DEVICE"               : "\\\"AGILEX\\\""    ,
        "CLK_PERIOD"           : "2ns"               ,
    },
    "ultrascale_1_port" : { # Ultrascale+ default settings
        "CLK_PERIOD"           : "4ns"               ,
        "CLK_DMA_PERIOD"       : "3ns"               ,

        "DMA_PORTS"            : "1"                 ,
        "MVB_UP_ITEMS"         : "2"                 ,
        "MFB_UP_REGIONS"       : "2"                 ,
        "MFB_UP_REG_SIZE"      : "1"                 ,
        "MFB_UP_BLOCK_SIZE"    : "8"                 ,
        "MFB_UP_ITEM_WIDTH"    : "32"                ,
        "DMA_MFB_UP_REGIONS"   : "MFB_UP_REGIONS"    ,
        "MVB_DOWN_ITEMS"       : "4"                 ,
        "MFB_DOWN_REGIONS"     : "4"                 ,
        "MFB_DOWN_REG_SIZE"    : "1"                 ,
        "MFB_DOWN_BLOCK_SIZE"  : "4"                 ,
        "MFB_DOWN_ITEM_WIDTH"  : "32"                ,
        "DMA_MFB_DOWN_REGIONS" : "MFB_DOWN_REGIONS"  ,
        "RQ_TUSER_WIDTH"       : "137"               ,
        "RC_TUSER_WIDTH"       : "161"               ,
        "ENDPOINT_TYPE"        : "\\\"DUMMY\\\""     ,
        "DEVICE"               : "\\\"ULTRASCALE\\\"",
    },
    "virtex7_1_port" : { # Virtex7
        "DMA_PORTS"            : "1"                 ,
        "MVB_UP_ITEMS"         : "1"                 ,
        "MFB_UP_REGIONS"       : "1"                 ,
        "MFB_UP_REG_SIZE"      : "1"                 ,
        "MFB_UP_BLOCK_SIZE"    : "8"                 ,
        "MFB_UP_ITEM_WIDTH"    : "32"                ,
        "DMA_MFB_UP_REGIONS"   : "MFB_UP_REGIONS*2"  ,
        "MVB_DOWN_ITEMS"       : "2"                 ,
        "MFB_DOWN_REGIONS"     : "2"                 ,
        "MFB_DOWN_REG_SIZE"    : "1"                 ,
        "MFB_DOWN_BLOCK_SIZE"  : "4"                 ,
        "MFB_DOWN_ITEM_WIDTH"  : "32"                ,
        "DMA_MFB_DOWN_REGIONS" : "MFB_DOWN_REGIONS*2",
        "RQ_TUSER_WIDTH"       : "60"                ,
        "RC_TUSER_WIDTH"       : "75"                ,
        "ENDPOINT_TYPE"        : "\\\"DUMMY\\\""     ,
        "DEVICE"               : "\\\"7SERIES\\\""   ,
    },
    "2_ports" : {
        "DMA_PORTS"                 : "2"            ,
    },
    "rcb_128" : {
        "RCB_SIZE"                  : "1'b1"         ,
    },
    "slow_read" : {
        "TR_MIN"               : "300"                             ,
        "TR_MAX"               : "500"                             ,
        "ONLY_READ"            : "1"                               ,
        "TEST_NAME"            : "\\\"test::slow_dma_down_test\\\"",
    },
    "_combinations_" : (
    (), # Works the same as '("default",),' as the "default" is applied in every combination
    ("virtex7_1_port"      , "2_ports", "rcb_128", "slow_read",),
    ("s10_256_port"        ,                                   ),
    ("agilex_4p_gen5x16"   ,                                   ),
    ("agilex_1_port_300mhz",                                   ),
    ("agilex_1_port_500mhz",                                   ),
    ("agilex_1_port_300mhz", "2_ports",                        ),
    ("agilex_1_port_500mhz", "2_ports",                        ),
    ("ultrascale_1_port"   ,                                   ),
    ("ultrascale_1_port"   , "2_ports",                        ),
    ("virtex7_1_port"      ,                                   ),
    ("virtex7_1_port"      , "2_ports",                        ),
    (                        "2_ports", "rcb_128",             ),
    ("s10_256_port"        , "2_ports", "rcb_128",             ),
    ("agilex_4p_gen5x16"   , "2_ports", "rcb_128",             ),
    ("agilex_1_port_300mhz", "2_ports", "rcb_128",             ),
    ("agilex_1_port_500mhz", "2_ports", "rcb_128",             ),
    ("ultrascale_1_port"   , "2_ports", "rcb_128",             ),
    ("virtex7_1_port"      , "2_ports", "rcb_128",             ),
    ),
}
