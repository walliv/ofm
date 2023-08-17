-- tx_dma_pcie_trans_buffer.vhd: this is a specially made component to buffer PCIe transactions
-- Copyright (C) 2023 CESNET z.s.p.o.
-- Author(s): Vladislav Valek  <xvalek14@vutbr.cz>
--
-- SPDX-License-Identifier: BSD-3-Clause

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

-- Note:

use work.math_pack.all;
use work.type_pack.all;

-- This component instantiaties data buffers for all channels. Internally, the component constists
-- of Block RAMs. This component has the largest footprint since data are stored by bytes for every
-- channel. The component behaves as quasi buffer to which data can by written with the resolution
-- to DWords and read with the resolution to bytes, i.e. as a RAM with different widths of addresses
-- for each port.
entity TX_DMA_PCIE_TRANS_BUFFER is
    generic (
        DEVICE : string := "ULTRASCALE";

        -- Total number of DMA Channels within this DMA Endpoint
        CHANNELS : natural := 8;

        -- =========================================================================================
        -- Input PCIe interface parameters
        -- =========================================================================================
        MFB_REGIONS     : natural := 1;
        MFB_REGION_SIZE : natural := 1;
        MFB_BLOCK_SIZE  : natural := 8;
        MFB_ITEM_WIDTH  : natural := 32;

        -- Determines the number of bytes that can be stored in the buffer.
        POINTER_WIDTH : natural := 16
        );
    port (
        CLK   : in std_logic;
        RESET : in std_logic;

        -- =========================================================================================
        -- Input MFB bus (quasi writing interface)
        -- =========================================================================================
        PCIE_MFB_DATA    : in  std_logic_vector(MFB_REGIONS*MFB_REGION_SIZE*MFB_BLOCK_SIZE*MFB_ITEM_WIDTH-1 downto 0);
        PCIE_MFB_META    : in  std_logic_vector(PCIE_MFB_REGIONS*((PCIE_MFB_REGION_SIZE*PCIE_MFB_BLOCK_SIZE*PCIE_MFB_ITEM_WIDTH)/8+log2(CHANNELS)+62+1)-1 downto 0);
        PCIE_MFB_SOF     : in  std_logic_vector(MFB_REGIONS -1 downto 0);
        PCIE_MFB_EOF     : in  std_logic_vector(MFB_REGIONS -1 downto 0);
        PCIE_MFB_SOF_POS : in  std_logic_vector(MFB_REGIONS*max(1, log2(MFB_REGION_SIZE)) -1 downto 0);
        PCIE_MFB_EOF_POS : in  std_logic_vector(MFB_REGIONS*max(1, log2(MFB_REGION_SIZE*MFB_BLOCK_SIZE)) -1 downto 0);
        PCIE_MFB_SRC_RDY : in  std_logic;
        PCIE_MFB_DST_RDY : out std_logic := '1';

        -- =========================================================================================
        -- Output reading interface
        --  
        -- Similar to BRAM block.
        -- =========================================================================================
        -- Note: This will be shared for both regions
        RD_CHAN     : in  std_logic_vector(log2(CHANNELS) -1 downto 0);
        RD_DATA     : out std_logic_vector(MFB_REGIONS*MFB_REGION_SIZE*MFB_BLOCK_SIZE*MFB_ITEM_WIDTH-1 downto 0);
        RD_ADDR     : in  std_logic_vector(POINTER_WIDTH -1 downto 0);
        RD_EN       : in  std_logic;
        RD_DATA_VLD : out std_logic
    );
end entity;

architecture FULL of TX_DMA_PCIE_TRANS_BUFFER is

    constant MFB_LENGTH   : natural := MFB_REGIONS*MFB_REGION_SIZE*MFB_BLOCK_SIZE*MFB_ITEM_WIDTH;
    -- Number of items in MFB word
    constant MFB_ITEMS    : MFB_LENGTH/MFB_ITEM_WIDTH;
    -- The Address is restricted by BAR_APERTURE (IP_core setting)
    constant BUFFER_DEPTH : natural := (2**POINTER_WIDTH)/(MFB_LENGTH/8);

    -- =============================================================================================
    -- Defining ranges for meta signal
    -- =============================================================================================
    constant META_IS_DMA_HDR_W : natural := 1;
    constant META_PCIE_ADDR_W  : natural := 62;
    constant META_CHAN_NUM_W   : natural := log2(CHANNELS);
    constant META_BE_W         : natural := MFB_LENGTH/8;

    constant META_IS_DMA_HDR_O : natural := 0;
    constant META_PCIE_ADDR_O  : natural := META_IS_DMA_HDR_O + META_IS_DMA_HDR_W;
    constant META_CHAN_NUM_O   : natural := META_PCIE_ADDR_O + META_PCIE_ADDR_W;
    constant META_BE_O         : natural := META_CHAN_NUM_O + META_CHAN_NUM_W;

    subtype META_IS_DMA_HDR is natural range META_IS_DMA_HDR_O + META_IS_DMA_HDR_W -1 downto META_IS_DMA_HDR_O;
    subtype META_PCIE_ADDR  is natural range   META_PCIE_ADDR_O + META_PCIE_ADDR_W -1 downto META_PCIE_ADDR_O;
    subtype META_CHAN_NUM   is natural range     META_CHAN_NUM_O + META_CHAN_NUM_W -1 downto META_CHAN_NUM_O;
    subtype META_BE         is natural range                 META_BE_O + META_BE_W -1 downto META_BE_O;

    -- counter of the address for each valid word following the beginning of the transaction
    signal addr_cntr_pst            : unsigned(PCIE_MFB_META(META_PCIE_ADDR)'length -1 downto 0);
    signal addr_cntr_nst            : unsigned(PCIE_MFB_META(META_PCIE_ADDR)'length -1 downto 0);

    -- control of the amount of shift on the writing barrel shifters
    signal wr_shift_sel             : slv_array_t(MFB_REGIONS - 1 downto 0)(log2(MFB_LENGTH/32) -1 downto 0);

    signal wr_be_bram_bshifter      : slv_array_t(MFB_REGIONS - 1 downto 0)((PCIE_MFB_DATA'length/8) -1 downto 0);
    signal wr_be_bram_demux         : slv_array_t(CHANNELS -1 downto 0)((PCIE_MFB_DATA'length/8) -1 downto 0);
    signal wr_addr_bram_by_shift    : slv_array_t((PCIE_MFB_DATA'length/32) -1 downto 0)(log2(BUFFER_DEPTH) -1 downto 0);
    signal wr_data_bram_bshifter    : slv_array_t(MFB_REGIONS - 1 downto 0)(MFB_LENGTH -1 downto 0);

    signal chan_num_pst             : std_logic_vector(log2(CHANNELS) -1 downto 0);
    signal chan_num_nst             : std_logic_vector(log2(CHANNELS) -1 downto 0);

    signal rd_en_bram_demux         : std_logic_vector(CHANNELS -1 downto 0);
    signal rd_data_bram_mux         : std_logic_vector(MFB_LENGTH -1 downto 0);
    signal rd_data_bram             : slv_array_t(CHANNELS -1 downto 0)(MFB_LENGTH -1 downto 0);
    signal rd_addr_bram_by_shift    : slv_array_t((PCIE_MFB_DATA'length/8) -1 downto 0)(log2(BUFFER_DEPTH) -1 downto 0);

    -- 2 regions stuff
    signal pcie_mfb_meta_arr        : slv_array_t(PCIE_MFB_REGIONS - 1 downto 0)((PCIE_MFB_REGION_SIZE*PCIE_MFB_BLOCK_SIZE*PCIE_MFB_ITEM_WIDTH)/8+log2(CHANNELS)+62+1-1 downto 0);

begin
    -- =============================================================================================
    -- Prototype
    -- =============================================================================================

    -- Meta array
    pcie_mfb_meta_arr   <= slv_array_deser(PCIE_MFB_META, PCIE_MFB_REGIONS);

    -- Last SOF
    -- Higher takes
    addr_cntr_reg_p : process (CLK) is
    begin
        if (rising_edge(CLK)) then
            if (RESET = '1') then
                addr_cntr_pst <= (others => '0');
            else
                addr_cntr_pst <= addr_cntr_nst;
            end if;
        end if;
    end process;

    addr_cntr_nst_logic_p : process (all) is
    begin
        addr_cntr_nst <= addr_cntr_pst;

        -- Increment the address for a next word by 8 (the number of DWs in the word) to be written
        -- to the BRAMs.
        -- So you are incrementing up to the next region 

        -- When the new packet arrives his address is saved and incremented by a region size
        -- What about packet that fits into one region? - Interesting

        -- Careful there! The number '8' is correct only for one region
        -- I think the EOF is not relevant at all 

        if (PCIE_MFB_SRC_RDY = '1') then
            -- +16
            addr_cntr_nst <= addr_cntr_pst + MFB_REGIONS*MFB_BLOCK_SIZE;

            if (PCIE_MFB_SOF(0) = '1') then
                -- +16
                addr_cntr_nst <= unsigned(pcie_mfb_meta_arr(0)(META_PCIE_ADDR)) + MFB_REGIONS*MFB_BLOCK_SIZE;
            end if;

            if (PCIE_MFB_SOF(1) = '1') then
                -- Note: MFB_REGIONS - 'i' be a solution
                -- +8
                addr_cntr_nst <= unsigned(pcie_mfb_meta_arr(1)(META_PCIE_ADDR)) + MFB_BLOCK_SIZE;
            end if;
        end if;
    end process;

    -- This process controls the shift of the input word and the corresponding byte enable signal to it.
    -- When beginning of a transaction is captured, the shift is taken directly from the current address,
    -- but when it continues, then select shift from the counter of addresses.

    -- This part basically takes 3 bit of address (i assume that it's addressed by DWORDS) and use them to shift 
    -- DWORDS in DATA and BYTE ENABLE signal to the LEFT (higher bits?)
    -- Block in BS = DWORD

    -- The "2 downto 0" is specification of address - now generic
    -- The address system here is divided into two parts -> becasuse of BRAM configuration

    wr_bshifter_0_ctrl_p : process (all) is
        variable pcie_mfb_meta_addr_v : std_logic_vector(META_PCIE_ADDR_W -1 downto 0);
    begin
        wr_shift_sel(0) <= (others => '0');

        if (PCIE_MFB_SRC_RDY = '1') then
            if (PCIE_MFB_SOF(0) = '1') then
                pcie_mfb_meta_addr_v    := pcie_mfb_meta_arr(0)(META_PCIE_ADDR);
                wr_shift_sel(0)         <= pcie_mfb_meta_addr_v(log2(MFB_ITEMS) - 1  downto 0);
            else
                -- Shared address
                wr_shift_sel(0)         <= std_logic_vector(addr_cntr_pst(log2(MFB_ITEMS) - 1 downto 0));
            end if;
        end if;
    end process;

    -- This packet starts at the beginning of the second region, so we need to correct the address by the number of DWords in region
    -- It can be done at the input of the BS
    -- Control for second region
    wr_bshifter_1_ctrl_p : process (all) is
        variable pcie_mfb_meta_addr_v : std_logic_vector(META_PCIE_ADDR_W -1 downto 0);
    begin
        wr_shift_sel(1) <= (others => '0');

        if (PCIE_MFB_SRC_RDY = '1') then
            if (PCIE_MFB_SOF(1) = '1') then
                pcie_mfb_meta_addr_v    := pcie_mfb_meta_arr(1)(META_PCIE_ADDR);
                wr_shift_sel(1)         <= pcie_mfb_meta_addr_v(log2(MFB_ITEMS) - 1  downto 0);
            elsif (PCIE_MFB_SOF(0) = '1') then
                pcie_mfb_meta_addr_v    := pcie_mfb_meta_arr(0)(META_PCIE_ADDR);
                wr_shift_sel(1)         <= pcie_mfb_meta_addr_v(log2(MFB_ITEMS) - 1  downto 0);                
            else 
                -- Shared address
                wr_shift_sel(1)         <= std_logic_vector(addr_cntr_pst(log2(MFB_ITEMS) - 1 downto 0));
            end if;
        end if;
    end process;

    -- Connect to Port A
    wr_data_barrel_shifter_0_i : entity work.BARREL_SHIFTER_GEN
        generic map (
            BLOCKS     => MFB_REGIONS*MFB_BLOCK_SIZE,
            BLOCK_SIZE => MFB_ITEM_WIDTH,
            SHIFT_LEFT => TRUE
        )
        port map (
            DATA_IN  => PCIE_MFB_DATA,
            DATA_OUT => wr_data_bram_bshifter(0),
            SEL      => wr_shift_sel(0)
        );

    -- This BS is used for shifting data that start in second region
    -- Connect to Port B
    wr_data_barrel_shifter_1_i : entity work.BARREL_SHIFTER_GEN
    generic map (
        BLOCKS     => MFB_REGIONS*MFB_BLOCK_SIZE,
        BLOCK_SIZE => MFB_ITEM_WIDTH,
        SHIFT_LEFT => TRUE
    )
    port map (
        DATA_IN  => PCIE_MFB_DATA,
        DATA_OUT => wr_data_bram_bshifter(1),
        -- TODO: Check that this works correctly
        SEL      => std_logic_vector(unsigned(wr_shift_sel(1)) + 8)
    );        

    
    -- Byte enable (first region) for port A
    wr_be_barrel_shifter_0_i : entity work.BARREL_SHIFTER_GEN
        generic map (
            BLOCKS     => MFB_REGIONS*MFB_BLOCK_SIZE,
            BLOCK_SIZE => 4,
            SHIFT_LEFT => TRUE
        )
        port map (
            DATA_IN  => pcie_mfb_meta_arr(0)(META_BE),
            DATA_OUT => wr_be_bram_bshifter(0),
            SEL      => wr_shift_sel(0)
        );
        
    -- Byte enable (second region) port B
    wr_be_barrel_shifter_1_i : entity work.BARREL_SHIFTER_GEN
        generic map (
            BLOCKS     => MFB_REGIONS*MFB_BLOCK_SIZE,
            BLOCK_SIZE => 4,
            SHIFT_LEFT => TRUE
        )
        port map (
            DATA_IN  => pcie_mfb_meta_arr(1)(META_BE),
            DATA_OUT => wr_be_bram_bshifter(1),
            SEL      => std_logic_vector(unsigned(wr_shift_sel(1)) + 8)
        );

    -- =============================================================================================
    -- End of Prototype
    -- =============================================================================================

    -- This process increments the address on the lowest DWords when shift occurs. 
    -- That means that when data are shifted on the input, the rotation causes higher DWs to appear on the lower positions. (True)
    -- Writing on the same address could cause the overwrite of data already stored in lower BRAMs.
    wr_addr_recalc_p : process (all) is
        variable pcie_mfb_meta_addr_v : std_logic_vector(META_PCIE_ADDR_W -1 downto 0);
    begin
        wr_addr_bram_by_shift <= (others => (others => '0'));

        if (PCIE_MFB_SRC_RDY = '1') then
            if (PCIE_MFB_SOF = "1") then
                pcie_mfb_meta_addr_v := PCIE_MFB_META(META_PCIE_ADDR);
                -- These are probably the bits that were used in shift machine
                -- So these address bit are for Buffer? - It's address within the single BRAM
                    -- Wiser me: These bits address a column of bits
                -- Default assignment
                wr_addr_bram_by_shift <= (others => pcie_mfb_meta_addr_v(log2(BUFFER_DEPTH)+3 -1 downto 3));

                for i in 0 to ((MFB_LENGTH/32) -1) loop
                    if (i < unsigned(pcie_mfb_meta_addr_v(2 downto 0))) then
                        wr_addr_bram_by_shift(i) <= std_logic_vector(unsigned(pcie_mfb_meta_addr_v(log2(BUFFER_DEPTH)+3 -1 downto 3)) + 1);
                    end if;
                end loop;
            else
                wr_addr_bram_by_shift <= (others => std_logic_vector(addr_cntr_pst(log2(BUFFER_DEPTH)+3 -1 downto 3)));

                for i in 0 to ((MFB_LENGTH/32) -1) loop
                    if (i < addr_cntr_pst(2 downto 0)) then
                        wr_addr_bram_by_shift(i) <= std_logic_vector(addr_cntr_pst(log2(BUFFER_DEPTH)+3 -1 downto 3) + 1);
                    end if;
                end loop;
            end if;
        end if;
    end process;

    -- =============================================================================================
    -- Demultiplexers
    -- =============================================================================================
    chan_num_hold_reg_p : process (CLK) is
    begin
        if (rising_edge(CLK)) then
            if (RESET = '1') then
                chan_num_pst <= (others => '0');
            else
                chan_num_pst <= chan_num_nst;
            end if;
        end if;
    end process;

    -- this FSM stores the number of a channel in order to properly steer the demultiplexers
    chan_num_hold_nst_logic_p : process (all) is
    begin
        chan_num_nst <= chan_num_pst;

        if (PCIE_MFB_SRC_RDY = '1' and PCIE_MFB_SOF = "1") then
            chan_num_nst <= PCIE_MFB_META(META_CHAN_NUM);
        end if;
    end process;

    -- 
    wr_bram_data_demux_p : process (all) is
    begin
        wr_be_bram_demux <= (others => (others => '0'));

        if (PCIE_MFB_SRC_RDY = '1') then
            if (PCIE_MFB_SOF = "1") then
                wr_be_bram_demux(to_integer(unsigned(PCIE_MFB_META(META_CHAN_NUM)))) <= wr_be_bram_bshifter;
            else
                wr_be_bram_demux(to_integer(unsigned(chan_num_pst)))                 <= wr_be_bram_bshifter;
            end if;
        end if;
    end process;

    brams_for_channels_g : for j in 0 to (CHANNELS -1) generate
        sdp_bram_be_g : for i in 0 to ((MFB_LENGTH/8) -1) generate
            sdp_bram_be_i : entity work.SDP_BRAM_BE
                generic map (
                    BLOCK_ENABLE   => false,
                    -- allow individual bytes to be assigned
                    BLOCK_WIDTH    => 8,
                    -- each BRAM allows to write a single DW
                    DATA_WIDTH     => 8,
                    -- the depth of the buffer
                    ITEMS          => BUFFER_DEPTH,
                    COMMON_CLOCK   => TRUE,
                    OUTPUT_REG     => FALSE,
                    METADATA_WIDTH => 0,
                    DEVICE         => DEVICE
                )
                port map (
                    WR_CLK  => CLK,
                    WR_RST  => RESET,

                    WR_EN       => wr_be_bram_demux(j)(i),
                    WR_BE       => (others => '1'),
                    WR_ADDR     => wr_addr_bram_by_shift(i/4),
                    WR_DATA     => wr_data_bram_bshifter(i*8 +7 downto i*8),

                    RD_CLK      => CLK,
                    RD_RST      => RESET,
                    RD_EN       => '1',
                    RD_PIPE_EN  => rd_en_bram_demux(j),
                    RD_META_IN  => (others => '0'),
                    RD_ADDR     => rd_addr_bram_by_shift(i),
                    RD_DATA     => rd_data_bram(j)(i*8 +7 downto i*8),
                    RD_META_OUT => open,
                    RD_DATA_VLD => open
                );
        end generate;
    end generate;


    -- =============================================================================================
    -- Dual port BRAM - 2 inputs
    -- =============================================================================================
    -- dp_bram_be_i : entity work.DP_BRAM_BEHAV
    --     generic map (
    --         DATA_WIDTH => 8;
    --         ITEMS      => BUFFER_DEPTH;
    --         OUTPUT_REG => FALSE;
    --         -- What operation will be performed first when write and read are active
    --         -- in same time and same port? Possible values are:
    --         -- "WRITE_FIRST" - Default mode, works on Xilinx and Intel FPGAs.
    --         -- "READ_FIRST"  - This mode is not supported on Intel FPGAs, BRAM will be implemented into logic!
            
    --         RDW_MODE_A => "READ_FIRST";
    --         RDW_MODE_B => "READ_FIRST"
    --     );
    --     port map (
    --         CLK => CLK,
    --         RST => RESET,
    --         -- =======================================================================
    --         -- Port A
    --         -- =======================================================================
    --         PIPE_ENA : in  std_logic;                                   -- Enable of port A and output register, required extra logic on Intel FPGA
    --         REA      : in  std_logic;                                   -- Read enable of port A, only for generation DOA_DV
    --         WEA      : in  std_logic;                                   -- Write enable of port A
    --         ADDRA    : in  std_logic_vector(log2(ITEMS)-1 downto 0);    -- Port A address
    --         DIA      : in  std_logic_vector(DATA_WIDTH-1 downto 0);     -- Port A data in
    --         DOA      : out std_logic_vector(DATA_WIDTH-1 downto 0);     -- Port A data out
    --         DOA_DV   : out std_logic;                                   -- Port A data out valid
    --         -- =======================================================================
    --         -- Port B
    --         -- =======================================================================
    --         PIPE_ENB : in  std_logic;                                   -- Enable of port B and output register, required extra logic on Intel FPGA
    --         REB      : in  std_logic;                                   -- Read enable of port B, only for generation DOB_DV
    --         WEB      : in  std_logic;                                   -- Write enable of port B
    --         ADDRB    : in  std_logic_vector(log2(ITEMS)-1 downto 0);    -- Port B address
    --         DIB      : in  std_logic_vector(DATA_WIDTH-1 downto 0);     -- Port B data in
    --         DOB      : out std_logic_vector(DATA_WIDTH-1 downto 0);     -- Port B data out
    --         DOB_DV   : out std_logic                                    -- Port B data out valid
    --     );
     

    rd_en_demux : process (all) is
    begin
        rd_en_bram_demux <= (others => '0');

        if (RD_EN = '1') then
            rd_en_bram_demux(to_integer(unsigned(RD_CHAN))) <= '1';
        end if;
    end process;

    rd_data_bram_mux <= rd_data_bram(to_integer(unsigned(RD_CHAN)));

    -- The Reading side is addressable by bytes so the number of blocks is 4 times more than on the
    -- reading side
    rd_data_barrel_shifter_i : entity work.BARREL_SHIFTER_GEN
        generic map (
            BLOCKS     => 32,
            BLOCK_SIZE => 8,
            SHIFT_LEFT => FALSE
        )
        port map (
            DATA_IN  => rd_data_bram_mux,
            DATA_OUT => RD_DATA,
            SEL      => RD_ADDR(4 downto 0)
        );

    rd_addr_recalc_p : process (all) is
    begin
        rd_addr_bram_by_shift <= (others => RD_ADDR(log2(BUFFER_DEPTH)+5 -1 downto 5));

        for i in 0 to ((MFB_LENGTH/8) -1) loop
            if (i < unsigned(RD_ADDR(4 downto 0))) then
                rd_addr_bram_by_shift(i) <= std_logic_vector(unsigned(RD_ADDR(log2(BUFFER_DEPTH)+5 -1 downto 5)) + 1);
            end if;
        end loop;
    end process;

end architecture;
