-- tx_dma_pcie_trans_buffer.vhd: this is a specially made component to buffer PCIe transactions
-- Copyright (C) 2023 CESNET z.s.p.o.
-- Author(s): Vladislav Valek  <xvalek14@vutbr.cz>
--
-- SPDX-License-Identifier: BSD-3-Clause

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

-- Note:
-- Todo: Registers befora BRAM 
--       WNS: 1.5ns
--       BE logic is not correct at this time
--       Writing to the second port only when there's SOF in second region

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
        MFB_REGIONS     : natural := 2;
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
        --(2*((1*8*32)/8 + log2(8) + 62 + 1)-1 downto 0)
        -- 
        PCIE_MFB_META    : in  std_logic_vector(MFB_REGIONS*((MFB_REGION_SIZE*MFB_BLOCK_SIZE*MFB_ITEM_WIDTH)/8+log2(CHANNELS)+62+1)-1 downto 0);
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
    constant MFB_ITEMS    : natural := MFB_LENGTH/MFB_ITEM_WIDTH;
    -- Number of bytes in MFB word 
    constant MFB_BYTES    : natural := MFB_LENGTH/8;
    -- The Address is restricted by BAR_APERTURE (IP_core setting)
    constant BUFFER_DEPTH : natural := (2**POINTER_WIDTH)/(MFB_LENGTH/8);

    -- =============================================================================================
    -- Defining ranges for meta signal
    -- =============================================================================================
    constant META_IS_DMA_HDR_W : natural := 1;
    constant META_PCIE_ADDR_W  : natural := 62;
    constant META_CHAN_NUM_W   : natural := log2(CHANNELS);
    constant META_BE_W         : natural := (MFB_LENGTH/MFB_REGIONS)/8;

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
    signal wr_be_bram_demux         : slv_array_2d_t(MFB_REGIONS - 1 downto 0)(CHANNELS -1 downto 0)((PCIE_MFB_DATA'length/8) -1 downto 0);
    signal wr_addr_bram_by_shift    : slv_array_2d_t(MFB_REGIONS - 1 downto 0)((PCIE_MFB_DATA'length/32) -1 downto 0)(log2(BUFFER_DEPTH) -1 downto 0);
    signal wr_data_bram_bshifter    : slv_array_t(MFB_REGIONS - 1 downto 0)(MFB_LENGTH -1 downto 0);

    signal chan_num_pst             : std_logic_vector(log2(CHANNELS) -1 downto 0);
    signal chan_num_nst             : std_logic_vector(log2(CHANNELS) -1 downto 0);

    signal rd_en_bram_demux         : std_logic_vector(CHANNELS -1 downto 0);
    signal rd_data_bram_mux         : std_logic_vector(MFB_LENGTH -1 downto 0);
    signal rd_data_bram             : slv_array_2d_t(MFB_REGIONS - 1 downto 0)(CHANNELS -1 downto 0)(MFB_LENGTH -1 downto 0);
    signal rd_addr_bram_by_shift    : slv_array_t((PCIE_MFB_DATA'length/8) -1 downto 0)(log2(BUFFER_DEPTH) -1 downto 0);

    -- ================= --
    -- 2 regions support --
    -- ================= --
    -- Meta array
    signal pcie_mfb_meta_arr        : slv_array_t(MFB_REGIONS - 1 downto 0)((MFB_REGION_SIZE*MFB_BLOCK_SIZE*MFB_ITEM_WIDTH)/8+log2(CHANNELS)+62+1-1 downto 0);

    -- Converter signal: PCIE_MFB_DATA'length/32 => PCIE_MFB_DATA'length/ 8
    signal wr_addr_bram_by_trim     : slv_array_2d_t(MFB_REGIONS - 1 downto 0)((PCIE_MFB_DATA'length/8) -1 downto 0)(log2(BUFFER_DEPTH) -1 downto 0);

    -- Read/Write Address - TDP
    signal rw_addr_bram_by_mux      : slv_array_2d_t(MFB_REGIONS - 1 downto 0)((PCIE_MFB_DATA'length/8) -1 downto 0)(log2(BUFFER_DEPTH) -1 downto 0);

    -- Read data valid - TDP
    signal rd_data_valid_arr        : std_logic_vector(MFB_REGIONS - 1 downto 0);

    -- Read enable per channel
    signal rd_en_pch                : slv_array_t(MFB_REGIONS - 1 downto 0)(CHANNELS - 1 downto 0);

    -- Meta signal for whole MFB word
    signal pcie_meta_be             : slv_array_t(MFB_REGIONS - 1 downto 0)(MFB_LENGTH/8 - 1 downto 0);

    -- Input BRAM registers
    signal wr_be_bram_reg_0         : slv_array_2d_t(MFB_REGIONS - 1 downto 0)(CHANNELS -1 downto 0)((PCIE_MFB_DATA'length/8) -1 downto 0);
    signal wr_addr_bram_reg_0       : slv_array_2d_t(MFB_REGIONS - 1 downto 0)((PCIE_MFB_DATA'length/32) -1 downto 0)(log2(BUFFER_DEPTH) -1 downto 0);
    signal wr_data_bram_reg_0       : slv_array_t(MFB_REGIONS - 1 downto 0)(MFB_LENGTH -1 downto 0);

    signal wr_be_bram_reg_1         : slv_array_2d_t(MFB_REGIONS - 1 downto 0)(CHANNELS -1 downto 0)((PCIE_MFB_DATA'length/8) -1 downto 0);
    signal wr_addr_bram_reg_1       : slv_array_2d_t(MFB_REGIONS - 1 downto 0)((PCIE_MFB_DATA'length/32) -1 downto 0)(log2(BUFFER_DEPTH) -1 downto 0);
    signal wr_data_bram_reg_1       : slv_array_t(MFB_REGIONS - 1 downto 0)(MFB_LENGTH -1 downto 0);

    -- Input register
    signal inp_data_reg_0             : std_logic_vector(PCIE_MFB_DATA'range);
    signal inp_meta_reg_0             : std_logic_vector(PCIE_MFB_META'range);
    signal inp_sof_reg_0              : std_logic_vector(PCIE_MFB_SOF'range);
    signal inp_src_rdy_reg_0          : std_logic;

    signal inp_data_reg_1             : std_logic_vector(PCIE_MFB_DATA'range);
    signal inp_meta_reg_1             : std_logic_vector(PCIE_MFB_META'range);
    signal inp_sof_reg_1              : std_logic_vector(PCIE_MFB_SOF'range);
    signal inp_src_rdy_reg_1          : std_logic;    

begin
    -- =============================================================================================
    -- Input registers
    -- =============================================================================================
    inp_reg_p: process(CLK)
    begin
        if rising_edge(CLK) then
            if RESET = '1' then
                inp_data_reg_0    <= (others => '0');
                inp_meta_reg_0    <= (others => '0');
                inp_sof_reg_0     <= (others => '0');
                inp_src_rdy_reg_0 <= '0';

                inp_data_reg_1    <= (others => '0');
                inp_meta_reg_1    <= (others => '0');
                inp_sof_reg_1     <= (others => '0');
                inp_src_rdy_reg_1 <= '0';                
            else 
                inp_data_reg_0    <= PCIE_MFB_DATA;
                inp_meta_reg_0    <= PCIE_MFB_META;
                inp_sof_reg_0     <= PCIE_MFB_SOF;
                inp_src_rdy_reg_0 <= PCIE_MFB_SRC_RDY;

                inp_data_reg_1    <= inp_data_reg_0;
                inp_meta_reg_1    <= inp_meta_reg_0;
                inp_sof_reg_1     <= inp_sof_reg_0;
                inp_src_rdy_reg_1 <= inp_src_rdy_reg_0;
            end if;
        end if;
    end process;

    -- Meta array
    pcie_mfb_meta_arr   <= slv_array_deser(inp_meta_reg_1, MFB_REGIONS);

    -- =============================================================================================
    -- Address storage
    -- =============================================================================================
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
        -- When the new packet arrives, its address is stored and incremented by one region size

        -- Be careful! The number '8' is only correct for one region

        if (inp_src_rdy_reg_1 = '1') then
            -- Address Increment
            -- +16 (two regions)
            addr_cntr_nst <= addr_cntr_pst + MFB_REGIONS*MFB_BLOCK_SIZE;

            -- Last SOF - Higher takes
            for i in 0 to (MFB_REGIONS - 1) loop 
                if (inp_sof_reg_1(i) = '1') then
                    -- +16 / +8
                    addr_cntr_nst   <= unsigned(pcie_mfb_meta_arr(i)(META_PCIE_ADDR)) + (MFB_REGIONS - i)*MFB_BLOCK_SIZE;
                end if;
            end loop;
        end if;
    end process;

    -- =============================================================================================
    -- Data shift - first region
    -- =============================================================================================
    -- This process controls the shift of the input word and the corresponding byte enable signal to it.
    -- When beginning of a transaction is captured, the shift is taken directly from the current address,
    -- but when it continues, then select shift from the counter of addresses.

    -- The previous "2 downto 0" is specification of address - now generic for more regions
    -- The address system here is divided into two parts -> due to the BRAM configuration

    wr_bshifter_0_ctrl_p : process (all) is
        variable pcie_mfb_meta_addr_v : std_logic_vector(META_PCIE_ADDR_W -1 downto 0);
    begin
        wr_shift_sel(0) <= (others => '0');

        if (inp_src_rdy_reg_1 = '1') then
            if (inp_sof_reg_1(0) = '1') then
                pcie_mfb_meta_addr_v    := pcie_mfb_meta_arr(0)(META_PCIE_ADDR);
                wr_shift_sel(0)         <= pcie_mfb_meta_addr_v(log2(MFB_ITEMS) - 1  downto 0);
            else
                -- Shared address - last saved address
                wr_shift_sel(0)         <= std_logic_vector(addr_cntr_pst(log2(MFB_ITEMS) - 1 downto 0));
            end if;
        end if;
    end process;

    meta_be_g: if (MFB_REGIONS = 1) generate
        pcie_meta_be(0) <= pcie_mfb_meta_arr(0)(META_BE);
    else generate
        meta_sel_p: process(all)
        begin
            if (inp_sof_reg_1(1) = '1') then 
                -- The problem is that we only get half the information in metadata
                pcie_meta_be(0) <= pcie_mfb_meta_arr(1)(META_BE) & pcie_mfb_meta_arr(0)(META_BE);
                pcie_meta_be(1) <= (others => '0');
            else 
                pcie_meta_be(0) <= (META_BE_W - 1 downto 0 => '0') & pcie_mfb_meta_arr(0)(META_BE);
                pcie_meta_be(1) <= pcie_mfb_meta_arr(1)(META_BE) & (META_BE_W - 1 downto 0 => '0');
            end if;
        end process;
    end generate;    

    -- Connect to Port A
    wr_data_barrel_shifter_0_i : entity work.BARREL_SHIFTER_GEN
        generic map (
            BLOCKS     => MFB_REGIONS*MFB_BLOCK_SIZE,
            BLOCK_SIZE => MFB_ITEM_WIDTH,
            SHIFT_LEFT => TRUE
        )
        port map (
            DATA_IN  => inp_data_reg_1,
            DATA_OUT => wr_data_bram_bshifter(0),
            SEL      => wr_shift_sel(0)
        );

    -- Byte enable (first region) for port A
    wr_be_barrel_shifter_0_i : entity work.BARREL_SHIFTER_GEN
        generic map (
            BLOCKS     => MFB_REGIONS*MFB_BLOCK_SIZE,
            BLOCK_SIZE => 4,
            SHIFT_LEFT => TRUE
        )
        port map (
            DATA_IN  => pcie_meta_be(0),
            DATA_OUT => wr_be_bram_bshifter(0),
            SEL      => wr_shift_sel(0)
        );

    -- =============================================================================================
    -- Data shift - second region
    -- =============================================================================================
    -- This packet starts at the beginning of the second region, so we need to correct the address 
    -- by the number of DWords in region

    tworeg_bs_g: if (MFB_REGIONS = 2) generate
        wr_bshifter_1_ctrl_p : process (all) is
            variable pcie_mfb_meta_addr_v : std_logic_vector(META_PCIE_ADDR_W -1 downto 0);
        begin
            wr_shift_sel(1) <= (others => '0');

            if (inp_src_rdy_reg_1 = '1') then
                if (inp_sof_reg_1(1) = '1') then
                    -- The '+8' is MFB_BLOCK_SIZE and is only used when the SOF is in the second region
                    pcie_mfb_meta_addr_v    := std_logic_vector(unsigned(pcie_mfb_meta_arr(1)(META_PCIE_ADDR)) + 8);
                    wr_shift_sel(1)         <= pcie_mfb_meta_addr_v(log2(MFB_ITEMS) - 1  downto 0);
                -- not usefull (I hope)
                -- elsif (inp_sof_reg_1(0) = '1') then
                --     pcie_mfb_meta_addr_v    := pcie_mfb_meta_arr(0)(META_PCIE_ADDR);
                --     wr_shift_sel(1)         <= pcie_mfb_meta_addr_v(log2(MFB_ITEMS) - 1  downto 0);
                -- else 
                --     -- Shared address
                --     wr_shift_sel(1)         <= std_logic_vector(addr_cntr_pst(log2(MFB_ITEMS) - 1 downto 0));
                end if;
            end if;
        end process;

        -- This BS is used for shifting data that start in second region
        -- Connect to Port B
        wr_data_barrel_shifter_1_i : entity work.BARREL_SHIFTER_GEN
        generic map (
            BLOCKS     => MFB_REGIONS*MFB_BLOCK_SIZE,
            BLOCK_SIZE => MFB_ITEM_WIDTH,
            SHIFT_LEFT => TRUE
        )
        port map (
            DATA_IN  => inp_data_reg_1,
            DATA_OUT => wr_data_bram_bshifter(1),
            SEL      => wr_shift_sel(1)
        );       
        
        -- Byte enable (second region) port B
        wr_be_barrel_shifter_1_i : entity work.BARREL_SHIFTER_GEN
            generic map (
                BLOCKS     => MFB_REGIONS*MFB_BLOCK_SIZE,
                BLOCK_SIZE => 4,
                SHIFT_LEFT => TRUE
            )
            port map (
                DATA_IN  => pcie_meta_be(1),
                DATA_OUT => wr_be_bram_bshifter(1),
                SEL      => wr_shift_sel(1)
            );        
    end generate;

    -- =============================================================================================
    -- Address correction
    -- =============================================================================================
    -- This process increments the address on the lowest DWords when shift occurs. 
    -- That means that when data are shifted on the input, the rotation causes higher DWs to appear 
    -- on the lower positions.
    -- Writing on the same address could cause the overwrite of data already stored in lower BRAMs.

    -- Possibilites:
    --     SOF(0) SOF(1)
    -- 1)    0      0
    -- 2)    0      1
    -- 3)    1      0
    -- 4)    1      1

    wr_addr_recalc_p : process (all) is
        variable pcie_mfb_meta_addr_v : slv_array_t(MFB_REGIONS - 1 downto 0)(META_PCIE_ADDR_W -1 downto 0);
    begin

        -- This condition determines which address is used for data in the second region
        -- This includes combinations 1) and 3) for data in the second region
        if (inp_sof_reg_1(0) = '1') then
            pcie_mfb_meta_addr_v(MFB_REGIONS - 1)  := pcie_mfb_meta_arr(0)(META_PCIE_ADDR);
        else 
            pcie_mfb_meta_addr_v(MFB_REGIONS - 1)  := std_logic_vector(addr_cntr_pst);
        end if;

        -- This has been added to ensure looping compatibility
        -- Combination 1) and 2) for first region
        pcie_mfb_meta_addr_v(0) := std_logic_vector(addr_cntr_pst);

        for j in 0 to MFB_REGIONS - 1 loop
            wr_addr_bram_by_shift(j) <= (others => (others => '0'));

            if (inp_src_rdy_reg_1 = '1') then
                if (inp_sof_reg_1(j) = '1') then
                    -- Pass address to variable
                    -- Add number of items in region - used in second region
                    pcie_mfb_meta_addr_v(j) := std_logic_vector(unsigned(pcie_mfb_meta_arr(j)(META_PCIE_ADDR)) + j*MFB_BLOCK_SIZE);
    
                    wr_addr_bram_by_shift(j) <= (others => pcie_mfb_meta_addr_v(j)(log2(BUFFER_DEPTH)+log2(MFB_ITEMS) -1 downto log2(MFB_ITEMS)));

                    -- Increment address in bytes that has been overflowed
                    for i in 0 to ((MFB_LENGTH/32) -1) loop
                        if (i < unsigned(pcie_mfb_meta_addr_v(j)(log2(MFB_ITEMS) - 1 downto 0))) then
                            wr_addr_bram_by_shift(j)(i) <= std_logic_vector(unsigned(pcie_mfb_meta_addr_v(j)(log2(BUFFER_DEPTH)+log2(MFB_ITEMS) -1 downto log2(MFB_ITEMS))) + 1);
                        end if;
                    end loop;
                else
                    -- SOF in current region is 0
                    -- If this happens in the first region - The Saved address is passed
                    -- If this happens in the second region - The Current or The Saved address is passed based on SOF in first region
                    wr_addr_bram_by_shift(j) <= (others => pcie_mfb_meta_addr_v(j)(log2(BUFFER_DEPTH) + log2(MFB_ITEMS) -1 downto log2(MFB_ITEMS)));

                    -- Increment address in bytes that has been overflowed
                    for i in 0 to ((MFB_LENGTH/32) -1) loop
                        if (i < unsigned(pcie_mfb_meta_addr_v(j)(log2(MFB_ITEMS) - 1 downto 0))) then
                            wr_addr_bram_by_shift(j)(i) <= std_logic_vector(unsigned(pcie_mfb_meta_addr_v(j)(log2(BUFFER_DEPTH) + log2(MFB_ITEMS) -1 downto log2(MFB_ITEMS))) + 1);
                        end if;
                    end loop;                        
                end if;
            end if;
        end loop;
    end process;

    -- =============================================================================================
    -- Multiplexers
    -- =============================================================================================

    -- =============================================================================================
    -- Input registers - BRAM
    -- =============================================================================================
    bram_input_reg_p: process(CLK)
    begin
        if rising_edge(CLK) then 
            if RESET = '1' then 
                wr_be_bram_reg_0      <= (others => (others => (others => '0')));
                wr_addr_bram_reg_0    <= (others => (others => (others => '0')));
                wr_data_bram_reg_0    <= (others => (others => '0'));

                wr_be_bram_reg_1      <= (others => (others => (others => '0')));
                wr_addr_bram_reg_1    <= (others => (others => (others => '0')));
                wr_data_bram_reg_1    <= (others => (others => '0'));                
            else
                
                wr_be_bram_reg_0      <= wr_be_bram_demux;
                wr_addr_bram_reg_0    <= wr_addr_bram_by_shift;
                wr_data_bram_reg_0    <= wr_data_bram_bshifter;

                wr_be_bram_reg_1      <= wr_be_bram_reg_0;
                wr_addr_bram_reg_1    <= wr_addr_bram_reg_0;
                wr_data_bram_reg_1    <= wr_data_bram_reg_0;           
            end if;
        end if;
    end process;

    -- Multiplex based on value of META(Channel)
    -- Last value of the Channel is stored
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
    -- That was true, but now it stores last valid channel
    chan_num_hold_nst_logic_p : process (all) is
    begin
        chan_num_nst <= chan_num_pst;

        -- Higher takes
        if (inp_src_rdy_reg_1 = '1') then 
            for i in 0 to (MFB_REGIONS - 1) loop 
                if (inp_sof_reg_1(i) = '1') then
                    chan_num_nst <= pcie_mfb_meta_arr(i)(META_CHAN_NUM);
                end if;
            end loop;
        end if;
    end process;

    -- Select the channel information 
    -- Possibilites:
    --     SOF(0) SOF(1)
    -- 1)    0      0
    -- 2)    0      1
    -- 3)    1      0
    -- 4)    1      1
    -- In the first case, we simply select the stored value (chan_num_pst) - Both regions
    -- In the second case - First region takes stored value
    --                    - Second region takes actual value of meta(1)
    -- In the third case - First region takes actual value 
    --                   - Second region takes value of region one
    -- In the fourth case - First region takes actual value
    --                    - Second region takes actual value

    -- Default:
    -- Enable signal for each slice of BRAM
    wr_bram_data_demux_p : process (all) is
    begin
        for i in 0 to (MFB_REGIONS - 1) loop
            wr_be_bram_demux(i) <= (others => (others => '0'));

            if (inp_src_rdy_reg_1 = '1') then
                -- Default: Stored value - combination 1) and 2)
                wr_be_bram_demux(i)(to_integer(unsigned(chan_num_pst))) <= wr_be_bram_bshifter(i);

                if (inp_sof_reg_1(i) = '1') then
                    wr_be_bram_demux(i)(to_integer(unsigned(pcie_mfb_meta_arr(i)(META_CHAN_NUM)))) <= wr_be_bram_bshifter(i);
                -- This seems strange, but in the second loop it will cover combination 3)
                -- No effect in first loop
                elsif (inp_sof_reg_1(0) = '1') then 
                        wr_be_bram_demux(i)(to_integer(unsigned(pcie_mfb_meta_arr(0)(META_CHAN_NUM)))) <= wr_be_bram_bshifter(i);
                end if;
            end if;
        end loop;
    end process;

    -- One region
    sdp_bram_g: if (MFB_REGIONS = 1) generate
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

                        WR_EN       => wr_be_bram_reg_1(0)(j)(i),
                        WR_BE       => (others => '1'),
                        WR_ADDR     => wr_addr_bram_reg_1(0)(i/4),
                        WR_DATA     => wr_data_bram_reg_1(0)(i*8 +7 downto i*8),

                        RD_CLK      => CLK,
                        RD_RST      => RESET,
                        RD_EN       => '1',
                        RD_PIPE_EN  => rd_en_bram_demux(j),
                        RD_META_IN  => (others => '0'),
                        RD_ADDR     => rd_addr_bram_by_shift(i),
                        RD_DATA     => rd_data_bram(0)(j)(i*8 +7 downto i*8),
                        RD_META_OUT => open,
                        RD_DATA_VLD => open
                    );
            end generate;
        end generate;
    end generate;

    -- =============================================================================================
    -- Dual port BRAM - Control logic
    -- =============================================================================================
    tdp_bram_g: if (MFB_REGIONS = 2) generate

        -- Address management - Channel independent
        -- Multiple DWORD addresses to bytes
        addr_trim_regions_g : for i in 0 to MFB_REGIONS - 1 generate
            addr_trim_p: process(all) 
            begin
                for j in 0 to ((MFB_LENGTH/8) -1) loop
                    wr_addr_bram_by_trim(i)(j) <= wr_addr_bram_reg_1(i)(j/4);
                end loop;
            end process;
        end generate;

        -- The Address mutliplexor
        addr_mux_p : for i in 0 to MFB_REGIONS - 1 generate
            addr_mux_p: process(all)
            begin
                -- Default assignment
                rw_addr_bram_by_mux(i)  <= (others => (others => '0'));
                
                if (inp_src_rdy_reg_1 = '1') then
                    -- The first bit Byte Enable is enough to decide whether read to write
                    if (pcie_mfb_meta_arr(i)(META_BE_O) = '1') then
                        rw_addr_bram_by_mux(i) <= wr_addr_bram_by_trim(i);
                    else
                        rw_addr_bram_by_mux(i) <= rd_addr_bram_by_shift;
                    end if;
                end if;
            end process;
        end generate;

        -- Read enable - Write port priority
        rd_en_ch_g : for j in 0 to (CHANNELS -1) generate
            rd_en_reg_g : for i in 0 to (MFB_REGIONS - 1) generate
                rd_en_p: process(all)
                begin
                    -- Read enable per channel
                    rd_en_pch(i)(j) <= rd_en_bram_demux(j) and (not pcie_mfb_meta_arr(i)(META_BE_O));
                end process;
            end generate;
        end generate;

        -- =============================================================================================
        -- Dual port BRAM - 2 inputs
        -- =============================================================================================
        brams_for_channels_g : for j in 0 to (CHANNELS -1) generate
            tdp_bram_be_g : for i in 0 to ((MFB_LENGTH/8) -1) generate
                tdp_bram_be_i : entity work.DP_BRAM_BEHAV
                    generic map (
                        DATA_WIDTH => 8,
                        ITEMS      => BUFFER_DEPTH,
                        OUTPUT_REG => FALSE,
                        -- What operation will be performed first when write and read are active
                        -- in same time and same port? Possible values are:
                        -- "WRITE_FIRST" - Default mode, works on Xilinx and Intel FPGAs.
                        -- "READ_FIRST"  - This mode is not supported on Intel FPGAs, BRAM will be implemented into logic!

                        RDW_MODE_A => "WRITE_FIRST",
                        RDW_MODE_B => "WRITE_FIRST"
                    )
                    port map (
                        CLK => CLK,
                        RST => RESET,
                        -- =======================================================================
                        -- Port A
                        -- =======================================================================
                        PIPE_ENA => '1',
                        REA      => rd_en_pch(0)(j),
                        WEA      => wr_be_bram_reg_1(0)(j)(i),
                        ADDRA    => rw_addr_bram_by_mux(0)(i),
                        DIA      => wr_data_bram_reg_1(0)(i*8 +7 downto i*8),
                        DOA      => rd_data_bram(0)(j)(i*8 +7 downto i*8),
                        DOA_DV   => open,
                        -- =======================================================================
                        -- Port B
                        -- =======================================================================
                        PIPE_ENB => '1',
                        REB      => rd_en_pch(1)(j),
                        WEB      => wr_be_bram_reg_1(1)(j)(i),
                        ADDRB    => rw_addr_bram_by_mux(1)(i),
                        DIB      => wr_data_bram_reg_1(1)(i*8 +7 downto i*8),
                        DOB      => rd_data_bram(1)(j)(i*8 +7 downto i*8),
                        DOB_DV   => open
                    );
            end generate;
        end generate;

        rd_vld_p: process(CLK)
        begin
            if rising_edge(CLK) then
                rd_data_valid_arr   <= (others => '0');
                for j in 0 to MFB_REGIONS - 1 loop
                    for i in 0 to CHANNELS - 1 loop
                        if rd_en_pch(j)(i) = '1' then 
                            rd_data_valid_arr(j) <= '1';
                        end if;
                    end loop;
                end loop;
            end if;
        end process;
    end generate;

    -- =============================================================================================
    -- Demulitplexors
    -- =============================================================================================          
    -- Note: This part is common for both regions
    rd_en_demux: process (all) is
    begin
        rd_en_bram_demux <= (others => '0');

        if (RD_EN = '1') then
            rd_en_bram_demux(to_integer(unsigned(RD_CHAN))) <= '1';
        end if;
    end process;

    rd_data_demux_g: if (MFB_REGIONS = 1) generate
        RD_DATA_VLD         <= '1';
        rd_data_bram_mux <= rd_data_bram(0)(to_integer(unsigned(RD_CHAN)));
    else generate
        rd_data_demux_p: process(all)
        begin 
            rd_data_bram_mux    <= (others => '0');
            RD_DATA_VLD         <= '0';
            for i in 0 to MFB_REGIONS - 1  loop
                if rd_data_valid_arr(i) = '1' then 
                    rd_data_bram_mux <= rd_data_bram(i)(to_integer(unsigned(RD_CHAN)));
                    RD_DATA_VLD      <= '1';
                end if;
            end loop;
        end process;
    end generate;

    -- The Reading side is addressable by bytes so the number of blocks is 4 times more than on the
    -- reading side
    rd_data_barrel_shifter_i : entity work.BARREL_SHIFTER_GEN
        generic map (
            BLOCKS     => MFB_BYTES,
            BLOCK_SIZE => 8,
            SHIFT_LEFT => FALSE
        )
        port map (
            DATA_IN  => rd_data_bram_mux,
            DATA_OUT => RD_DATA,
            SEL      => RD_ADDR(log2(MFB_BYTES) - 1 downto 0)
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
