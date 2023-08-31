-- tx_dma_pkt_dispatcher.vhd: this component dispatches the DMA frames from buffers to the user logic
-- Copyright (C) 2023 CESNET z.s.p.o.
-- Author(s): Vladislav Valek  <xvalek14@vutbr.cz>
--
-- SPDX-License-Identifier: BSD-3-Clause

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

-- Note:
-- Todo: Add FIFO with counter (so we can write data into it while reading them out)
--       Support different latency of Buffer
--       Backward compatibility for one region

use work.math_pack.all;
use work.type_pack.all;
use work.dma_hdr_pkg.all;

-- This component dispatches the frames from data buffers according to available DMA Headers. Frames
-- are dispatched from all channels in order in which DMA headers came from the PCI Express.
-- After dispatching a frame, the component issues an update of header and data pointers. If a channel
-- is already stopped and DMA header for this channel occurs on the input of this component, this
-- DMA header is dropped and no frame data are dispatched to the output as well as no update of
-- pointers is issued.
entity TX_DMA_PKT_DISPATCHER is
    generic (
        DEVICE : string := "ULTRASCALE";

        CHANNELS            : natural := 8;
        HDR_META_WIDTH      : natural := 24;
        PKT_SIZE_MAX        : natural := 2**16 -1;

        -- For one region is used SDP BRAM
        -- For two regions is used TDP BRAM (BUFF_RD_DATA_VLD is used)
        -- => Different latencies while reading
        PCIE_MFB_REGIONS    : natural := 2; -- 1/2

        MFB_REGIONS         : natural := 1;
        MFB_REGION_SIZE     : natural := 8; -- 4/8
        MFB_BLOCK_SIZE      : natural := 8;
        MFB_ITEM_WIDTH      : natural := 8;

        DATA_POINTER_WIDTH    : natural := 16;
        DMA_HDR_POINTER_WIDTH : natural := 9
         
    );
    port (
        CLK   : in std_logic;
        RESET : in std_logic;

        -- =========================================================================================
        -- Outuput MFB interface to user logic
        -- =========================================================================================
        USR_MFB_META_HDR_META : out std_logic_vector(HDR_META_WIDTH -1 downto 0);
        USR_MFB_META_CHAN     : out std_logic_vector(log2(CHANNELS) -1 downto 0);
        USR_MFB_META_PKT_SIZE : out std_logic_vector(log2(PKT_SIZE_MAX+1) -1 downto 0);

        USR_MFB_DATA    : out std_logic_vector(MFB_REGIONS*MFB_REGION_SIZE*MFB_BLOCK_SIZE*MFB_ITEM_WIDTH-1 downto 0);
        USR_MFB_SOF     : out std_logic_vector(MFB_REGIONS -1 downto 0);
        USR_MFB_EOF     : out std_logic_vector(MFB_REGIONS -1 downto 0);
        USR_MFB_SOF_POS : out std_logic_vector(MFB_REGIONS*max(1, log2(MFB_REGION_SIZE)) -1 downto 0);
        USR_MFB_EOF_POS : out std_logic_vector(MFB_REGIONS*max(1, log2(MFB_REGION_SIZE*MFB_BLOCK_SIZE)) -1 downto 0);
        USR_MFB_SRC_RDY : out std_logic;
        USR_MFB_DST_RDY : in  std_logic;

        -- =========================================================================================
        -- Input interface from header buffer
        -- =========================================================================================
        -- This is not an address for reading interface of the buffer, but the addres on which the
        -- current header has been written.
        HDR_BUFF_ADDR    : in  std_logic_vector(62 -1 downto 0);
        HDR_BUFF_CHAN    : in  std_logic_vector(log2(CHANNELS) -1 downto 0);
        HDR_BUFF_DATA    : in  std_logic_vector(DMA_HDR_WIDTH -1 downto 0);
        HDR_BUFF_SRC_RDY : in  std_logic;
        HDR_BUFF_DST_RDY : out std_logic;

        -- =========================================================================================
        -- Reading interface to the data buffer
        -- =========================================================================================
        BUFF_RD_CHAN     : out std_logic_vector(log2(CHANNELS) -1 downto 0);
        BUFF_RD_DATA     : in  std_logic_vector(MFB_REGIONS*MFB_REGION_SIZE*MFB_BLOCK_SIZE*MFB_ITEM_WIDTH-1 downto 0);
        BUFF_RD_ADDR     : out std_logic_vector(DATA_POINTER_WIDTH -1 downto 0);
        BUFF_RD_EN       : out std_logic;
        -- Multiple region support
        BUFF_RD_DATA_VLD : in  std_logic;

        -- =========================================================================================
        -- Interface to the software manager
        --
        -- For pointer update and incrementing of packet counter.
        -- =========================================================================================
        PKT_SENT_CHAN  : out std_logic_vector(log2(CHANNELS) -1 downto 0);
        PKT_SENT_INC   : out std_logic;
        PKT_SENT_BYTES : out std_logic_vector(log2(PKT_SIZE_MAX+1) -1 downto 0);

        ENABLED_CHANS : in std_logic_vector(CHANNELS -1 downto 0);

        UPD_HDP_CHAN : out std_logic_vector(log2(CHANNELS) -1 downto 0);
        UPD_HDP_DATA : out std_logic_vector(DATA_POINTER_WIDTH -1 downto 0);
        UPD_HDP_EN   : out std_logic;

        UPD_HHP_CHAN : out std_logic_vector(log2(CHANNELS) -1 downto 0);
        UPD_HHP_DATA : out std_logic_vector(DMA_HDR_POINTER_WIDTH -1 downto 0);
        UPD_HHP_EN   : out std_logic
    );
end entity;

architecture FULL of TX_DMA_PKT_DISPATCHER is
    -- Constants:
    constant MFB_LENGTH   : natural := MFB_REGIONS*MFB_REGION_SIZE*MFB_BLOCK_SIZE*MFB_ITEM_WIDTH;
    constant META_LENGTH  : natural := HDR_META_WIDTH + log2(CHANNELS) + log2(PKT_SIZE_MAX + 1);

    type request_fsm_t is (S_IDLE, S_PKT_MIDDLE, S_UPDATE_STATUS);
    signal req_fsm_pst : request_fsm_t := S_IDLE;
    signal req_fsm_nst : request_fsm_t := S_IDLE;

    type disp_fsm_t is (S_IDLE, S_PKT_MIDDLE, S_UPDATE_STATUS);
    signal req_fsm_pst : disp_fsm_t := S_IDLE;
    signal req_fsm_nst : disp_fsm_t := S_IDLE;

    signal addr_cntr_pst : unsigned(BUFF_RD_ADDR'range);
    signal addr_cntr_nst : unsigned(BUFF_RD_ADDR'range);

    signal byte_cntr_pst : unsigned(log2(PKT_SIZE_MAX+1) -1 downto 0);
    signal byte_cntr_nst : unsigned(log2(PKT_SIZE_MAX+1) -1 downto 0);

    -- Requester
    signal req_data     : std_logic_vector(MFB_LENGTH - 1 downto 0);
    signal req_meta     : std_logic_vector(META_LENGTH - 1 downto 0);
    signal req_src_rdy  : std_logic;
    signal req_dst_rdy  : std_logic;

    signal req_fifo_en  : std_logic;

    -- Dispatcher
    signal buff_data    : std_logic_vector(MFB_LENGTH - 1 downto 0);
    signal buff_meta    : std_logic_vector(META_LENGTH - 1 downto 0);
    signal buff_src_rdy : std_logic;
    signal buff_dst_rdy : std_logic;
    signal buff_status  : std_logic_vector(log2((PKT_SIZE_MAX + 1)/(MFB_LENGTH/8)) downto 0);
    signal buff_afull   : std_logic;
    signal buff_aempty  : std_logic;

    signal buff_eof     : std_logic_vector(MFB_REGIONS - 1 downto 0);

    signal disp_src_rdy : std_logic;
    signal disp_dst_rdy : std_logic;

   
begin
    -- =============================================================================================
    -- Requester - Requests data based on accepted DMA_HDR
    -- =============================================================================================
    pkt_req_fsm_reg_p : process (CLK) is
    begin
        if (rising_edge(CLK)) then
            if (RESET = '1') then
                req_fsm_pst     <= S_IDLE;
                addr_cntr_pst   <= (others => '0');
                byte_cntr_pst   <= (others => '0');
            elsif (req_dst_rdy = '1') then
                req_fsm_pst     <= req_fsm_nst;
                addr_cntr_pst   <= addr_cntr_nst;
                byte_cntr_pst   <= byte_cntr_nst;
            end if;
        end if;
    end process;

    pkt_req_fsm_nst_logic_p : process (all) is
        variable dma_hdr_frame_ptr_v    : unsigned(DMA_FRAME_PTR_W -1 downto 0);
        variable dma_hdr_frame_length_v : unsigned(DMA_FRAME_LENGTH_W -1 downto 0);
    begin
        req_fsm_nst         <= req_fsm_pst;

        addr_cntr_nst       <= addr_cntr_pst;
        byte_cntr_nst       <= byte_cntr_pst;

        HDR_BUFF_DST_RDY    <= '0';

        BUFF_RD_ADDR        <= (others => '0');
        BUFF_RD_EN          <= '0';

        req_src_rdy         <= BUFF_RD_DATA_VLD;
        req_fifo_en         <= '0';

        dma_hdr_frame_ptr_v    := unsigned(HDR_BUFF_DATA(DMA_FRAME_PTR));
        dma_hdr_frame_length_v := unsigned(HDR_BUFF_DATA(DMA_FRAME_LENGTH));

        case req_fsm_pst is
            when S_IDLE =>
                req_src_rdy    <= '0';
                if (HDR_BUFF_SRC_RDY = '1') then
                    if (ENABLED_CHANS(to_integer(unsigned(HDR_BUFF_CHAN))) = '1') then
                        -- Data request
                        BUFF_RD_ADDR    <= std_logic_vector(dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range));
                        BUFF_RD_EN      <= '1';

                        -- Save current data
                        addr_cntr_nst   <= dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range);
                        byte_cntr_nst   <= resize(dma_hdr_frame_length_v, byte_cntr_nst'length);

                        -- Should be alright even though the packet will fit into MFB_WORD
                        req_fsm_nst <= S_PKT_MIDDLE;
                    else 
                        -- Discard header
                        HDR_BUFF_DST_RDY <= req_dst_rdy;
                    end if;
                end if;

            when S_PKT_MIDDLE =>
                -- Data request
                BUFF_RD_EN      <= '1';
                BUFF_RD_ADDR    <= std_logic_vector(addr_cntr_pst);

                if (BUFF_RD_DATA_VLD = '1') then
                    BUFF_RD_ADDR    <= std_logic_vector(addr_cntr_pst + (USR_MFB_DATA'length /8));
                    -- Update header data
                    addr_cntr_nst   <= addr_cntr_pst + (USR_MFB_DATA'length /8);
                    byte_cntr_nst   <= byte_cntr_pst - (USR_MFB_DATA'length /8);
                    
                    -- There are no data left
                    if (byte_cntr_pst <= (USR_MFB_DATA'length /8)) then
                        req_fsm_nst <= S_UPDATE_STATUS;
                    end if;
                end if;

            when S_UPDATE_STATUS =>
                -- This will be used when TDP is used in PCIe buffer
                req_fifo_en <= '1';
                if buff_aempty = '1' then 
                    req_fsm_nst <= S_IDLE;
                end if;

        end case;
    end process;

    req_data    <= BUFF_RD_DATA;
    req_meta    <= USR_MFB_META_HDR_META & USR_MFB_META_CHAN & USR_MFB_META_PKT_SIZE;
    
    -- =============================================================================================
    -- Temporary Buffer
    -- =============================================================================================
    -- Note: FIFO should have enough space for whole packet
        disp_buffer_i: entity work.MFB_FIFOX
        generic map (
            REGIONS             => MFB_REGIONS,
            REGION_SIZE         => MFB_REGION_SIZE,
            BLOCK_SIZE          => MFB_BLOCK_SIZE,
            ITEM_WIDTH          => MFB_ITEM_WIDTH,

            FIFO_DEPTH          => (PKT_SIZE_MAX + 1)/(MFB_LENGTH/8),
            DEVICE              => DEVICE,
            ALMOST_FULL_OFFSET  => 0,
            ALMOST_EMPTY_OFFSET => 0
        )
        port map(
            CLK => CLK,
            RST => RESET,

            RX_DATA     => req_data,
            RX_META     => req_meta,
            RX_SOF      => (others => '0'),
            RX_EOF      => (others => '0'),
            RX_SOF_POS  => (others => '0'),
            RX_EOF_POS  => (others => '0'),
            RX_SRC_RDY  => BUFF_RD_DATA_VLD,
            RX_DST_RDY  => req_dst_rdy,

            TX_DATA     => buff_data,
            TX_META     => buff_meta,
            TX_SOF      => open,
            TX_EOF      => open,
            TX_SOF_POS  => open,
            TX_EOF_POS  => open,
            TX_SRC_RDY  => buff_src_rdy,
            TX_DST_RDY  => buff_dst_rdy,

            FIFO_STATUS => buff_status,
            FIFO_AFULL  => buff_afull,
            FIFO_AEMPTY => buff_aempty
        );

        disp_src_rdy    <= buff_src_rdy when req_fifo_en = '1'         else '0';
        buff_dst_rdy    <= disp_dst_rdy when req_fifo_en = '1'         else '0';
        buff_eof(0)     <=          '1' when unsigned(buff_status) = 1 else '0';

        -- TODO: Add register wall

    -- =============================================================================================
    -- Wires
    -- =============================================================================================
    -- This is used when SDP BRAM is used in PCIe Buffer
    buff_data       <= req_data;
    buff_meta       <= req_meta;
    buff_src_rdy    <= HDR_BUFF_SRC_RDY;
    buff_eof(0)     <= '1' when byte_cntr_pst <= (USR_MFB_DATA'length /8) else '0';

    req_dst_rdy     <= USR_MFB_DST_RDY;


    -- =============================================================================================
    -- Dispatcher - Handles communication with USR_MFB
    -- =============================================================================================
    -- Note:         disp_src_rdy
    --               disp_dst_rdy

    pkt_disp_fsm_reg_p : process (CLK) is
    begin
        if (rising_edge(CLK)) then
            if (RESET = '1') then
                disp_fsm_pst     <= S_IDLE;
            elsif (USR_MFB_DST_RDY = '1') then
                disp_fsm_pst     <= disp_fsm_nst;
            end if;
        end if;
    end process;

    pkt_disp_fsm_nst_logic_p : process (all) is
    begin
        disp_fsm_nst         <= disp_fsm_pst;

        case disp_fsm_pst is
            when S_IDLE =>
                req_src_rdy    <= '0';
                if (buff_src_rdy = '1') then
                    disp_fsm_mfb_sof(0)  <= '1';
                    disp_fsm_mfb_src_rdy <= '1';

                    if (buff_eof(0) = '1') then
                        pkt_dispatch_nst <= S_UPDATE_STATUS;
                    else
                        pkt_dispatch_nst <= S_PKT_MIDDLE;
                    end if;

                    if (buff_eof(0) = '1') then
                        addr_cntr_nst        <= dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range);
                        disp_fsm_mfb_eof(0)  <= '1';

                        disp_fsm_mfb_eof_pos <= std_logic_vector(dma_hdr_frame_length_v(USR_MFB_EOF_POS'range) - 1);
                    end if;

                end if;

            when S_PKT_MIDDLE =>

            when S_UPDATE_STATUS =>

        end case;
    end process;


end architecture;