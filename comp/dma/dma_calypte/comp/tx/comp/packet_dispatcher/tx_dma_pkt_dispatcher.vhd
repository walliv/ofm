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

        CHANNELS       : natural := 8;
        HDR_META_WIDTH : natural := 24;
        PKT_SIZE_MAX   : natural := 2**16 -1;

        MFB_REGIONS     : natural := 1;
        MFB_REGION_SIZE : natural := 8; -- 4/8
        MFB_BLOCK_SIZE  : natural := 8;
        MFB_ITEM_WIDTH  : natural := 8;

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

    -- Next address
    signal next_addr_d    : unsigned(BUFF_RD_ADDR'range);
    signal next_addr_q    : unsigned(BUFF_RD_ADDR'range);
    -- Current address
    signal current_addr_d : unsigned(BUFF_RD_ADDR'range);
    signal current_addr_q : unsigned(BUFF_RD_ADDR'range);    

    signal byte_cntr_d    : unsigned(log2(PKT_SIZE_MAX+1) -1 downto 0);
    signal byte_cntr_q    : unsigned(log2(PKT_SIZE_MAX+1) -1 downto 0);

    -- Read FSM
    type rd_fsm_state_t is (S_IDLE, S_WR_BUFF, S_RD_BUFF);
    signal rd_fsm_pst : rd_fsm_state_t := S_IDLE;
    signal rd_fsm_nst : rd_fsm_state_t := S_IDLE;

    type out_fsm_state_t is (S_IDLE, S_WR_BUFF_OUT, S_RD_BUFF_OUT, S_UPDATE_STATUS);
    signal out_fsm_pst : out_fsm_state_t := S_IDLE;
    signal out_fsm_nst : out_fsm_state_t := S_IDLE;

    type pkt_dispatch_state_t is (S_IDLE, S_PKT_MIDDLE, S_UPDATE_STATUS);
    signal pkt_dispatch_pst : pkt_dispatch_state_t := S_IDLE;
    signal pkt_dispatch_nst : pkt_dispatch_state_t := S_IDLE;

    signal disp_fsm_mfb_sof     : std_logic_vector(USR_MFB_SOF'range);
    signal disp_fsm_mfb_eof     : std_logic_vector(USR_MFB_EOF'range);
    signal disp_fsm_mfb_eof_pos : std_logic_vector(USR_MFB_EOF_POS'range);
    signal disp_fsm_mfb_src_rdy : std_logic;
    signal mfb_dst_rdy_reg      : std_logic;
    signal buff_rd_data_reg     : std_logic_vector(BUFF_RD_DATA'range);

    signal fr_len_round_up_msk : unsigned(16 -1 downto 0);
    signal fr_len_rounded      : unsigned(16 -1 downto 0);

    -- Buffer signals
    signal disp_buffer_data    : std_logic_vector(MFB_LENGTH - 1 downto 0);
    signal disp_buffer_vld     : std_logic;
    signal disp_buffer_src_rdy : std_logic;
    signal disp_buffer_dst_rdy : std_logic;

    signal rd_fsm_sof          : std_logic_vector(MFB_REGIONS -1 downto 0);
    signal rd_fsm_eof          : std_logic_vector(MFB_REGIONS -1 downto 0);
    signal rd_fsm_sof_pos      : std_logic_vector(MFB_REGIONS*max(1, log2(MFB_REGION_SIZE)) -1 downto 0);
    signal rd_fsm_eof_pos      : std_logic_vector(MFB_REGIONS*max(1, log2(MFB_REGION_SIZE*MFB_BLOCK_SIZE)) -1 downto 0);

    signal disp_buffer_sof     : std_logic_vector(MFB_REGIONS -1 downto 0);
    signal disp_buffer_eof     : std_logic_vector(MFB_REGIONS -1 downto 0);
    signal disp_buffer_sof_pos : std_logic_vector(MFB_REGIONS*max(1, log2(MFB_REGION_SIZE)) -1 downto 0);
    signal disp_buffer_eof_pos : std_logic_vector(MFB_REGIONS*max(1, log2(MFB_REGION_SIZE*MFB_BLOCK_SIZE)) -1 downto 0);

    signal disp_buffer_aempty  : std_logic;
    signal disp_buffer_afull   : std_logic;
    signal disp_buffer_status  : std_logic_vector(log2((PKT_SIZE_MAX + 1)/(MFB_LENGTH/8)) downto 0);

    -- Output FSM
    signal out_meta_chan_d     : std_logic_vector(log2(CHANNELS) -1 downto 0);
    signal out_meta_chan_q     : std_logic_vector(log2(CHANNELS) -1 downto 0);

    signal out_meta_data_d     : std_logic_vector(DMA_HDR_WIDTH -1 downto 0);
    signal out_meta_data_q     : std_logic_vector(DMA_HDR_WIDTH -1 downto 0);
    
begin
    -- =============================================================================================
    -- RD_FSM - Read State Machine
    -- =============================================================================================

    rd_fsm_reg_p : process (CLK) is
    begin
        if (rising_edge(CLK)) then
            if (RESET = '1') then
                rd_fsm_pst      <= S_IDLE;
                next_addr_q     <= (others => '0');
                current_addr_q  <= (others => '0');
                byte_cntr_q     <= (others => '0');                
            else
                rd_fsm_pst      <= rd_fsm_pst_nst;
                next_addr_q     <= next_addr_d;
                current_addr_q  <= current_addr_d;
                byte_cntr_q     <= byte_cntr_d;
            end if;
        end if;
    end process

    -- Note: RD_DATA_VLD is not considered
    rd_fsm_nst_logic_p : process (all) is
        variable dma_hdr_frame_ptr_v    : unsigned(DMA_FRAME_PTR_W -1 downto 0);
        variable dma_hdr_frame_length_v : unsigned(DMA_FRAME_LENGTH_W -1 downto 0);
    begin
        rd_fsm_pst_nst  <= rd_fsm_pst;
        next_addr_d     <= next_addr_q;
        current_addr_d  <= current_addr_q;
        byte_cntr_d     <= byte_cntr_q;

        dma_hdr_frame_ptr_v    := unsigned(HDR_BUFF_DATA(DMA_FRAME_PTR));
        dma_hdr_frame_length_v := unsigned(HDR_BUFF_DATA(DMA_FRAME_LENGTH)); 

        rd_fsm_sof      <= (others => '0');
        rd_fsm_eof      <= (others => '0');
        rd_fsm_sof_pos  <= (others => '0'); -- ok
        rd_fsm_eof_pos  <= std_logic_vector(dma_hdr_frame_length_v(USR_MFB_EOF_POS'range) - 1);

        -- PCIe buffer ctrl
        BUFF_RD_ADDR    <= (others => '0');
        BUFF_RD_EN      <= '0';

        HDR_BUFF_DST_RDY    <= '0';

        -- Note: After RD_EN is set, the RD_DATA_VLD signal will be high next clock
        -- If not, the address and enable has to be high as long as RD_DATA_VLD
        -- It seems that in the new design there must be next_addr (done) and current_addr
        -- The next_addr will be used when RD_DATA_VLD is high
        -- The current_addr will be used when RD_DATA_VLD is low (we wait in case both ports in the buffer are in use)
        case rd_fsm_pst is
            when S_IDLE     =>
                if (HDR_BUFF_SRC_RDY = '1' then  
                    -- Accept DMA header
                    if ENABLED_CHANS(to_integer(unsigned(HDR_BUFF_CHAN))) = '1') then
                        rd_fsm_nst <= S_WR_BUFF;
                        
                        -- Save address and byte lenght of incoming packet
                        next_addr_d      <= dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range) + (USR_MFB_DATA'length /8);
                        current_addr_d   <= dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range);
                        byte_cntr_d      <= resize(dma_hdr_frame_length_v, byte_cntr_d'length);

                        -- Request Buffer
                        BUFF_RD_ADDR     <= std_logic_vector(dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range));
                        BUFF_RD_EN       <= '1';

                    -- Discard DMA header
                    else
                        HDR_BUFF_DST_RDY    <= '1';
                    end if;
                end if;
            
            -- Write data to the FIFO based on DMA_HDR information
            when S_WR_BUFF  =>
                -- Request Buffer
                BUFF_RD_ADDR    <= std_logic_vector(current_addr_q);
                BUFF_RD_EN      <= '1';

                if (BUFF_RD_DATA_VLD = '1') then
                    -- At this point, the buffer is ready to accept a new request.
                    BUFF_RD_ADDR    <= std_logic_vector(next_addr_q);
                    current_addr_d  <= next_addr_q;

                    -- With every received MFB word, the address and remaining data are updated
                    next_addr_d     <= next_addr_q + (USR_MFB_DATA'length /8);
                    byte_cntr_d     <= byte_cntr_q - (USR_MFB_DATA'length /8);

                    -- Check if there is any data left
                    if (byte_cntr_q <= (USR_MFB_DATA'length /8)) then
                        -- This means that pre-update value is small enough to fit in MFB word 
                        BUFF_RD_EN       <= '0';
                        rd_fsm_nst       <= S_RD_BUFF;

                        -- Last part of packet
                        rd_fsm_eof       <= '1';
                    end if;

                    -- First written word gets SOF 
                    if disp_buffer_aempty = '1' then 
                        rd_fsm_sof       <= '1';
                    end if;
                end if;

            -- Wait until FIFO is empty (USR_MFB took all the data)
            when S_RD_BUFF  =>
                if disp_buffer_aempty = '1' then 
                    rd_fsm_nst <= S_IDLE;
                end if;

        end case;
    end process;

    -- =============================================================================================
    -- BUFFER - Input FIFO
    -- =============================================================================================
    dispatcher_input_fifo_i: entity work.MFB_FIFOX
        generic map (
            REGIONS             => MFB_REGIONS,
            REGION_SIZE         => MFB_REGION_SIZE,
            BLOCK_SIZE          => MFB_BLOCK_SIZE,
            ITEM_WIDTH          => MFB_ITEM_WIDTH,

            META_WIDTH          : natural := 0;
        
            FIFO_DEPTH          => (PKT_SIZE_MAX + 1)/(MFB_LENGTH/8),
            DEVICE              => DEVICE,
            ALMOST_FULL_OFFSET  => 0,
            ALMOST_EMPTY_OFFSET => 0
        )
        port map(
            -- =========================================================================
            -- CLOCK AND RESET
            -- =========================================================================
            CLK => CLK,
            RST => RESET,

            -- Metadata:
            -- USR_MFB_META_HDR_META : out std_logic_vector(HDR_META_WIDTH -1 downto 0);
            -- USR_MFB_META_CHAN     : out std_logic_vector(log2(CHANNELS) -1 downto 0);
            -- USR_MFB_META_PKT_SIZE : out std_logic_vector(log2(PKT_SIZE_MAX+1) -1 downto 0);
        
            RX_DATA     => BUFF_RD_DATA,
            RX_META     : in  std_logic_vector(REGIONS*META_WIDTH-1 downto 0) := (others => '0');
            RX_SOF      => rd_fsm_sof,
            RX_EOF      => rd_fsm_eof,
            RX_SOF_POS  => rd_fsm_sof_pos,
            RX_EOF_POS  => rd_fsm_eof_pos,
            RX_SRC_RDY  => BUFF_RD_DATA_VLD,
            RX_DST_RDY  => open,
        
            TX_DATA     => disp_buffer_data,
            TX_META     : out std_logic_vector(REGIONS*META_WIDTH-1 downto 0);
            TX_SOF      => disp_buffer_sof,
            TX_EOF      => disp_buffer_eof,
            TX_SOF_POS  => disp_buffer_sof_pos,
            TX_EOF_POS  => disp_buffer_eof_pos,
            TX_SRC_RDY  => disp_buffer_src_rdy,
            TX_DST_RDY  => disp_buffer_dst_rdy,
        
            FIFO_STATUS => disp_buffer_status,
            FIFO_AFULL  => disp_buffer_afull,
            FIFO_AEMPTY => disp_buffer_aempty
        );

    -- =============================================================================================
    -- OUT_FSM - Output State Machine
    -- =============================================================================================
    -- TODO: SOF, EOF, EOF_POS
    -- NOTE: the S_SOF or S_EOF are not usable
    out_fsm_reg_p : process (CLK) is
    begin
        if (rising_edge(CLK)) then
            if (RESET = '1') then
                out_fsm_pst     <= S_IDLE;
                out_meta_chan_q <= (others => '0');
                out_meta_data_q <= (others => '0');
            else--if (USR_MFB_DST_RDY = '1') then
                out_fsm_pst     <= out_fsm_nst;
                out_meta_chan_q <= out_meta_chan_d;
                out_meta_data_q <= out_meta_data_d;
            end if;
        end if;
    end process

    out_fsm_nst_logic_p : process (all) is
        variable dma_hdr_frame_ptr_v    : unsigned(DMA_FRAME_PTR_W -1 downto 0);
        variable dma_hdr_frame_length_v : unsigned(DMA_FRAME_LENGTH_W -1 downto 0);
    begin
        out_fsm_nst     <= out_fsm_pst;
        out_meta_chan_d <= out_meta_chan_q;
        out_meta_data_d <= out_meta_data_q;

        disp_buffer_dst_rdy <= '0';

        USR_MFB_SRC_RDY     <= '0';

        USR_MFB_META_HDR_META   <= resize(out_meta_data_q(DMA_USR_METADATA),HDR_META_WIDTH);
        USR_MFB_META_CHAN       <= out_meta_chan_q;
        USR_MFB_META_PKT_SIZE   <= out_meta_data_q(DMA_FRAME_LENGTH)(USR_MFB_META_PKT_SIZE'range);

        PKT_SENT_INC <= '0';
        UPD_HDP_EN   <= '0';
        UPD_HHP_EN   <= '0';


        dma_hdr_frame_ptr_v    := unsigned(HDR_BUFF_DATA(DMA_FRAME_PTR));
        dma_hdr_frame_length_v := unsigned(HDR_BUFF_DATA(DMA_FRAME_LENGTH));

        -- What about small packets (the one that can fit in one MFB word)
        case out_fsm_pst is
            -- The S_IDLE will be triggered faster than we receive the data from buffer
            when S_IDLE             =>
                if (HDR_BUFF_SRC_RDY = '1' then
                    if ENABLED_CHANS(to_integer(unsigned(HDR_BUFF_CHAN))) = '1') then
                        out_fsm_nst <= S_WR_BUFF_OUT;

                        -- Store meta signals for later usage
                        out_meta_chan_d <= HDR_BUFF_CHAN;
                        out_meta_data_d <= HDR_BUFF_DATA;
                end if;
                
            -- Wait until the FIFO is written to
            when S_WR_BUFF_OUT      =>
                if (BUFF_RD_DATA_VLD = '1') then 
                    if (byte_cntr_q <= (USR_MFB_DATA'length /8)) then
                        out_fsm_nst  <= S_RD_BUFF_OUT;
                    end if;
                end if;

            -- Read data from FIFO until it's empty
            when S_RD_BUFF_OUT      =>
                disp_buffer_dst_rdy <= USR_MFB_DST_RDY;
                USR_MFB_SRC_RDY     <= disp_buffer_src_rdy;

                -- Set the EOF in transition OR in update status
                -- Maybe there (it was like that in previous implementation)
                -- Note: same as in previous fsm
                if (disp_buffer_aempty = '1') then
                    out_fsm_nst <= S_UPDATE_STATUS;
                end if;

            -- Set the EOF of the last word
            when S_UPDATE_STATUS    =>
                HDR_BUFF_DST_RDY <= '1';
                PKT_SENT_INC     <= '1';
                UPD_HDP_EN       <= '1';
                UPD_HHP_EN       <= '1';

                out_fsm_nst         <= S_IDLE;

        end case;
    end process;

    BUFF_RD_CHAN <= HDR_BUFF_CHAN;

    -- Statistics
    PKT_SENT_CHAN  <= HDR_BUFF_CHAN;
    PKT_SENT_BYTES <= HDR_BUFF_DATA(DMA_FRAME_LENGTH)(PKT_SENT_BYTES'range);

    fr_len_round_up_msk <= not to_unsigned(31,16);
    fr_len_rounded <= (unsigned(HDR_BUFF_DATA(DMA_FRAME_LENGTH)) + 31) and fr_len_round_up_msk;

    UPD_HDP_CHAN <= HDR_BUFF_CHAN;
    UPD_HDP_DATA <= std_logic_vector(resize(fr_len_rounded + unsigned(HDR_BUFF_DATA(DMA_FRAME_PTR)), DATA_POINTER_WIDTH));
    UPD_HHP_CHAN <= HDR_BUFF_CHAN;
    UPD_HHP_DATA <= std_logic_vector(unsigned(HDR_BUFF_ADDR(1 + DMA_HDR_POINTER_WIDTH -1 downto 1)) + 1);

    -- User MFB
    USR_MFB_DATA        <= disp_buffer_data;
    USR_MFB_SOF         <= disp_buffer_sof;
    USR_MFB_EOF         <= disp_buffer_eof;
    USR_MFB_SOF_POS     <= disp_buffer_sof_pos;
    USR_MFB_EOF_POS     <= disp_buffer_eof_pos;
    USR_MFB_SRC_RDY     <= disp_buffer_src_rdy;
    disp_buffer_dst_rdy <= USR_MFB_DST_RDY;
