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

    type out_fsm_state_t is (S_IDLE, S_WR_BUFF_OUT, S_SOF, S_RD_BUFF_OUT, S_EOF);
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

    signal disp_buffer_empty   : std_logic;
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

        -- PCIe buffer ctrl
        BUFF_RD_ADDR <= (others => '0');
        BUFF_RD_EN   <= '0';

        dma_hdr_frame_ptr_v    := unsigned(HDR_BUFF_DATA(DMA_FRAME_PTR));
        dma_hdr_frame_length_v := unsigned(HDR_BUFF_DATA(DMA_FRAME_LENGTH));        

        -- Note: After RD_EN is set, the RD_DATA_VLD signal will be high next clock 
        -- If not, the address and enable has to be high as long as RD_DATA_VLD
        -- It seems that in the new design there must be next_addr (done) and current_addr
        -- The next_addr will be used when RD_DATA_VLD is high
        -- The current_addr will be used when RD_DATA_VLD is low (we wait in case both ports in the buffer are in use)
        case rd_fsm_pst is
            when S_IDLE     =>
                if (HDR_BUFF_SRC_RDY = '1' then  
                    -- Accept header 
                    if ENABLED_CHANS(to_integer(unsigned(HDR_BUFF_CHAN))) = '1') then

                        -- Decide if incoming packet fits into USR_MFB
                        if (dma_hdr_frame_length_v > (USR_MFB_DATA'length /8)) then
                            pkt_dispatch_nst <= S_WR_BUFF;

                            -- Save address and number of bytes in packet
                            next_addr_d   <= dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range) + (USR_MFB_DATA'length /8);
                            byte_cntr_d   <= resize(dma_hdr_frame_length_v, byte_cntr_d'length) - (USR_MFB_DATA'length /8);                            
                        else
                            pkt_dispatch_nst <= S_RD_BUFF;
                        end if;

                        BUFF_RD_ADDR     <= std_logic_vector(dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range));
                        BUFF_RD_EN       <= '1';

                        current_addr_d   <= std_logic_vector(dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range));

                    -- Discard DMA header
                    else
                        HDR_BUFF_DST_RDY    <= '1';
                    end if;
                end if;
            
            -- Write data to the FIFO based on DMA_HDR information
            when S_WR_BUFF  =>
                -- It depends on the FIFO whether we are ready or not 
                -- The FIFO should have enough space for a packet of maximum size
                BUFF_RD_EN           <= '1';


                if (BUFF_RD_DATA_VLD = '1') then
                    BUFF_RD_ADDR    <= next_addr_q;
                    current_addr_d  <= next_addr_q;

                    -- With every received MFB word, the address and remaining data are updated
                    next_addr_d     <= next_addr_q + (USR_MFB_DATA'length /8);
                    byte_cntr_d     <= byte_cntr_q - (USR_MFB_DATA'length /8);

                    if (byte_cntr_q <= (USR_MFB_DATA'length /8)) then
                        BUFF_RD_EN       <= '0';
                        pkt_dispatch_nst <= S_RD_BUFF;
                    end if;
                else 
                    BUFF_RD_ADDR    <= current_addr_q;
                end if;

            -- Wait until FIFO is empty (USR_MFB took all the data)
            when S_RD_BUFF  =>
                if disp_buffer_empty = '1' then 
                    pkt_dispatch_nst <= S_IDLE;
                end if;

        end case;
    end process;

    -- =============================================================================================
    -- BUFFER - Input FIFO
    -- =============================================================================================
    dispatcher_input_fifo_i: entity work.MVB_FIFOX
        generic map (
            ITEMS               => 1
            ITEM_WIDTH          => MFB_LENGTH,
            FIFO_DEPTH          => (PKT_SIZE_MAX + 1)/(MFB_LENGTH/8),
            DEVICE              => DEVICE,
            ALMOST_FULL_OFFSET  => 0,
            ALMOST_EMPTY_OFFSET => 0,
            
            -- Could be usefull fro one region in PCIe transaction
            FAKE_FIFO           => FALSE
        )
        port map(
            CLK   => CLK,
            RESET => RESET,

            RX_DATA     => BUFF_RD_DATA,
            RX_VLD      => (others => '1'),
            RX_SRC_RDY  => BUFF_RD_DATA_VLD,
            RX_DST_RDY  => open

            TX_DATA     => disp_buffer_data,
            TX_VLD      => disp_buffer_vld,
            TX_SRC_RDY  => disp_buffer_src_rdy,
            TX_DST_RDY  => disp_buffer_dst_rdy,

            STATUS      => disp_buffer_status,
            AFULL       => disp_buffer_afull,
            AEMPTY      => disp_buffer_empty
        );

    -- =============================================================================================
    -- OUT_FSM - Output State Machine
    -- =============================================================================================
    -- TODO: SOF, EOF, EOF_POS
    out_fsm_reg_p : process (CLK) is
    begin
        if (rising_edge(CLK)) then
            if (RESET = '1') then
                out_fsm_pst     <= S_IDLE;
                out_meta_chan_q <= (others => '0');
                out_meta_data_q <= (others => '0');
            elsif (USR_MFB_DST_RDY = '1') then
                out_fsm_pst     <= out_fsm_nst;
                out_meta_chan_q <= out_meta_chan_d;
                out_meta_data_q <= out_meta_data_d;
            end if;
        end if;
    end process

    out_fsm_nst_logic_p : process (all) is
    begin
        out_fsm_nst     <= out_fsm_pst;
        out_meta_chan_d <= out_meta_chan_q;
        out_meta_data_d <= out_meta_data_q;

        disp_buffer_dst_rdy <= '0';

        USR_MFB_SOF     <= (others => '0');
        USR_MFB_EOF     <= (others => '0');
        USR_MFB_SOF_POS <= (others => '0');
        USR_MFB_EOF_POS <= (others => '0');

        USR_MFB_META_HDR_META   <= resize(out_meta_data_q(DMA_USR_METADATA),HDR_META_WIDTH);
        USR_MFB_META_CHAN       <= out_meta_chan_q;
        USR_MFB_META_PKT_SIZE   <= out_meta_data_q(DMA_FRAME_LENGTH)(USR_MFB_META_PKT_SIZE'range);


        case rd_fsm_pst is
            when S_IDLE         =>
                if (HDR_BUFF_SRC_RDY = '1' and ENABLED_CHANS(to_integer(unsigned(HDR_BUFF_CHAN))) = '1') then
                    out_fsm_nst     <= S_WR_BUFF_OUT;

                    -- Store meta signals for later usage
                    out_meta_chan_d <= HDR_BUFF_CHAN;
                    out_meta_data_d <= HDR_BUFF_DATA;
                end if;
                
            -- Wait until the FIFO is written to
            when S_WR_BUFF_OUT  =>
                if (rd_fsm_pst = S_RD_BUFF) then
                    out_fsm_nst <= S_RD_BUFF_OUT;
                end if;

            -- Set the SOF of the first word
            -- Cases: Packet will fit into MFB word (SOF and EOF are set at the same time)
            --        Packet is larger than MFB word (Two words)
            --        Packet is larger than MFB word (More than two words)
            when S_SOF          => 
                

            -- When the FIFO
            when S_RD_BUFF_OUT  =>
                disp_buffer_dst_rdy <= USR_MFB_DST_RDY;
                USR_MFB_SRC_RDY     <= '1';
                if (unsigned(disp_buffer_status) = 1) then
                    out_fsm_nst <= S_EOF;
                end if;

            -- Set the EOF of the last word
            when S_EOF          =>
                disp_buffer_dst_rdy <= USR_MFB_DST_RDY;

        end case;
    end process;
    
    USR_MFB_DATA    <= BUFF_RD_DATA when mfb_dst_rdy_reg = '1' else buff_rd_data_reg;
    USR_MFB_SOF_POS <= (others => '0');











































      
        -- BUFF_RD_CHAN     : out std_logic_vector(log2(CHANNELS) -1 downto 0);
        -- BUFF_RD_DATA     : in  std_logic_vector(MFB_REGIONS*MFB_REGION_SIZE*MFB_BLOCK_SIZE*MFB_ITEM_WIDTH-1 downto 0);
        -- BUFF_RD_ADDR     : out std_logic_vector(DATA_POINTER_WIDTH -1 downto 0);
        -- BUFF_RD_EN       : out std_logic;
        -- -- Multiple region support
        -- BUFF_RD_DATA_VLD : in  std_logic;


    -- Register with reset and Enable signal in form DST_RDY
    pkt_dispatch_fsm_reg_p : process (CLK) is
    begin
        if (rising_edge(CLK)) then
            if (RESET = '1') then
                pkt_dispatch_pst <= S_IDLE;
                addr_cntr_pst    <= (others => '0');
                byte_cntr_pst    <= (others => '0');
            elsif (USR_MFB_DST_RDY = '1') then
                pkt_dispatch_pst <= pkt_dispatch_nst;
                addr_cntr_pst    <= addr_cntr_nst;
                byte_cntr_pst    <= byte_cntr_nst;
            end if;
        end if;
    end process;

    -- =============================================================================================
    -- FSM - Transition logic
    -- =============================================================================================
    pkt_dispatch_fsm_nst_logic_p : process (all) is
        variable dma_hdr_frame_ptr_v    : unsigned(DMA_FRAME_PTR_W -1 downto 0);
        variable dma_hdr_frame_length_v : unsigned(DMA_FRAME_LENGTH_W -1 downto 0);
    begin
        pkt_dispatch_nst <= pkt_dispatch_pst;

        dma_hdr_frame_ptr_v    := unsigned(HDR_BUFF_DATA(DMA_FRAME_PTR));
        dma_hdr_frame_length_v := unsigned(HDR_BUFF_DATA(DMA_FRAME_LENGTH));

        case pkt_dispatch_pst is
            when S_IDLE =>
                -- SRC_RDY of HDR_FIFO (The packet is ready) and ENABLED_CHANS from software manager
                if (HDR_BUFF_SRC_RDY = '1' and ENABLED_CHANS(to_integer(unsigned(HDR_BUFF_CHAN))) = '1') then
                    -- Decide if incoming packet fits into USR_MFB
                    if (dma_hdr_frame_length_v > (USR_MFB_DATA'length /8)) then
                        pkt_dispatch_nst <= S_PKT_MIDDLE;
                    else
                        pkt_dispatch_nst <= S_UPDATE_STATUS;
                    end if;
                end if;

            when S_PKT_MIDDLE =>
                if (byte_cntr_pst <= (USR_MFB_DATA'length /8)) then
                    pkt_dispatch_nst <= S_UPDATE_STATUS;
                end if;

            when S_UPDATE_STATUS =>
                pkt_dispatch_nst <= S_IDLE;
        end case;
    end process;

    -- =============================================================================================
    -- FSM - output logic
    -- =============================================================================================
    pkt_dispatch_fsm_output_logic_p : process (all) is
        variable dma_hdr_frame_ptr_v    : unsigned(DMA_FRAME_PTR_W -1 downto 0);
        variable dma_hdr_frame_length_v : unsigned(DMA_FRAME_LENGTH_W -1 downto 0);
    begin
        addr_cntr_nst <= addr_cntr_pst;
        byte_cntr_nst <= byte_cntr_pst;

        disp_fsm_mfb_sof     <= (others => '0');
        disp_fsm_mfb_eof     <= (others => '0');
        disp_fsm_mfb_eof_pos <= (others => '0');
        disp_fsm_mfb_src_rdy <= '0';

        HDR_BUFF_DST_RDY <= '0';

        BUFF_RD_ADDR <= std_logic_vector(addr_cntr_pst);
        BUFF_RD_EN   <= '0';

        PKT_SENT_INC <= '0';
        UPD_HDP_EN   <= '0';
        UPD_HHP_EN   <= '0';

        dma_hdr_frame_ptr_v    := unsigned(HDR_BUFF_DATA(DMA_FRAME_PTR));
        dma_hdr_frame_length_v := unsigned(HDR_BUFF_DATA(DMA_FRAME_LENGTH));

        case pkt_dispatch_pst is
            when S_IDLE =>

                if (HDR_BUFF_SRC_RDY = '1') then
                    -- Stays in S_IDLE
                    if (ENABLED_CHANS(to_integer(unsigned(HDR_BUFF_CHAN))) = '0') then
                        -- Does this mean discard? - I mean, the header will be lost, but the data will stay in the buffer (not that problematic is it)
                        HDR_BUFF_DST_RDY <= USR_MFB_DST_RDY;
                    -- Moves to S_PKT_MIDDLE or S_UPDATE_STATUS based on the dma_hdr_frame_length_v
                    -- If dma_hdr_frame_length_v < USR_MFB_DATA'length /8 => S_UPDATE_STATUS
                    -- else => S_PKT_MIDDLE
                    else
                        -- This will happen always whenever you move
                        disp_fsm_mfb_sof     <= "1";
                        disp_fsm_mfb_src_rdy <= '1';

                        BUFF_RD_ADDR <= std_logic_vector(dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range));
                        BUFF_RD_EN   <= '1';

                        -- When the packet, according to it's length, fits in the output word, then
                        -- assign EOF and do not count next address for the reading.

                        -- The data fits => eof is assigned and the current address is saved
                        -- else saved address is the current one extended by MFB_DATA'length
                        -- The byte counter is lowered by MFB_DATA'length
                        if (dma_hdr_frame_length_v <= (USR_MFB_DATA'length /8)) then
                            addr_cntr_nst        <= dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range);
                            disp_fsm_mfb_eof     <= "1";
                            -- take only the lower bits from the frame length
                            disp_fsm_mfb_eof_pos <= std_logic_vector(dma_hdr_frame_length_v(USR_MFB_EOF_POS'range) - 1);
                        else
                            addr_cntr_nst <= dma_hdr_frame_ptr_v(BUFF_RD_ADDR'range) + (USR_MFB_DATA'length /8);
                            byte_cntr_nst <= resize(dma_hdr_frame_length_v, byte_cntr_nst'length) - (USR_MFB_DATA'length /8);
                        end if;
                    end if;
                end if;

            when S_PKT_MIDDLE =>
                -- if (byte_cntr_pst <= (USR_MFB_DATA'length /8)) then
                -- Move condition (moving to S_UPDATE_STATUS)

                -- Since it's in middle of the packet it's doing the usuall
                    -- Increment reading address
                    -- lower byte_cntr
                    -- sets src rdy 
                    -- and read new data
                addr_cntr_nst        <= addr_cntr_pst + (USR_MFB_DATA'length /8);
                byte_cntr_nst        <= byte_cntr_pst - (USR_MFB_DATA'length /8);
                disp_fsm_mfb_src_rdy <= '1';
                BUFF_RD_EN           <= '1';

                -- This happens when it moves
                -- So sets EOF and EOF_POS
                if (byte_cntr_pst <= (USR_MFB_DATA'length /8)) then
                    disp_fsm_mfb_eof     <= "1";
                    disp_fsm_mfb_eof_pos <= std_logic_vector(dma_hdr_frame_length_v(USR_MFB_EOF_POS'range) - 1);
                end if;

            -- End of the packet
            -- This could be set when the FIFO is empty (or the data we need are read)
            when S_UPDATE_STATUS =>
                -- from this state we move right to S_IDLE
                -- The machine is updated with USR_MFB_DST_RDY
                -- so these will be in one the moment it moves to S_IDLE
                -- Header buffer
                HDR_BUFF_DST_RDY <= USR_MFB_DST_RDY;
                -- Software manager
                PKT_SENT_INC     <= USR_MFB_DST_RDY;
                UPD_HDP_EN       <= USR_MFB_DST_RDY;
                UPD_HHP_EN       <= USR_MFB_DST_RDY;
        end case;
    end process;

    -- This part can be independent

    -- PCIe buffer
    BUFF_RD_CHAN <= HDR_BUFF_CHAN;

    -- Software manager
    PKT_SENT_CHAN  <= HDR_BUFF_CHAN;
    PKT_SENT_BYTES <= HDR_BUFF_DATA(DMA_FRAME_LENGTH)(PKT_SENT_BYTES'range);

    -- Some mask I don't know what it's doing 
    -- Frame lenght?
    --      0000 0000 0001 1111 - what?
    -- not: 1111 1111 1110 0000 
    fr_len_round_up_msk <= not to_unsigned(31,16);
    fr_len_rounded <= (unsigned(HDR_BUFF_DATA(DMA_FRAME_LENGTH)) + 31) and fr_len_round_up_msk;
    -- DMA_FRAM_LENGTH [15:0]- Total length of transmitted packet in bytes

    UPD_HDP_CHAN <= HDR_BUFF_CHAN;
    UPD_HDP_DATA <= std_logic_vector(resize(fr_len_rounded + unsigned(HDR_BUFF_DATA(DMA_FRAME_PTR)), DATA_POINTER_WIDTH));
    UPD_HHP_CHAN <= HDR_BUFF_CHAN;
    UPD_HHP_DATA <= std_logic_vector(unsigned(HDR_BUFF_ADDR(1 + DMA_HDR_POINTER_WIDTH -1 downto 1)) + 1);
    
    -- =============================================================================================
    -- Output logic
    -- =============================================================================================
    -- This process delays the set of all output MFB signals because the data come from the data
    -- buffer one clock cycle after the address and enable signal have been set.

    -- Interesting
    out_delay_reg_p : process (CLK) is
    begin
        if (rising_edge(CLK)) then
            if (RESET = '1') then
                USR_MFB_SOF     <= (others => '0');
                USR_MFB_EOF     <= (others => '0');
                USR_MFB_EOF_POS <= (others => '0');
                USR_MFB_SRC_RDY <= '0';
            elsif (USR_MFB_DST_RDY = '1') then
                USR_MFB_SOF     <= disp_fsm_mfb_sof;
                USR_MFB_EOF     <= disp_fsm_mfb_eof;
                USR_MFB_EOF_POS <= disp_fsm_mfb_eof_pos;
                USR_MFB_SRC_RDY <= disp_fsm_mfb_src_rdy;
            end if;
        end if;
    end process;

    USR_MFB_META_HDR_META <= resize(HDR_BUFF_DATA(DMA_USR_METADATA),HDR_META_WIDTH);
    USR_MFB_META_CHAN     <= HDR_BUFF_CHAN;
    USR_MFB_META_PKT_SIZE <= HDR_BUFF_DATA(DMA_FRAME_LENGTH)(USR_MFB_META_PKT_SIZE'range);

    USR_MFB_DATA    <= BUFF_RD_DATA when mfb_dst_rdy_reg = '1' else buff_rd_data_reg;
    USR_MFB_SOF_POS <= (others => '0');

    mfb_dst_rdy_reg_p : process (CLK) is
    begin
        if (rising_edge(CLK)) then
            mfb_dst_rdy_reg <= USR_MFB_DST_RDY;
        end if;
    end process;

    buff_rd_data_reg_p : process (CLK) is
    begin
        if (rising_edge(CLK)) then
            if (RESET = '1') then
                buff_rd_data_reg <= (others => '0');
            elsif (mfb_dst_rdy_reg = '1') then
                buff_rd_data_reg <= BUFF_RD_DATA;
            end if;
        end if;
    end process;
end architecture;
