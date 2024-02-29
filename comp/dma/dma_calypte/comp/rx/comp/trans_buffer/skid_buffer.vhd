-- skid_buffer.vhd: a primitive FIFO-like buffer from registers
-- Copyright (C) 2024 CESNET z.s.p.o.
-- Author(s): Vladislav Valek  <xvalek14@vutbr.cz>
--
-- SPDX-License-Identifier: BSD-3-Clause

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.type_pack.all;
use work.math_pack.all;

entity SKID_BUFFER is

    generic (
        DATA_WIDTH : natural := 256;
        STAGES     : natural := 2;
        FAKE_BUFF  : boolean := FALSE);

    port (
        CLK        : in  std_logic;
        RST        : in  std_logic;
        RX_DATA    : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        RX_SRC_RDY : in  std_logic;
        RX_DST_RDY : out std_logic;
        TX_DATA    : out slv_array_t(STAGES-1 downto 0)(DATA_WIDTH -1 downto 0);
        TX_SRC_RDY : out std_logic;
        TX_DST_RDY : in  std_logic);

end entity;

architecture FULL of SKID_BUFFER is

    signal sb_data    : slv_array_t(STAGES downto 0)(RX_DATA'range);
    signal sb_src_rdy : std_logic_vector(STAGES downto 0);
    signal sb_dst_rdy : std_logic_vector(STAGES downto 0);

begin

    fake_buff_g : if (FAKE_BUFF) generate
        TX_DATA(0) <= RX_DATA;
        TX_SRC_RDY <= RX_SRC_RDY;
        RX_DST_RDY <= TX_DST_RDY;
    end generate;

    not_fake_buff_g : if (not FAKE_BUFF) generate

        sb_data(0)    <= RX_DATA;
        sb_src_rdy(0) <= RX_SRC_RDY;
        RX_DST_RDY    <= sb_dst_rdy(0);

        skid_buff_stages_g : for i in 1 to STAGES generate
            skid_buffer_p : process (CLK) is
            begin
                if (rising_edge(CLK)) then
                    if (RST = '1') then
                        sb_src_rdy(i) <= '0';
                    elsif (sb_dst_rdy(i-1) = '1') then
                        sb_data(i)    <= sb_data(i-1);
                        sb_src_rdy(i) <= sb_src_rdy(i-1);
                    end if;
                end if;
            end process;

            sb_dst_rdy(i-1) <= sb_dst_rdy(i) or (not sb_src_rdy(i));
        end generate;

        TX_DATA            <= sb_data(STAGES downto 1);
        TX_SRC_RDY         <= sb_src_rdy(STAGES);
        sb_dst_rdy(STAGES) <= TX_DST_RDY;

    end generate;
end architecture;
