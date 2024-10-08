-- testbench.vhd: Testbench for merger from n inputs to m outputs
-- Copyright (C) 2018 CESNET
-- Author(s): Jan Kubalek <xkubal11@stud.fit.vutbr.cz>
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- $Id$
--

library IEEE;

use IEEE.std_logic_1164.all;
use ieee.math_real.all;
use work.math_pack.all;
use ieee.numeric_std.all;
use work.type_pack.all;
use std.env.stop;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------

entity testbench is
end entity testbench;

-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------

architecture behavioral of testbench is

   -- Constants declaration ---------------------------------------------------

   -- Synchronization
   constant C_CLK_PER            : time := 5.0 ns;
   constant C_RST_TIME           : time := 10 * C_CLK_PER;
   constant VER_LENGTH           : natural := 10000;

   constant DATA_WIDTH           : integer := 8;
   constant ITEMS                : integer := 4;
   constant QUICK_RESET_EN       : boolean := true;
   constant RESET_VAL            : integer := 42;
   constant READ_PORTS           : integer := 8;
   constant OPERATORS            : integer := 3;
   constant OPERATIONS           : integer := 2;

   -- Signals declaration -----------------------------------------------------

   -- Synchronization
   signal clk                                : std_logic;
   signal clk2                               : std_logic;
   signal rst                                : std_logic := '1';

   signal op_item_sel    : slv_array_t(OPERATORS-1 downto 0)(log2(ITEMS)-1 downto 0);
   signal op_operations  : slv_array_t(OPERATORS-1 downto 0)(OPERATIONS-1 downto 0);
   signal op_in_sel      : slv_array_t(OPERATORS-1 downto 0)(log2(ITEMS)-1 downto 0);
   signal op_in_ops      : slv_array_t(OPERATORS-1 downto 0)(OPERATIONS-1 downto 0);
   signal op_in_data     : slv_array_t(OPERATORS-1 downto 0)(DATA_WIDTH-1 downto 0);
   signal op_out_data    : slv_array_t(OPERATORS-1 downto 0)(DATA_WIDTH-1 downto 0);
   signal read_addr      : slv_array_t(READ_PORTS-1 downto 0)(log2(ITEMS)-1 downto 0) := (others => (others => '0'));
   signal read_addr_reg0 : slv_array_t(READ_PORTS-1 downto 0)(log2(ITEMS)-1 downto 0) := (others => (others => '0'));
   signal read_addr_reg1 : slv_array_t(READ_PORTS-1 downto 0)(log2(ITEMS)-1 downto 0) := (others => (others => '0'));
   signal read_data      : slv_array_t(READ_PORTS-1 downto 0)(DATA_WIDTH-1 downto 0);

   signal TEST_STATUS    : std_logic := '1';

   function get_init_mem return slv_array_t is
      variable mem : slv_array_t(ITEMS-1 downto 0)(DATA_WIDTH-1 downto 0);
   begin
      mem := (others => (others => '0'));
      if (QUICK_RESET_EN=true) then
         mem := (others => std_logic_vector(to_unsigned(RESET_VAL,DATA_WIDTH)));
      end if;
      return mem;
   end function;

   signal fake_mem : slv_array_t(ITEMS-1 downto 0)(DATA_WIDTH-1 downto 0) := get_init_mem;
   signal op_ops_s : slv_array_t(ITEMS-1 downto 0)(OPERATIONS-1 downto 0) := (others => (others => '0'));
-- ----------------------------------------------------------------------------
--                            Architecture body
-- ----------------------------------------------------------------------------

begin

   -- -------------------------------------------------------------------------
   -- CROSSBAR SCHEDULER planner
   -- -------------------------------------------------------------------------

   uut: entity work.n_loop_op_pro
   generic map(
      DATA_WIDTH     => DATA_WIDTH,
      ITEMS          => ITEMS,
      QUICK_RESET_EN => QUICK_RESET_EN,
      RESET_VAL      => RESET_VAL,
      READ_PORTS     => READ_PORTS,

      OPERATORS      => OPERATORS,
      OPERATIONS     => OPERATIONS
   )
   port map(
      CLK          => clk,
      CLK2         => clk2,
      RESET        => rst,

      OP_ITEM_SEL   => op_item_sel,
      OP_OPERATIONS => op_operations,
      OP_IN_SEL     => op_in_sel,
      OP_IN_OPS     => op_in_ops,
      OP_IN_DATA    => op_in_data,
      OP_OUT_DATA   => op_out_data,
      READ_ADDR     => read_addr,
      READ_DATA     => read_data
   );

   -- -------------------------------------------------------------------------
   --                        clk and reset generators
   -- -------------------------------------------------------------------------

   -- generating clk
   clk_gen: process
   begin
      for i in 0 to VER_LENGTH-1 loop
         clk <= '1';
         wait for C_CLK_PER / 2;
         clk <= '0';
         wait for C_CLK_PER / 2;
      end loop;
      report "Verification finished successfully!";
      stop;
      wait;
   end process clk_gen;
   clk2_gen: process
   begin
      clk2 <= '1';
      wait for C_CLK_PER / 2 / 2;
      clk2 <= '0';
      wait for C_CLK_PER / 2 / 2;
   end process clk2_gen;

   -- generating reset
   rst_gen: process
   begin
      rst <= '1';
      wait for C_RST_TIME;
      wait until rising_edge(clk);
      rst <= '0';
      wait;
   end process rst_gen;

   -- -------------------------------------------------------------------------

   read_addr_reg : process (CLK)
   begin
      if (rising_edge(CLK)) then
         read_addr_reg0 <= read_addr;
         read_addr_reg1 <= read_addr_reg0;
      end if;
   end process;

   op: process (op_in_data,op_in_ops)
      variable data : slv_array_t(0 to OPERATORS-1)(DATA_WIDTH-1 downto 0);
   begin
      -- operators
      for i in 0 to OPERATORS-1 loop
         data(i) := op_in_data(i);
         op_out_data(i) <= (others => 'X');
         for e in 0 to OPERATIONS-1 loop
            if (op_in_ops(i)(e)='1') then
               data(i) := std_logic_vector(unsigned(data(i))+e+1);
               op_out_data(i) <= data(i);
            end if;
         end loop;
      end loop;
   end process;

   tb: process
      variable seed1 : positive := 42;
      variable seed2 : positive := 42;

      variable rand  : real;
      variable X     : integer;

      variable op_ops : slv_array_t(0 to ITEMS-1)(0 to OPERATIONS-1) := (others => (others => '0'));
      variable op_ops1 : slv_array_t(0 to ITEMS-1)(0 to OPERATIONS-1) := (others => (others => '0'));

      variable fake_mem_v : slv_array_t(0 to ITEMS-1)(DATA_WIDTH-1 downto 0) := get_init_mem;
   begin
--      wait for 1 ns;
      -- Wait for the reset
      if (rst='1') then
         wait until rst='0';
      end if;

      wait until rising_edge(CLK);

      -- fail
      assert (TEST_STATUS/='0')
         report "INCORRECT VALUE READ FROM DUT!"
         severity failure;

      -- input gen
      for i in 0 to OPERATORS-1 loop
         for e in 0 to OPERATIONS-1 loop
            op_operations(i)(e) <= '0';
            uniform(seed1,seed2,rand);
            X := integer(rand*real(2));
            if (X=0) then
               op_operations(i)(e) <= '1';
            end if;
         end loop;
         uniform(seed1,seed2,rand);
         X := integer(rand*real(ITEMS));
         op_item_sel(i) <= std_logic_vector(to_unsigned(X,log2(ITEMS)));
      end loop;

      -- read gen
      for i in 0 to READ_PORTS-1 loop
         uniform(seed1,seed2,rand);
         X := integer(rand*real(ITEMS));
         read_addr(i) <= std_logic_vector(to_unsigned(X,log2(ITEMS)));
      end loop;

      -- fake mem op
      for i in 0 to ITEMS-1 loop
         for e in 0 to OPERATIONS-1 loop
            if (op_ops_s(i)(e)='1') then
               fake_mem_v(i) := std_logic_vector(unsigned(fake_mem_v(i))+e+1);
            end if;
         end loop;
      end loop;
      fake_mem <= fake_mem_v;

      op_ops := (others => (others => '0'));
      for i in 0 to OPERATORS-1 loop
         op_ops(to_integer(unsigned(op_item_sel(i)))) := op_ops(to_integer(unsigned(op_item_sel(i)))) or op_operations(i);
      end loop;

      op_ops_s <= op_ops1;
      op_ops1  := op_ops;

      -- check
      TEST_STATUS <= '1';
      for i in 0 to READ_PORTS-1 loop
         if (read_data(i)/=fake_mem(to_integer(unsigned(read_addr_reg1(i))))) then
            TEST_STATUS <= '0';
            report "data: " & integer'image(to_integer(unsigned(read_data(i)))) & " addr_reg1:" & integer'image(to_integer(unsigned(read_addr_reg1(i)))) & " fake_mem_data:" & integer'image(to_integer(unsigned(fake_mem(to_integer(unsigned(read_addr_reg1(i))))))) severity error;

         end if;
      end loop;
   end process;

end architecture behavioral;
