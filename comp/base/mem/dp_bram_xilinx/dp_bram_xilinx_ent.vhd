--!
--! \file dp_bram_xilinx_ent.vhd
--! \brief Dual port BRAM for Xilinx devices, entity
--! \author Pavel Benáček <benacek@cesnet.cz>
--! \author Jan Kučera <jan.kucera@cesnet.cz>
--! \date 2018
--!
--! \section License
--!
--! Copyright (C) 2018 CESNET
--!
--! SPDX-License-Identifier: BSD-3-Clause
--!

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;


--! \brief Entity of dual port Xilinx BRAM declaration
entity DP_BRAM_XILINX is
   generic (
      --! Select target device "VIRTEX5", "VIRTEX6", "7SERIES", "SPARTAN6", "ULTRASCALE".
      DEVICE : string := "ULTRASCALE";

      --! A read operation is implicitly performed to address ADDR[A|B] combinatorially,
      --! regardless of RE[A|B] inputs. The data output is than registered each CLK[A|B]
      --! cycle that EN[A|B] is asserted.

      --! Input/output data width.
      DATA_WIDTH     : integer := 108;

      --! Address bus width.
      ADDRESS_WIDTH  : integer := 10;

      --! What operation will be performed first when both WE and RE are active?
      --! - WRITE_FIRST (default) | READ_FIRST | NO_CHANGE.
      WRITE_MODE_A   : string := "WRITE_FIRST";
      WRITE_MODE_B   : string := "WRITE_FIRST";

      --! Enable output register.
      ENABLE_OUT_REG : boolean := true;

      --! Asserts will report reading of uinitialized items from memory in verification.
      DEBUG_ASSERT_UNINITIALIZED : boolean := false;

      --! Clocking mode: "common_clock" (sync), "independent_clock" (async).
      --! - Only for ULTRASCALE devices (DEVICE = "ULTRASCALE")!
      --! - When the attribute “CLOCKING_MODE” is set to “common_clock”, all read/write operations
      --! - to memory through port A and port B are performed on CLKA. If this attribute is set to
      --! - “independent_clock”, then read/write operations through port A are performed based on
      --! - CLKA, and read/write operations through port B are performed based on CLKB.
      CLOCKING_MODE  : string := "common_clock";

      --! Block RAM type, 18Kb or 36Kb blocks.
      --! - Only for non ULTRASCALE devices (DEVICE /= "ULTRASCALE")!
      BRAM_TYPE      : integer := 36
   );
   port (
      --! Port A clock.
      CLKA : in std_logic;
      --! Output registers synchronous reset for Port A.
      RSTA : in std_logic := '0';
      --! Port A enable.
      PIPE_ENA : in std_logic;
      --! Port A read enable (implicit when PIPE_ENA = '1').
      REA : in std_logic := '1';
      --! Port A write enable.
      WEA : in std_logic;
      --! Port A address.
      ADDRA : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      --! Port A write data.
      DIA : in std_logic_vector(DATA_WIDTH-1 downto 0);
      --! Port A output data.
      DOA : out std_logic_vector(DATA_WIDTH-1 downto 0);
      --! Port A output data validity.
      DOA_DV : out std_logic;

      --! Port B clock.
      CLKB : in std_logic;
      --! Output registers synchronous reset for Port B.
      RSTB : in std_logic := '0';
      --! Port B enable.
      PIPE_ENB : in std_logic;
      --! Port B read enable (implicit when PIPE_ENB = '1').
      REB : in std_logic := '1';
      --! Port B write enable.
      WEB : in std_logic;
      --! Port B address.
      ADDRB : in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
      --! Port B write data.
      DIB : in std_logic_vector(DATA_WIDTH-1 downto 0);
      --! Port B output data.
      DOB : out std_logic_vector(DATA_WIDTH-1 downto 0);
      --! Port B output data validity.
      DOB_DV : out std_logic
   );
end entity;
