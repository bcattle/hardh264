-------------------------------------------------------------------------
-- Test for H264 CAVLC - VHDL
-- 
-- Written by Andy Henson
-- Copyright (c) 2008 Zexia Access Ltd
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--    * Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--    * Neither the name of the Zexia Access Ltd nor the
--      names of its contributors may be used to endorse or promote products
--      derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY ZEXIA ACCESS LTD ``AS IS'' AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL ZEXIA ACCESS LTD OR ANDY HENSON BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-------------------------------------------------------------------------

-- TEST stuff
-- reads in array from VHDL code here
-- outputs stuff to stdout

library IEEE;
use ieee.std_logic_1164.ALL;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.ALL;
use std.textio.all;
use work.h264.all;

entity test is
end test;

architecture cavlc_test of test is
	--
	constant STATE_IDLE   : std_logic_vector(2 downto 0) := b"000";
	constant STATE_READ   : std_logic_vector(2 downto 0) := b"001";
	constant STATE_CTOKEN : std_logic_vector(2 downto 0) := b"010";
	constant STATE_T1SIGN : std_logic_vector(2 downto 0) := b"011";
	constant STATE_COEFFS : std_logic_vector(2 downto 0) := b"100";
	constant STATE_TZEROS : std_logic_vector(2 downto 0) := b"101";
	constant STATE_RUNBF  : std_logic_vector(2 downto 0) := b"110";
	signal CLK : std_logic := '0';			--clock
	signal CLK2 : std_logic;				--2x clock
	signal ENABLE : std_logic := '0';				--values transfered only when this is 1
	signal READY : std_logic;				--values transfered only when this is 1
	signal VIN : std_logic_vector(11 downto 0) := x"000";		--12bits max (+/- 2048)
	signal NIN : std_logic_vector(4 downto 0);	--N coeffs nearby mb
	signal VE : std_logic_vector(27 downto 0) := (others=>'0');
	signal VL : std_logic_vector(4 downto 0) := (others=>'0');
	signal VALID : std_logic := '0';	-- enable delayed to same as VE/VL
	signal XSTATE : std_logic_vector(2 downto 0) := (others=>'0');
	signal STATE : std_logic_vector(2 downto 0) := (others=>'0');
	signal NOUT : std_logic_vector(4 downto 0);
	signal VIN1 : std_logic_vector(11 downto 0) := x"000";
	signal VIN2 : std_logic_vector(11 downto 0) := x"000";
	signal EN1: std_logic;
	signal EN2: std_logic;
	signal SIN : std_logic := '0';			--stream/strobe flag, copied to VS
	signal VS : std_logic;					--stream/strobe flag sync'd with VL/VE
	--
begin
	uut : h264cavlc
	port map (
		CLK => clk,
		CLK2 => clk2,
		ENABLE => enable,
		READY => ready,
		VIN => vin,
		NIN => nin,
		SIN => sin,
		VS => vs,
		VE => ve,
		VL => vl,
		VALID => valid,
		XSTATE => xstate,
		NOUT => nout
	);
	--
process		--generate CLK2, 100MHz will do for this sim, and CLK at 50MHz
begin
	CLK2 <= '0';
	wait for 5 ns;
	CLK2 <= '1';
	CLK <= not CLK;
	wait for 5 ns;
end process;
	--
process	--data input
	type Tdata is array(0 to 15) of integer;
	type TdataArray is array(0 to 6) of Tdata;
	variable index : integer;
	variable data : TdataArray := (
	(	0, 3,-1, 0,
		0,-1, 1, 0,
		1, 0, 0, 0,
		0, 0, 0, 0
	),
	(	-2,4, 0,-1,
		3, 0, 0, 0,
		-3,0, 0, 0,
		0, 0, 0, 0
	),
	(	-2,4, 2,-1,
		3, 1, 1, 1,
		-3,1, 1, 2,
		-1,-1, 1, 1
	),
	(	-2,4, 0,-1,
		3, 0, 0, 2,
		-3,0, 0, 0,
		0, 0, 0, 0
	),
	(	4, 3, 3, 3,
		3, 2, 3, 2,
		-3,1, 2, 2,
		1, 2, 3, 2
	),
	--(	14, 13,  23, 3,
	--	33, 22,-449, 2,
	--	-3, 8, 1022,54,
	--	 6, 5,  -12, 1
	--),
	(	1, 1, 1, 1,
		1, 1, 1, 1,
		1, 1, 1, 1,
		0, 0, 0, 0
	),
	(	0, 0, 0, 0,
		0, 0, 0, 0,
		0, 0, 0, 0,
		0, 0, 0, 0
	));
begin
	enable <= '0';
	for index in 0 to 6 loop
		wait until READY='1';
		wait until rising_edge(clk2); enable <= '1';
		if index=5 then SIN <= '1'; end if;	--DEBUG, test this feature
		nin <= b"00000";		      vin <= CONV_STD_LOGIC_VECTOR(data(index)(15),12);	--reverse zigzag order
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(14),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(11),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(7),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(10),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(13),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(12),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(9),12);
	 	wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(6),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(3),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(2),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(5),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(8),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(4),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(1),12);
		wait until rising_edge(clk2); vin <= CONV_STD_LOGIC_VECTOR(data(index)(0),12);
		wait until rising_edge(clk2); enable <= '0';
	end loop;
	wait for 1 ms;
end process;
	--
process(CLK2)
begin
	en2 <= enable;
	vin2 <= vin;
	en1 <= en2;
	vin1 <= vin2;
end process;
	--
process(CLK)		--DEBUG STUFF ONLY
	variable sout : line;
	variable n : integer;
begin
	if rising_edge(CLK) then
		state <= xstate;	--delay xstate by 1clk so it lines up with VE/VL
		if en1='1' or en2='1' then
			if SIN='0' then
				write(sout,"IN");
			else
				write(sout,"in");
			end if;
			if en1='1' then
				if vin1(11)='0' then
					write(sout," ");
					write(sout,conv_integer(vin1));	--probably single digit
				else
					write(sout,"-");
					write(sout,conv_integer(x"000"-vin1));	--probably single digit
				end if;
			else
				write(sout,"  ");
			end if;
			if en2='1' then
				if vin2(11)='0' then
					write(sout," ");
					write(sout,conv_integer(vin2));	--probably single digit
				else
					write(sout,"-");
					write(sout,conv_integer(x"000"-vin2));	--probably single digit
				end if;
			else
				write(sout,"  ");
			end if;
			write(sout,"  ");
		else
			write(sout,"--      ");
		end if;
		--DEBUG OUTPUT INTERNAL STATES
		if state = STATE_IDLE then
			write(sout,"IDLE  ");
		elsif state = STATE_CTOKEN then
			write(sout,"CTOKEN");
		elsif state = STATE_T1SIGN then
			write(sout,"T1SIGN");
		elsif state = STATE_COEFFS then
			write(sout,"COEFFS");
		elsif state = STATE_TZEROS then
			write(sout,"TZEROS");
		elsif state = STATE_RUNBF then
			write(sout,"RUNBF ");
		else
			write(sout,"????  ");
		end if;
		if VALID='1' then
			--output on VL/VE
			if VS='0' then
				write(sout," OUT ");
			else
				write(sout," out ");
			end if;
			n := conv_integer(vl);
			if n<10 then write(sout," "); end if;
			write(sout,n);
			write(sout," ");
			for i in n-1 downto 0 loop
				if i > n then
					write(sout,'.');
				else
					write(sout,conv_integer(ve(i)));
				end if;
			end loop;
		end if;
		writeline(output,sout);
	end if;
end process;
	--
end cavlc_test;
