-------------------------------------------------------------------------
-- Test2 for H264 CAVLC - VHDL
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
-- reads in test vectors from stdin (testcoeff.txt)
-- outputs stuff to stdout (should match testcoeff-ref.out)
-- takes about 4ms simulation time to run all
-- modify verbose, below, to enable verbose output (off to match files)

library IEEE;
use ieee.std_logic_1164.ALL;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.ALL;
use std.textio.all;
use work.h264.all;

entity test2 is
end test2;

architecture cavlc_test2 of test2 is
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
	signal VE : std_logic_vector(24 downto 0) := (others=>'0');
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
	variable index : integer;
	variable data : Tdata;
	--
	variable s : line;
	variable sout : line;
	variable c : character;
	variable vali : integer;
	variable n : integer;
begin
	enable <= '0';
	write(sout,"# Test output from VHDL CAVLC_TEST2");
	writeline(output,sout);
	--
	cmd: while not endfile(input) loop
		readline(input,s);
		write(s,' ');	--add space to end
		--write(sout,"READ:");
		--//write(sout,s);
		--writeline(output,sout);
		read(s,c);
		if c = '#' then
			next cmd;
		end if;
		if c /= 'c' then
			write(sout,"ERROR EXPECTING coeff:");
			--//write(sout,s);
			writeline(output,sout);
			next cmd;
		end if;
		read(s,c);--o
		read(s,c);--e
		read(s,c);--f
		read(s,c);--f
		write(sout,"coeff ");
		for i in 0 to 15 loop
			read(s,vali);	--first coeff
			data(i) := vali;
			write(sout,vali);
			write(sout," ");
		end loop;
		writeline(output,sout);
		--
		-- each coeff line is used for 9 tests:
		-- 16 coeff CAVLC with nin=0,2,4,8
		-- 15 coeff CAVLC with nin=0,2,4,8
		-- and a 4 coeff CAVLC
		for nintype in 0 to 8 loop
			if READY='0' then
				wait until READY='1';
			end if;
			wait until rising_edge(clk2);
			if nintype=1 or nintype=5 then
				nin <= b"00010";
			elsif nintype=2 or nintype=6 then
				nin <= b"00100";
			elsif nintype=3 or nintype=7 then
				nin <= b"01000";
			else
				nin <= b"00000";
			end if;
			if nintype < 4 then
				n := 16;
			elsif nintype < 8 then
				n := 15;
			else
				n := 4;
			end if;
			for i in n-1 downto 0 loop
				wait until rising_edge(clk2);
				enable <= '1';
				vin <= CONV_STD_LOGIC_VECTOR(data(i),12);	--reverse order
			end loop;
			wait until rising_edge(clk2);
			enable <= '0';
			wait until rising_edge(clk2);
			wait for 400 ns;	--to output all before next read
		end loop; --nintype
	end loop;	--all input lines
	write(sout,"#end of input");
	writeline(output,sout);
	wait for 1 ms;
	assert false report "DONE" severity ERROR;
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
process(CLK)		--output stage
	variable sout : line;
	variable n : integer;
	variable verbose : boolean := false;
	variable soutb : line;
	variable bb : integer := 8;
begin
	if rising_edge(CLK) then
		state <= xstate;	--delay xstate by 1clk so it lines up with VE/VL
		if verbose and (en1='1' or en2='1' or VALID='1' or state/=STATE_IDLE) then
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
				elsif i > 24 then
					write(sout,0);
				else
					write(sout,conv_integer(ve(i)));
				end if;
			end loop;
		end if;
		writeline(output,sout);
		end if;--verbose
		--
		--add bits to bytes output, display output if RUNBF state (last state)
		--bb count is used to add a space after each 8 bits
		--
		if VALID='1' then
			n := conv_integer(vl);
			for i in n-1 downto 0 loop
				if i>24 then
					write(soutb,0);
				else
					write(soutb,conv_integer(ve(i)));
				end if;
				bb := bb - 1;
				if bb=0 then write(soutb," "); bb:= 8; end if;
			end loop;
		end if;
		if state = STATE_RUNBF and xstate /= STATE_RUNBF then
			--end of processing, output all
			writeline(output,soutb);
			bb := 8;
		end if;
	end if;
end process;
	--
end cavlc_test2;
