-------------------------------------------------------------------------
-- Test for H264 transforms - VHDL
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
-- reads in test vectors from stdin (testresidual.txt)
-- outputs stuff to stdout (should match testresidual-ref.out)
-- takes about 300us simulation time to run all
-- modify slow to give decent gap between each processing
-- uses READY if slow not set

library IEEE;
use ieee.std_logic_1164.ALL;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.ALL;
use std.textio.all;
use work.h264.all;

entity test is
end test;

architecture test_transform of test is
	--
	signal CLK : std_logic := '0';			--clock
	signal CLK2 : std_logic;				--2x clock
	signal READY : std_logic := '0';				--ready for enable when this set
	signal ENABLE : std_logic := '0';				--values transfered only when this is 1
	signal XXIN : std_logic_vector(35 downto 0) := (others => '0');
	signal VALID : std_logic := '0';
	signal YNOUT : std_logic_vector(13 downto 0);
	--
begin
	uut : h264coretransform
	port map (
		CLK => clk2,
		READY => ready,
		ENABLE => enable,
		XXIN => xxin,
		VALID => valid,
		YNOUT => ynout
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
	--
	variable slow : boolean := false;
	--
begin
	enable <= '0';
	write(sout,"# Test output from VHDL TEST_TRANSFORM");
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
		if c /= 'r' then
			write(sout,"ERROR EXPECTING residual");
			--//write(sout,s);
			writeline(output,sout);
			next cmd;
		end if;
		read(s,c);--e
		read(s,c);--s
		read(s,c);--i
		read(s,c);--d
		read(s,c);--u
		read(s,c);--a
		read(s,c);--l
		write(sout,"residual ");
		for i in 0 to 15 loop
			read(s,vali);	--first coeff
			data(i) := vali;
			write(sout,vali);
			write(sout," ");
			assert vali <= 255 and vali >= -255 report "residual value out of range" severity ERROR;
		end loop;
		--
		-- each coeff line is used for 1 tests:
		-- 16 residual coretransform
		wait until rising_edge(clk2);
		if ready='0' then
			wait until ready='1';
		end if;
		for i in 0 to 3 loop
			enable <= '1';
			xxin <=
				CONV_STD_LOGIC_VECTOR(data(i*4+3),9) &
				CONV_STD_LOGIC_VECTOR(data(i*4+2),9) &
				CONV_STD_LOGIC_VECTOR(data(i*4+1),9) &
				CONV_STD_LOGIC_VECTOR(data(i*4),9);
			wait until rising_edge(clk2);
		end loop;
		enable <= '0';		
		wait until rising_edge(clk2);
		wait until rising_edge(clk2);
		writeline(output,sout);		--write input line now (quite late so after previous output)
		if slow then
			wait for 400 ns;	--to output all before next read
		end if;
	end loop;	--all input lines
	wait for 400 ns;
	write(sout,"#end of input");
	writeline(output,sout);
	wait for 1 ms;
	assert false severity ERROR;
end process;
	--
process(CLK2)		--output stage
	variable sout : line;
	variable n : integer;
	--variable verbose : boolean := false;
	variable wasvalid : boolean := false;
begin
	if rising_edge(CLK2) then
		if VALID='1' then
			if not wasvalid then
				write(sout,"=> coeff");
				wasvalid := true;
			end if;
			if YNOUT(13)='0' then
				n := conv_integer(YNOUT);
			else
				n := conv_integer(YNOUT)-16384;
			end if;
			write(sout," ");
			write(sout,n);
		elsif wasvalid then -- and not VALID
			writeline(output,sout);
			wasvalid := false;
		end if;
	end if;
end process;
	--
end test_transform;




