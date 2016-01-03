-------------------------------------------------------------------------
-- Test2 for H264 transforms - VHDL
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

-- TEST stuff:  test2 tests coretransform, quantise, dequantise, invtransform
-- reads in test vectors from stdin (testresidual.txt)
-- outputs stuff to stdout
-- takes about 300us simulation time to run all
-- modify slow to give decent gap between each processing
-- uses READY if slow not set
-- modify verbose to show all intermediate steps

-- output of "ERROR" if reconstructed samples differ lots from input
-- output of "DIFF" if reconstructed samples differ slightly  from input
-- UNF

library IEEE;
use ieee.std_logic_1164.ALL;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.ALL;
use std.textio.all;
use work.h264.all;
use work.misc.all;

entity test_transform2 is
end test_transform2;

architecture test_transform of test_transform2 is
	--
	signal CLK : std_logic := '0';			--clock
	signal CLK2 : std_logic;				--2x clock
	signal QP : std_logic_vector(5 downto 0) := Conv_std_logic_vector(0,6);
	signal READY : std_logic := '0';				--ready for enable when this set
	signal ENABLE : std_logic := '0';				--values transfered only when this is 1
	signal XXIN : std_logic_vector(35 downto 0) := (others => '0');
	signal uuttransform_valid : std_logic := '0';
	signal uuttransform_ynout : std_logic_vector(13 downto 0);
	signal uutquant_valid : std_logic := '0';
	signal uutquant_zout : std_logic_vector(11 downto 0);
	signal uutdequant_valid : std_logic := '0';
	signal uutdequant_wout : std_logic_vector(15 downto 0);
	signal uutinvtransform_valid : std_logic := '0';
	signal uutinvtransform_xout : std_logic_vector(35 downto 0);
	--
begin
	uuttransform : h264coretransform
	port map (
		CLK => clk2,
		READY => ready,
		ENABLE => enable,
		XXIN => xxin,
		VALID => uuttransform_valid,
		YNOUT => uuttransform_ynout
	);
	--
	uutquant : h264quantise
	port map (
		CLK => clk2,
		ENABLE => uuttransform_valid,
		DCCI => '0',
		QP => qp,
		YNIN => uuttransform_ynout,
		ZOUT => uutquant_zout,
		VALID => uutquant_valid
	);
	--
	uutdequant : h264dequantise
	port map (
		CLK => clk2,
		ENABLE => uutquant_valid,
		QP => qp,
		DCCI => '0',
		ZIN => uutquant_zout,
		WOUT => uutdequant_wout,
		VALID => uutdequant_valid
	);
	--
	uutinvtransform : h264invtransform
	port map (
		CLK => clk2,
		ENABLE => uutdequant_valid,
		WIN => uutdequant_wout,
		--LAST => 
		VALID => uutinvtransform_valid,
		XOUT => uutinvtransform_xout
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
	variable slow : boolean := true;
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
			wait for 1000 ns;	--to output all before next read
		end if;
	end loop;	--all input lines
	wait for 1000 ns;
	write(sout,"#end of input");
	writeline(output,sout);
	wait for 1 ms;
	assert false report "DONE" severity ERROR;
end process;
	--
process(CLK2)		--output from uuttransform
	variable sout : line;
	variable n : integer;
	--variable verbose : boolean := false;
	variable wasvalid : boolean := false;
begin
	if rising_edge(CLK2) then
		if uuttransform_valid='1' then
			if not wasvalid then
				write(sout,"=> coeff");
				wasvalid := true;
			end if;
			n := conv_integer_signed(uuttransform_ynout);
			write(sout," ");
			write(sout,n);
		elsif wasvalid then -- and not VALID
			writeline(output,sout);
			wasvalid := false;
		end if;
	end if;
end process;
	--
process(CLK2)		--output from uutquantise
	variable sout : line;
	variable n : integer;
	--variable verbose : boolean := false;
	variable wasvalid : boolean := false;
begin
	if rising_edge(CLK2) then
		if uutquant_valid='1' then
			if not wasvalid then
				write(sout,"=> quant(qp:");
				write(sout,conv_integer(qp));
				write(sout,")");
				wasvalid := true;
			end if;
			if uutquant_zout(11)='0' then
				n := conv_integer(uutquant_zout);
			else
				n := conv_integer(uutquant_zout)-4096;
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
process(CLK2)		--output from uutdequantise
	variable sout : line;
	variable n : integer;
	--variable verbose : boolean := false;
	variable wasvalid : boolean := false;
begin
	if rising_edge(CLK2) then
		if uutdequant_valid='1' then
			if not wasvalid then
				write(sout,"=> coeff'");
				wasvalid := true;
			end if;
			if uutdequant_wout(15)='0' then
				n := conv_integer(uutdequant_wout);
			else
				n := conv_integer(uutdequant_wout)-65536;
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
process(CLK2)		--output from uutinvtransform
	variable sout : line;
	variable rout : line;
	variable n : integer;
	variable b : integer;
	variable wasvalid : boolean := false;
begin
	if rising_edge(CLK2) then
		if uutinvtransform_valid='1' then
			if not wasvalid then
				write(sout,"=> residual");
				--write(rout,"=> residual");
				wasvalid := true;
			end if;
			for i in 0 to 3 loop
				b := i*9;
				if uutinvtransform_xout(8+b)='0' then
					n := conv_integer(uutinvtransform_xout(8+b downto b));
				else
					n := conv_integer(uutinvtransform_xout(8+b downto b))-512;
				end if;
				write(sout," ");
				write(sout,n);
			end loop;
			write(sout,";");
		elsif wasvalid then -- and not VALID
			writeline(output,sout);
			wasvalid := false;
		end if;
	end if;
end process;
	--
process(CLK2)		--check for dequantise
	variable sout : line;
	variable qout : line;
	type intarray is array(0 to 15) of integer;
	type table is array(0 to 5) of integer;
	variable aquant : intarray;
	variable qi : integer;
	variable qo : integer;
	variable qq : integer;
	variable qpi : integer;
	variable qpj : integer;
	variable zig : integer;
	variable v : integer;
	variable w : integer;
	constant tablea : table := (10,11,13,14,16,18);
	constant tableb : table := (16,18,20,23,25,29);
	constant tablec : table := (13,14,16,18,20,23);
begin
	if rising_edge(CLK2) then
		if uutquant_valid='1' then
			if uutquant_zout(11)='0' then
				aquant(qi) := conv_integer(uutquant_zout);
			else
				aquant(qi) := conv_integer(uutquant_zout)-4096;
			end if;
--			if qi=0 then write(sout,"** quant"); end if;
--			if qi=0 then write(qout,"** coeff"); end if;
--			write(sout," ");
--			write(sout,aquant(qi));
			qi := qi+1;
		else
			qi := 0;
		end if;
		if uutdequant_valid='1' then
			qq := aquant(qo);
			qpi := conv_integer(QP);
			qpj := qpi / 6;			--multiples of 6
			qpi := qpi - (qpj*6);	--remainder 0..5
			assert qpi>=0 and qpi<=5;
			assert qpj>=0 and qpj<=8;	--51 max / 6 is 8
			zig := 15-qo;
			if zig=0 or zig=3 or zig=5 or zig=11 then
				--positions 0,0; 0,2; 2,0; 2,2 - tablea
				v := tablea(qpi);
			elsif zig=4 or zig=10 or zig=12 or zig=15 then
				--positions 1,1; 1,3; 3,1; 3,3 - tableb
				v := tableb(qpi);
			else
				v := tablec(qpi);
			end if;
			if qpj > 0 then
				for i in 0 to qpj loop
					v := v*2;
				end loop;
			end if;
			qq := qq*v;
			if uutdequant_wout(15)='0' then
				w := conv_integer(uutdequant_wout);
			else
				w := conv_integer(uutdequant_wout)-65536;
			end if;
			assert qq = w report "computed qq= differs from dequant component " severity warning; 
--			write(qout," ");
--			write(qout,qq);
			qo := qo+1;
--			if qo=16 then writeline(output,sout); end if;
--			if qo=16 then writeline(output,qout); end if;
		else
			qo := 0;
		end if;
	end if;
end process;
	--
process(CLK2)		--check for INVTRANSFORM
	variable cout : line;
	variable sout : line;
	variable nullln : line;
	type array44 is array(0 to 3, 0 to 3) of integer;
	variable d : array44;
	variable e : array44;
	variable f : array44;
	variable g : array44;
	variable h : array44;
	variable x : integer;
	variable y : integer;
	variable qi : integer;
	variable yo : integer;
	variable v : integer;
	variable b : integer;
	variable n : integer;
	variable w : integer;
	variable differ : boolean;
begin
	if rising_edge(CLK2) then
		if uutdequant_valid='1' then
			if uutdequant_wout(15)='0' then
				w := conv_integer(uutdequant_wout);
			else
				w := conv_integer(uutdequant_wout)-65536;
			end if;
			if qi=0 then y:=3; x:=3;	
			elsif qi=1 then y:=3; x:=2;
			elsif qi=2 then y:=2; x:=3;
			elsif qi=3 then y:=1; x:=3;
			elsif qi=4 then y:=2; x:=2;
			elsif qi=5 then y:=3; x:=1;
			elsif qi=6 then y:=3; x:=0;
			elsif qi=7 then y:=2; x:=1;
			elsif qi=8 then y:=1; x:=2;
			elsif qi=9 then y:=0; x:=3;
			elsif qi=10 then y:=0; x:=2;
			elsif qi=11 then y:=1; x:=1;
			elsif qi=12 then y:=2; x:=0;
			elsif qi=13 then y:=1; x:=0;
			elsif qi=14 then y:=0; x:=1;
			elsif qi=15 then y:=0; x:=0;
			end if;
			d(x,y) := w;
			qi := qi+1;
		else
			qi := 0;
		end if;
		if qi=16 then
			write(cout,"** coeff'");
			for y in 0 to 3 loop
				for x in 0 to 3 loop
					write(cout," ");
					write(cout,d(x,y));
				end loop;
				write(cout,";");
			end loop;
			write(sout,"** residual");
			differ := false;
			--
			--now perform transform on it
			--standard para 8.5.8
			--
			for i in 0 to 3 loop
				e(i,0):=d(i,0)+d(i,2);
				e(i,1):=d(i,0)-d(i,2);
				e(i,2):=(d(i,1)/2)-d(i,3);
				e(i,3):=d(i,1)+(d(i,3)/2);
			end loop;
			for i in 0 to 3 loop
				f(i,0):=e(i,0)+e(i,3);
				f(i,1):=e(i,1)+e(i,2);
				f(i,2):=e(i,1)-e(i,2);
				f(i,3):=e(i,0)-e(i,3);
			end loop;
			for i in 0 to 3 loop
				g(0,i):=f(0,i)+f(2,i);
				g(1,i):=f(0,i)-f(2,i);
				g(2,i):=f(1,i)/2-f(3,i);
				g(3,i):=f(1,i)+f(3,i)/2;
			end loop;
			for i in 0 to 3 loop
				h(0,i):=g(0,i)+g(3,i);
				h(1,i):=g(1,i)+g(2,i);
				h(2,i):=g(1,i)-g(2,i);
				h(3,i):=g(0,i)-g(3,i);
			end loop;
		end if;
		if uutinvtransform_valid='1' then
			for x in 0 to 3 loop
				b := x*9;
				v := h(x,yo);
				--if v>0 then v := v+32; else v := v-32; end if;
				--v := v/64;
				v := v+32;
				v := shr(v,6);
				--optional clipping
				if v>255 then v:=255; end if;
				if v<-256 then v:=-256; end if;
				--if uutinvtransform_xout(8+b)='0' then
				n := conv_integer_signed(uutinvtransform_xout(8+b downto b));
				--else
				--	n := conv_integer(uutinvtransform_xout(8+b downto b))-512;
				--end if;
				write(sout," ");
				write(sout,v);
				if n /= v then
					differ := true;
				end if;
				--assert n = v report "computed invtransform differs" severity warning; 
			end loop;
			write(sout,";");
			if yo=3 then
				if differ then
					write(sout," ** DIFFERS");
					writeline(output,cout);
					writeline(output,sout);
				else
					cout:=nullln;
					sout:=nullln;
				end if;
			end if;
			yo := yo+1;
		else
			yo := 0;
		end if;
	end if;
end process;
	--
end test_transform;

