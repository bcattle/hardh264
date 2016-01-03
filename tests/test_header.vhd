-------------------------------------------------------------------------
-- Test for H264 header / tobytes - VHDL
-- 
-- Written by Andy Henson
-- Copyright (C) 2008 Zexia Access Ltd
-- All rights reserved
-------------------------------------------------------------------------

-- TEST stuff:  tests header & tobytes modules
-- outputs stuff to stdout

library IEEE;
use ieee.std_logic_1164.ALL;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.ALL;
use std.textio.all;
use work.h264.all;

entity test_header is
end test_header;

architecture test of test_header is
	--
	signal CLK : std_logic := '0';			--clock
	signal CLK2 : std_logic;				--2x clock
	--
	signal NEWSLICE : std_logic := '1';			--reset: this is the first a slice
	--signal SINTRA : std_logic;				--slice I flag
	--
	signal QP : std_logic_vector(5 downto 0) := (others=>'0');
	--signal MINTRA : std_logic;				--macroblock I flag
	signal READY : std_logic;					--soft "ready" flag
	signal LSTROBE : std_logic := '0';				--luma data here (16 of these)
	signal CSTROBE : std_logic := '0';				--chroma data (first latches CTYPE)
	signal PMODE : std_logic := '0';				--luma prev_intra4x4_pred_mode_flag
	signal RMODE : std_logic_vector(2 downto 0);	--luma rem_intra4x4_pred_mode_flag
	signal CMODE : std_logic_vector(1 downto 0);	--intra_chroma_pred_mode
	--
	signal VE : std_logic_vector(19 downto 0) := (others=>'0');
	signal VL : std_logic_vector(4 downto 0) := (others=>'0');
	signal VALID : std_logic := '0';	-- VE/VL valid
	signal VE1 : std_logic_vector(24 downto 0) := (others=>'0');
	signal VL1 : std_logic_vector(4 downto 0) := (others=>'0');
	signal VALID1 : std_logic := '0';	-- VE/VL1 valid
	signal VE2 : std_logic_vector(19 downto 0) := (others=>'0');
	signal VL2 : std_logic_vector(4 downto 0) := (others=>'0');
	signal VALID2 : std_logic := '0';	-- VE/VL2 valid
	--
	signal XBYTE: std_logic_vector(7 downto 0) := (others=>'0');
	signal STROBE : std_logic := '0';	-- BYTE valid
	--
begin
	uut : h264header
	port map (
		CLK => clk,
		NEWSLICE => newslice, 
		--LASTSLICE => '1'
		SINTRA => '1',	--all slices are Intra in this test
		--
		MINTRA => '1',	--ditto all mbs
		LSTROBE => lstrobe,
		CSTROBE => cstrobe,
		QP => qp,
		--
		PMODE => pmode,
		RMODE => rmode,
		CMODE => cmode,
		--
		PTYPE => b"00",
		PSUBTYPE => b"00",
		MVDX => x"000",
		MVDY => x"000",
		--
		VE => ve,
		VL => vl,
		VALID => valid
	);
	--
	uut2: h264tobytes
	port map (
		CLK => clk,
		VALID => valid1,
		READY => ready,
		VE => ve1,
		VL => vl1,
		BYTE => xbyte,
		STROBE => strobe
	);
	--
	valid1 <= valid when valid='1' else valid2;
	ve1 <= b"00000"&ve when valid='1' else b"00000"&ve2;
	vl1 <= vl when valid='1' else vl2;
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
process	--data input / compute
	variable s : line;
	variable sout : line;
	variable c : character;
	variable vali : integer;
	variable n : integer;
begin
	write(sout,"# Test output from VHDL TEST_HEADER");
	writeline(output,sout);
	--
	for iqp in 0 to 51 loop
		write(sout,"# qp=");
		write(sout,iqp);
		writeline(output,sout);
		qp <= conv_std_logic_vector(iqp,6);
		--test header, I-frame only
		newslice <= '1';
		wait until rising_edge(CLK);
		newslice <= '0';
		wait until rising_edge(CLK);
		for i in 0 to 15 loop
			wait for 1 us; wait until rising_edge(CLK);
			if i>5 and i<9 then
				pmode <= '0';
			else
				pmode <= '1';
				rmode <= b"00" & conv_std_logic_vector(i,1);
			end if;
			lstrobe <= '1';
			wait until rising_edge(CLK);
			wait until rising_edge(CLK);
			wait until rising_edge(CLK);
			wait until rising_edge(CLK);
			lstrobe <= '0';
			wait until rising_edge(CLK);
			wait for 1 us; wait until rising_edge(CLK);
			if i=8 or i=10 or i=12 or i=14 then
				cstrobe <= '1';
				cmode <= b"01";	--eg
				wait until rising_edge(CLK);
				wait until rising_edge(CLK);
				wait until rising_edge(CLK);
				wait until rising_edge(CLK);
				cstrobe <= '0';
				wait for 1 us; wait until rising_edge(CLK);
			end if;
		end loop;
		--
		wait for 1 us; wait until rising_edge(CLK);
		--
		VALID2 <= '1';
		VE2 <= x"10080";	--special align terminal
		VL2 <= b"01000";
		wait until rising_edge(CLK);	
		VALID2 <= '0';
		wait until rising_edge(CLK);	
		wait for 1 us; wait until rising_edge(CLK);
	--
	end loop;
	--
	write(sout,"#2nd stage tests (of tobytes)");
	writeline(output,sout);
	--
	write(sout,"TEST 111111111");
	writeline(output,sout);
	VALID2 <= '1';
	VE2 <= x"001FF";
	VL2 <= b"01001";	--9bits of 1's (mostly to non-align it)
	wait until rising_edge(CLK);
	VALID2 <= '0';
	wait until rising_edge(CLK);
	--
	--for bit lengths of 1 to 12, each time output 8 sets with different patterns
	--total of 96 output words, 624 bits (78 bytes)
	for n in 1 to 12 loop
		for b in 0 to 7 loop
			VALID2 <= '1';
			VE2 <= x"00"&
				conv_std_logic_vector(b,3)&conv_std_logic_vector(b,3)
				&conv_std_logic_vector(b,3)&conv_std_logic_vector(b,3);
			VL2 <= conv_std_logic_vector(n,5);
			wait until rising_edge(CLK);
			write(sout,"TEST ");
			for i in n-1 downto 0 loop
				write(sout,conv_integer(VE2(i)));
			end loop;
			writeline(output,sout);
		end loop;
		VALID2 <= '0';
		wait until rising_edge(CLK);
		if READY='0' and VL2(0)='0' then	--only check READY every other group of 8 (every 16)
			wait until READY='1';
		end if;
		wait until rising_edge(CLK);
	end loop;
	wait until rising_edge(CLK);
	VALID2 <= '1';
	VE2 <= x"10080";	--special align terminal
	VL2 <= b"01000";
	wait until rising_edge(CLK);
	VALID2 <= '0';
	wait until rising_edge(CLK);
	wait for 3 us; wait until rising_edge(CLK);
	--
	-- align tests (slow)
	for n in 0 to 8 loop
		VALID2 <= '1';
		VE2 <= x"000FF";
		VL2 <= conv_std_logic_vector(n,5);
		wait until rising_edge(CLK);	
			write(sout,"TEST ");
			for i in n-1 downto 0 loop
				write(sout,conv_integer(VE2(i)));
			end loop;
			write(sout," 1 ALIGN");
			writeline(output,sout);
		VALID2 <= '1';
		VE2 <= x"10080";	--special align terminal
		VL2 <= b"01000";
		wait until rising_edge(CLK);	
		VALID2 <= '0';
		wait until rising_edge(CLK);	
		wait until rising_edge(CLK);	
		wait until rising_edge(CLK);
		wait until rising_edge(CLK);	
		wait until rising_edge(CLK);
		wait until rising_edge(CLK);
	end loop;
	--
	--align tests (fast)
	for n in 0 to 8 loop
		VALID2 <= '1';
		VE2 <= x"000FF";
		VL2 <= conv_std_logic_vector(n,5);
		wait until rising_edge(CLK);	
			write(sout,"TEST ");
			for i in n-1 downto 0 loop
				write(sout,conv_integer(VE2(i)));
			end loop;
			write(sout," 1 ALIGN");
			writeline(output,sout);
		VALID2 <= '1';
		VE2 <= x"10080";	--special align terminal
		VL2 <= b"01000";
		wait until rising_edge(CLK);	
	end loop;
	VALID2 <= '0';
	wait until rising_edge(CLK);
	for w in 1 to 32 loop
		wait until rising_edge(CLK);
	end loop;
	--
	-- align+done tests (slow)
	for n in 0 to 8 loop
		VALID2 <= '1';
		VE2 <= x"000FF";
		VL2 <= conv_std_logic_vector(n,5);
		wait until rising_edge(CLK);	
			write(sout,"TEST ");
			for i in n-1 downto 0 loop
				write(sout,conv_integer(VE2(i)));
			end loop;
			write(sout," 1 ALIGN+DONE");
			writeline(output,sout);
		VALID2 <= '1';
		VE2 <= x"30080";	--special align terminal
		VL2 <= b"01000";
		wait until rising_edge(CLK);	
		VALID2 <= '0';
		wait until rising_edge(CLK);	
		wait until rising_edge(CLK);	
		wait until rising_edge(CLK);
		wait until rising_edge(CLK);	
		wait until rising_edge(CLK);
		wait until rising_edge(CLK);	
	end loop;
	--
	--align+done tests (fast)
	for n in 0 to 8 loop
		VALID2 <= '1';
		VE2 <= x"000FF";
		VL2 <= conv_std_logic_vector(n,5);
		wait until rising_edge(CLK);	
			write(sout,"TEST ");
			for i in n-1 downto 0 loop
				write(sout,conv_integer(VE2(i)));
			end loop;
			write(sout," 1 ALIGN+DONE");
			writeline(output,sout);
		VALID2 <= '1';
		VE2 <= x"30080";	--special align terminal
		VL2 <= b"01000";
		wait until rising_edge(CLK);	
	end loop;
	VALID2 <= '0';
	wait until rising_edge(CLK);
	for w in 0 to 120 loop
		wait until rising_edge(CLK);	
	end loop;
	--
	--stuffing tests (slow)
	for n in 8 downto 0 loop
			write(sout,"STUFFING TEST 00 00 0");
			write(sout,n);
			writeline(output,sout);
		VALID2 <= '1';
		VE2 <= x"00000";
		VL2 <= b"10000";	--two bytes zeros
		wait until rising_edge(CLK);	
		VE2 <= conv_std_logic_vector(n,20);
		VL2 <= b"01000";	--byte
		wait until rising_edge(CLK);
		VALID2 <= '0';
		for w in 0 to 10 loop
			wait until rising_edge(CLK);	
		end loop;
	end loop;
	--
	--suffing tests (fast)
	for n in 8 downto 0 loop
		if READY='0' then
			VALID2 <= '0';
			wait until READY='1';
		end if;
			write(sout,"STUFFING TEST 00 00 0");
			write(sout,n);
			writeline(output,sout);
		VALID2 <= '1';
		VE2 <= x"00000";
		VL2 <= b"10000";	--two bytes zeros
		wait until rising_edge(CLK);	
		VE2 <= conv_std_logic_vector(n,20);
		VL2 <= b"01000";	--byte
		wait until rising_edge(CLK);
	end loop;
	--friendly align terminal
	VALID2 <= '1';
	VE2 <= x"30080";	--special align terminal
	VL2 <= b"01000";
	wait until rising_edge(CLK);	
	VALID2 <= '0';
	wait until rising_edge(CLK);	
	--
	wait for 3 us; wait until rising_edge(CLK);
	write(sout,"#end of input");
	writeline(output,sout);
	assert false report "DONE" severity FAILURE;
end process;
	--
process(CLK)		--output from uut
	variable sout : line;
	variable bb: integer;
	variable n: integer;
begin
	if rising_edge(CLK) then
		if VALID='1' then
			write(sout,"OUT ");
			n := conv_integer(vl);
			bb := 8;
			for i in n-1 downto 0 loop
				write(sout,conv_integer(ve(i)));
				bb := bb - 1;
				if bb=0 then write(sout," "); bb:= 8; end if;
			end loop;
			writeline(output,sout);
		end if;
	end if;
end process;
	--
process(CLK)		--output from uut1
	variable sout : line;
	variable n : integer;
	constant table : string := "0123456789ABCDEF";
begin
	if rising_edge(CLK) then
		if STROBE='1' then
			write(sout,"=> BYTE ");
			n := conv_integer(xbyte(7 downto 4));
			write(sout, table(n+1));
			n := conv_integer(xbyte(3 downto 0));
			write(sout, table(n+1));
			writeline(output,sout);
		end if;
	end if;
end process;
	--
end test;
