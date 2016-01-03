-------------------------------------------------------------------------
-- Test for H264 prediction - VHDL
-- 
-- Written by Andy Henson
-- Copyright (C) 2008 Zexia Access Ltd
-- All rights reserved
-------------------------------------------------------------------------

-- TEST stuff:  tests i4x4 prediction (mono) + i8x8cc
-- reads in test vectors from stdin (testpix.txt)
-- outputs stuff to stdout
-- takes about ?us simulation time to run all
-- this simple test assumes no quantisation losses, feedback = data out

library IEEE;
use ieee.std_logic_1164.ALL;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.ALL;
use std.textio.all;
use work.h264.all;
use work.misc.all;

entity test_pred is
end test_pred;

architecture test of test_pred is
	--
	constant MB_PER_LINE : integer := 2;
	constant QX : integer := 1;	--1 gives no test quantise, >1 quantises
	--
	type Trow is array(0 to 15) of integer;
	type Tfullrow is array(0 to 63) of integer;
	type Tdata is array(0 to 15, 0 to 15) of integer;
	type Ttypes is array(0 to 15) of std_logic_vector(3 downto 0);
	--
	signal CLK : std_logic := '0';			--clock
	signal CLK2 : std_logic;				--2x clock
	--
	signal NEWSLICE : std_logic := '1';				--first in slice
	signal NEWLINE : std_logic := '1';				--first mb and submb
	signal READYI : std_logic := '0';				--ready for enable when this set
	signal STROBEI : std_logic := '0';				--values transfered only when this is 1
	signal DATAI : std_logic_vector(31 downto 0) := (others => '0');
	signal TOPV : std_logic := '0';
	signal TOPI : std_logic_vector(31 downto 0) := (others => '0');
	signal TOPMI : std_logic_vector(3 downto 0) := (others => '0');
	signal FBSTROBE : std_logic := '0';				--feedback transfered only when this is 1
	signal FEEDBI : std_logic_vector(31 downto 0) := (others => '0');
	signal STROBEO : std_logic := '0';				--values transfered out when this is 1
	signal MSTROBEO : std_logic := '0';				--values transfered out when this is 1
	signal STROBEOd : std_logic := '0';				--delayed Strobeo
	signal DATAO : std_logic_vector(35 downto 0) := (others => '0');
	signal baseo : std_logic_vector(31 downto 0) := (others => '0');
	signal MODEO : std_logic_vector(3 downto 0) := (others => '0');	--0..8 prediction type
	signal PMODEO : std_logic := '0';	--prediction type same
	signal RMODEO : std_logic_vector(2 downto 0) := (others => '0');	--prediction type rem
	signal XXO : std_logic_vector(1 downto 0) := (others => '0');
	signal XXINC : std_logic := '0';
	signal CHREADY : std_logic := '0';
	signal READYO : std_logic := '0';
	--
	signal recon_data : std_logic_vector(35 downto 0) := (others => '0');
	signal recon_strobe : std_logic := '0';
	--
	signal result : Tdata := (others => (others => 0));			--result data
	signal modeos : Ttypes := (others => X"0");			--result types
	--
	signal toppix : Tfullrow;	--actually units of 4 pixels
	signal topmode : Tfullrow;
	signal mbx :integer := 0;	--macroblock x counter
	signal mbxx :integer := 0;	--macroblock x counter (arrays)
	signal data : Tdata;
	--
begin
	uut : h264intra4x4
	port map (
		CLK => clk2,
		--
		-- in interface:
		NEWSLICE => NEWSLICE,
		NEWLINE => NEWLINE,
		STROBEI => strobei,
		DATAI => datai,
		READYI => readyi,
		--
		-- top interface:
		TOPI => topi,
		TOPMI => topmi,
		XXO => xxo,
		XXINC => xxinc,
		--
		-- feedback interface:
		FEEDBI => feedbi(31 downto 24),
		FBSTROBE => fbstrobe,
		--
		-- out interface:
		STROBEO => strobeo,
		DATAO => datao,
		BASEO => baseo,
		READYO => readyo,
		MSTROBEO => mstrobeo,
		MODEO => MODEO,
		PMODEO => PMODEO,
		RMODEO => RMODEO,
		--
		CHREADY => chready
	);
	--
	TOPI <=
		CONV_STD_LOGIC_VECTOR(toppix(mbxx*4 + conv_integer(XXO)),32);
	TOPMI <= 
		CONV_STD_LOGIC_VECTOR(topmode(mbxx*4 + conv_integer(XXO)),4);
	--
	recon : h264recon
	port map (
		CLK2 => clk2,
		--
		NEWSLICE => NEWSLICE,
		STROBEI => recon_strobe,
		DATAI => recon_data,
		BSTROBEI => strobeo,
		BCHROMAI => '0',
		BASEI => baseo,
		--
		STROBEO => fbstrobe,
		DATAO => feedbi
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
process	--data input / compute
	variable index : integer;
	variable toprow : Trow;
	variable leftcol : Trow;
	variable top0valid : boolean := false;
	variable left0valid : boolean := false;
	variable topvalid : boolean := false;
	variable leftvalid : boolean := false;
	--
	variable s : line;
	variable sout : line;
	variable c : character;
	variable vali : integer;
	variable n : integer;
	--
	variable slow : boolean := true;
	variable x : integer;
	variable y : integer;
	variable submbv : std_logic_vector(3 downto 0);
	variable xv : std_logic_vector(1 downto 0);
	variable yv : std_logic_vector(1 downto 0);
	--
	--variable dconly : boolean;
	variable dif : integer;
	variable avg : integer;
	variable sad : integer;
	variable dcsad : integer;
	variable hsad : integer;
	variable vsad : integer;
	--
begin
	write(sout,"# Test output from VHDL TEST_PRED");
	writeline(output,sout);
	y := 0;
	--
	cmd: while not endfile(input) loop
		readline(input,s);
		write(s,' ');	--add space to end
		read(s,c);
		if c = '#' then
			if y /= 0 then
				write(sout,"ERROR: # in middle of macroblock");
				writeline(output,sout);
			end if;
			next cmd;
		end if;
		if c /= 'p' then
			write(sout,"ERROR EXPECTING pix");
			writeline(output,sout);
			next cmd;
		end if;
		read(s,c);--i
		read(s,c);--x
		write(sout,"pix ");
		for x in 0 to 15 loop
			read(s,vali);	--first pix
			data(x,y) <= vali;
			write(sout,vali);
			write(sout," ");
			assert vali <= 255 and vali >= 0 report "pix value out of range" severity ERROR;
		end loop;
		writeline(output,sout);
		--
		if y < 15 then
			y := y + 1;
			next cmd;
		end if;
		--
		-- got entire macroblock
		--
		wait for 1 ns;	--ensure data() valid
		--
		-- now process
		--
		for submb in 0 to 15 loop
			submbv := conv_std_logic_vector(submb,4);
			xv := submbv(2)&submbv(0);
			yv := submbv(3)&submbv(1);
			x := conv_integer(xv);
			y := conv_integer(yv);
			write(sout,"PROCESSING SUBMB ");
			write(sout,submb);
			write(sout," X ");
			write(sout,x);
			write(sout," Y ");
			write(sout,y);
			write(sout,"; ");
			--writeline(output,sout);
			x := x*4;
			y := y*4;
			if y = 0 then
				topvalid := top0valid;
			else
				topvalid := true;
			end if;
			if x = 0 then
				leftvalid := left0valid;
			else
				leftvalid := true;
			end if;
			--
			--compute average for dc
			if topvalid and leftvalid then
				avg := toprow(x) + toprow(x+1) + toprow(x+2) + toprow(x+3);
				avg := avg + leftcol(y) + leftcol(y+1) + leftcol(y+2) + leftcol(y+3);
				avg := (avg + 4 ) / 8;
			elsif topvalid then
				avg := toprow(x) + toprow(x+1) + toprow(x+2) + toprow(x+3);
				avg := (avg + 2) / 4;
			elsif leftvalid then
				avg := leftcol(y) + leftcol(y+1) + leftcol(y+2) + leftcol(y+3);
				avg := (avg + 2) / 4;
			else
				avg := 128;
			end if;
			write(sout,"avg ");
			write(sout,avg);
			--compute dc sad
			sad := 0;
			for xx in x to x+3 loop
				for yy in y to y+3 loop
					dif := data(xx,yy)-avg;
					sad := sad + abs(dif);
				end loop;
			end loop;
			dcsad := sad;
			write(sout," dcsad ");
			write(sout,sad);
			--compute horizontal sad
			sad := 0;
			if leftvalid and topvalid then
				for xx in x to x+3 loop
					for yy in y to y+3 loop
						dif := data(xx,yy)-leftcol(yy);
						sad := sad + abs(dif);
					end loop;
				end loop;
			else
				sad := 9999999;	--not valid
			end if;
			hsad := sad;
			if leftvalid and topvalid then
				write(sout," hsad ");
				write(sout,sad);
			else
				write(sout," dconly");
			end if;
			--compute vertical sad
			sad := 0;
			if leftvalid and topvalid then
				for xx in x to x+3 loop
					for yy in y to y+3 loop
						dif := data(xx,yy)-toprow(xx);
						result(xx,yy) <= dif;
						sad := sad + abs(dif);
					end loop;
				end loop;
			else
				sad := 9999999;	--not valid
			end if;
			vsad := sad;
			if leftvalid and topvalid then
				write(sout," vsad ");
				write(sout,sad);
			end if;
			write(sout," choosing ");
			--nb: we choose the lowest numbered mode if dif equal
			if dcsad < hsad and dcsad < vsad then
				write(sout,"dc(2)");
				modeos(submb) <= x"2";
				for xx in x to x+3 loop
					for yy in y to y+3 loop
						result(xx,yy) <= data(xx,yy)-avg;
					end loop;
				end loop;
			elsif hsad < vsad then
				write(sout,"horiz(1)");
				modeos(submb) <= x"1";
				for xx in x to x+3 loop
					for yy in y to y+3 loop
						result(xx,yy) <= data(xx,yy)-leftcol(yy);
					end loop;
				end loop;
			else
				write(sout,"vert(0)");
				modeos(submb) <= x"0";
			end if;
			writeline(output,sout);
			--
			wait for 1 ns;	--ensure signals updated
			--
			--feedback (test)
			--set top and left pixels
			--/QX*QX is rough approx of quantisation behaviour
			for xx in x to x+3 loop
				toprow(xx) := data(xx,y+3)/QX*QX;
			end loop;
			for yy in y to y+3 loop
				leftcol(yy) := data(x+3,yy)/QX*QX;
			end loop;
			--
		end loop;
		--
		for y in 0 to 15 loop
			write(sout,"result");
			for x in 0 to 15 loop
				write(sout," ");
				write(sout,result(x,y));
			end loop;
			writeline(output,sout);
		end loop;
		--
		--UUT INPUT
		if not left0valid then
			NEWLINE <= '1';
			wait until rising_edge(clk2);
			write(sout,"newline pulsed");
			writeline(output,sout);
		else
			NEWLINE <= '0';
			NEWSLICE <= '0';
		end if;
		for y in 0 to 15 loop
			if READYI='0' then wait until READYI='1'; end if;
			for xx in 0 to 3 loop
				x := xx*4;
				STROBEI <= '1';
				DATAI <=
					CONV_STD_LOGIC_VECTOR(data(x+3,y),8) &
					CONV_STD_LOGIC_VECTOR(data(x+2,y),8) &
					CONV_STD_LOGIC_VECTOR(data(x+1,y),8) &
					CONV_STD_LOGIC_VECTOR(data(x,y),8);
				wait until rising_edge(clk2);
				NEWLINE <= '0';
				NEWSLICE <= '0';
			end loop;
			STROBEI <= '0';
			wait until rising_edge(clk2);
		end loop;
		--END UUT INPUT
		--
		-- wait for UUT to want another mb
		if READYI='0' then wait until READYI='1'; end if;
		--
		y := 0;
		if mbx = MB_PER_LINE-1 then
			mbx <= 0;
			left0valid := false;
			top0valid := true;
		else
			mbx <= mbx + 1;
			left0valid := true;
		end if;
		--
		if slow then
			wait for 2000 ns;
		end if;
		--
	end loop;	--all input lines
	if not slow then
		wait for 1500 ns;
	end if;
	write(sout,"#end of input");
	writeline(output,sout);
	assert false report "DONE" severity FAILURE;
end process;
	--
process(CLK2)		--output from uut
	variable sout : line;
	variable submb : integer := 0;
	--variable verbose : boolean := false;
	variable wasvalid : boolean := false;
	type Tfb is array(0 to 15) of std_logic_vector(35 downto 0);
	variable fbdelay: Tfb;	--delay for FEEDB
	variable fbptr : integer := 0;
	variable fbptro : integer := 0;
	variable FEEDBI32 : std_logic_vector(31 downto 0) := (others => '0');
	variable submbv : std_logic_vector(3 downto 0);
	variable xv : std_logic_vector(1 downto 0);
	variable yv : std_logic_vector(1 downto 0);
	variable x : integer;
	variable y : integer;
begin
	if rising_edge(CLK2) then
		if strobeod='1' and STROBEO='0' then
			READYO <= '0';
		elsif CHREADY='1' then
			READYO <= '0';
		else
			READYO <= '1';
		end if;
		strobeod <= STROBEO;
		--
		if STROBEO='1' then
			if not wasvalid then
				write(sout,"submb ");
				write(sout,submb);
				write(sout," MODEO ");
				write(sout,conv_integer(MODEO));
				if PMODEO='1' then
					write(sout," typepr 1");
				else
					write(sout," typepr 0/");
					write(sout,conv_integer(RMODEO));
				end if;
				if MODEO /= modeos(submb) then
					assert false report "MODEO /= modeos(submb) [UUT/=REF]" severity warning;
					write(sout,"   <== DIFFERS from ");
					write(sout,conv_integer(modeos(submb)));
				end if;
				writeline(output,sout);
				wasvalid := true;				
			end if;
			fbdelay(fbptr) := DATAO;	--prior to reconstruct
			submbv := conv_std_logic_vector(submb,4);
			xv := submbv(2)&submbv(0);
			yv := submbv(3)&submbv(1);
			x := conv_integer(xv)*4;
			y := conv_integer(yv)*4 + fbptr;
			assert conv_std_logic_vector(result(x,y),9) = DATAO(8 downto 0) report "result(x,y)/=DATAO [UUT/=REF]" severity warning;
			assert conv_std_logic_vector(result(x+1,y),9) = DATAO(17 downto 9) report "result(x+1,y)/=DATAO [UUT/=REF]" severity warning;
			assert conv_std_logic_vector(result(x+2,y),9) = DATAO(26 downto 18) report "result(x+2,y)/=DATAO [UUT/=REF]" severity warning;
			assert conv_std_logic_vector(result(x+3,y),9) = DATAO(35 downto 27) report "result(x+3,y)/=DATAO [UUT/=REF]" severity warning;
			if conv_std_logic_vector(result(x,y),9) /= DATAO(8 downto 0) or
				conv_std_logic_vector(result(x+1,y),9) /= DATAO(17 downto 9) or
				conv_std_logic_vector(result(x+2,y),9) /= DATAO(26 downto 18) or
				conv_std_logic_vector(result(x+3,y),9) /= DATAO(35 downto 27) then
					write(sout,"** DIFFERS ***");
					writeline(output,sout);
			end if;
			--fbdelay(fbptr) := data(x,y)/QX*QX + data(x+1,y)/QX*QX*256 + data(x+2,y)/QX*QX*256*256 + data(x+3,y)/QX*QX*256*256*256;
			--
			assert fbptr < 4 and fbptr >= 0;
			--no more
			fbptr := fbptr + 1;
		elsif STROBEO='0' and wasvalid then
			submb := submb + 1;
			wasvalid := false;
			if submb = 16 then
				submb := 0;
			end if;
		end if;
		--UUT: reconstruct and feedback
		if STROBEO='0' and fbptro < fbptr then
			recon_strobe <= '1';
			recon_data <= fbdelay(fbptro);
			--FBSTROBE <= '1';
			--feedbi32 := CONV_STD_LOGIC_VECTOR(fbdelay(fbptro),32);
			--FEEDBI <= feedbi32(31 downto 24);
			if fbptro+1 < fbptr then
				fbptro := fbptro + 1;
			else
				fbptro := 0;
				fbptr := 0;
			end if;
		else
			recon_strobe <= '0';
		end if;
		if XXINC='1' and NEWLINE='0' then
			mbxx <= mbxx+1;
		elsif NEWLINE='1' then
			mbxx <= 0;
		end if;
		if MSTROBEO='1' then
			topmode(mbxx*4 + conv_integer(XXO)) <= conv_integer(MODEO);
		end if;
		if fbstrobe='1' then
			toppix(mbxx*4 + conv_integer(XXO)) <= conv_integer(FEEDBI);
		end if;
		--END UUT
	end if;
end process;
	--
end test;

architecture testcc of test_pred is
	--
	constant MB_PER_LINE : integer := 2;
	constant QX : integer := 1;	--1 gives no test quantise, >1 quantises
	--
	type Trow is array(0 to 15) of integer;
	type Tfullrow is array(0 to 63) of integer;
	type Tdata is array(0 to 15, 0 to 15) of integer;
	type Ttypes is array(0 to 15) of std_logic_vector(3 downto 0);
	--
	signal CLK : std_logic := '0';			--clock
	signal CLK2 : std_logic;				--2x clock
	--
	signal NEWSLICE : std_logic := '1';				--first in slice
	signal NEWLINE : std_logic := '1';				--first mb
	signal READYI : std_logic := '0';				--ready for enable when this set
	signal STROBEI : std_logic := '0';				--values transfered only when this is 1
	signal DATAI : std_logic_vector(31 downto 0) := (others => '0');
	signal TOPV : std_logic := '0';
	signal TOPI : std_logic_vector(31 downto 0) := (others => '0');
	signal TOPMI : std_logic_vector(3 downto 0) := (others => '0');
	signal FBSTROBE : std_logic := '0';				--feedback transfered only when this is 1
	signal FEEDBI : std_logic_vector(31 downto 0) := (others => '0');
	signal STROBEO : std_logic := '0';				--values transfered out when this is 1
	signal STROBEOd : std_logic := '0';				--values transfered out when this is 1
	signal DATAO : std_logic_vector(35 downto 0) := (others => '0');
	signal baseo : std_logic_vector(31 downto 0) := (others => '0');
	signal DCSTROBEO : std_logic := '0';				--values transfered out when this is 1
	signal DCDATAO : std_logic_vector(15 downto 0) := (others => '0');
	signal CMODEO : std_logic_vector(1 downto 0) := (others => '0');	--0..8 prediction type
	signal XXO : std_logic_vector(1 downto 0) := (others => '0');
	signal XXINC : std_logic := '0';
	signal CHREADY : std_logic := '0';
	signal READYO : std_logic := '0';
	--
	signal recon_data : std_logic_vector(35 downto 0) := (others => '0');
	signal recon_strobe : std_logic := '0';
	--
	signal result : Tdata := (others => (others => 0));			--result data
	signal modeos : Ttypes := (others => X"0");			--result types
	--
	signal toppix : Tfullrow;	--actually units of 4 pixels
	signal topmode : Tfullrow;
	signal mbx :integer := 0;	--macroblock x counter
	signal mbxx :integer := 0;	--macroblock x counter (arrays)
	signal data : Tdata;
	--
begin
	uut : h264intra8x8cc
	port map (
		CLK2 => clk2,
		--
		-- in interface:
		NEWSLICE => NEWSLICE,
		NEWLINE => NEWLINE,
		STROBEI => strobei,
		DATAI => datai,
		READYI => readyi,
		--
		-- top interface:
		TOPI => topi,
		XXO => xxo,
		XXINC => xxinc,
		--
		-- feedback interface:
		FEEDBI => feedbi(31 downto 24),
		FBSTROBE => fbstrobe,
		--
		-- out interface:
		STROBEO => strobeo,
		DATAO => datao,
		BASEO => baseo,
		READYO => readyo,
		DCSTROBEO => dcstrobeo,
		DCDATAO => dcdatao,
		CMODEO => CMODEO
	);
	--
	TOPI <=
		CONV_STD_LOGIC_VECTOR(toppix(mbxx*4 + conv_integer(XXO)),32);
	TOPMI <= 
		CONV_STD_LOGIC_VECTOR(topmode(mbxx*4 + conv_integer(XXO)),4);
	--
	recon : h264recon
	port map (
		CLK2 => clk2,
		--
		NEWSLICE => NEWSLICE,
		STROBEI => recon_strobe,
		DATAI => recon_data,
		BSTROBEI => strobeo,
		BCHROMAI => '0',
		BASEI => baseo,
		--
		STROBEO => fbstrobe,
		DATAO => feedbi
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
process	--data input / compute
	variable index : integer;
	variable toprow : Trow;
	variable leftcol : Trow;
	variable top0valid : boolean := false;
	variable left0valid : boolean := false;
	variable topvalid : boolean := false;
	variable leftvalid : boolean := false;
	--
	variable s : line;
	variable sout : line;
	variable c : character;
	variable vali : integer;
	variable n : integer;
	--
	variable slow : boolean := true;
	variable x : integer;
	variable y : integer;
	variable sum : integer;
	--
	--variable dconly : boolean;
	variable dif : integer;
	variable avg : integer;
	variable sad : integer;
	variable dcsad : integer;
	variable hsad : integer;
	variable vsad : integer;
	--
begin
	write(sout,"# Test output from VHDL TEST_PRED");
	writeline(output,sout);
	y := 0;
	--
	cmd: while not endfile(input) loop
		readline(input,s);
		write(s,' ');	--add space to end
		read(s,c);
		if c = '#' then
			if y /= 0 then
				write(sout,"ERROR: # in middle of macroblock");
				writeline(output,sout);
			end if;
			next cmd;
		end if;
		if c /= 'p' then
			write(sout,"ERROR EXPECTING pix");
			assert false report "ERROR EXPECTING pix" severity FAILURE;
			writeline(output,sout);
			next cmd;
		end if;
		read(s,c);--i
		read(s,c);--x
		write(sout,"pix ");
		for x in 0 to 15 loop
			read(s,vali);	--first pix
			data(x,y) <= vali;
			write(sout,vali);
			write(sout," ");
			assert vali <= 255 and vali >= 0 report "pix value out of range" severity ERROR;
		end loop;
		writeline(output,sout);
		--
		if y < 7 then
			y := y + 1;
			next cmd;
		end if;
		--
		-- got entire macroblock
		--
		wait for 1 ns;	--ensure data() valid
		--
		-- now process
		--
--		for crcb in 0 to 1 loop
--			for quad in 0 to 3 loop
--				if quad=0 then
--					x=0; y=0;
--				elsif quad=1 then
--					x=1; y=0;
--				elsif quad=1 then
--					x=0; y=1;
--				else	--if quad=3 then
--					x=1; y=1;
--				end if;
--				write(sout,"PROCESSING CRCB ");
--				write(sout,crcb);
--				write(sout," QUAD ");
--				write(sout,quad);
--				write(sout," X ");
--				write(sout,x);
--				write(sout," Y ");
--				write(sout,y);
--				write(sout,"; ");
--				--writeline(output,sout);
--				x := x*4;
--				y := y*4;
--				if y = 0 then
--					topvalid := top0valid;
--				else
--					topvalid := true;
--				end if;
--				if x = 0 then
--					leftvalid := left0valid;
--				else
--					leftvalid := true;
--				end if;
--				--
--				wait for 1 ns;	--ensure signals updated
--			--
--			--feedback (test)
--			--set top and left pixels
--			--/QX*QX is rough approx of quantisation behaviour
--			for xx in x to x+3 loop
--				toprow(xx) := data(xx,y+3)/QX*QX;
--			end loop;
--			for yy in y to y+3 loop
--				leftcol(yy) := data(x+3,yy)/QX*QX;
--			end loop;
--			--
--			end loop;
--		end loop;
		--
--		for y in 0 to 15 loop
--			write(sout,"result");
--			for x in 0 to 15 loop
--				write(sout," ");
--				write(sout,result(x,y));
--			end loop;
--			writeline(output,sout);
--		end loop;
		--
		--UUT INPUT
		if not left0valid then
			wait for 2 us;	--for mb to finish output
			wait until rising_edge(clk2);
			NEWLINE <= '1';
			wait until rising_edge(clk2);
			write(sout,"newline pulsed");
			writeline(output,sout);
		else
			NEWLINE <= '0';
			NEWSLICE <= '0';
		end if;
		--cb
		for y in 0 to 7 loop
			if READYI='0' then wait until READYI='1'; end if;
			for xx in 0 to 1 loop
				x := xx*4;
				STROBEI <= '1';
				DATAI <=
					CONV_STD_LOGIC_VECTOR(data(x+3,y),8) &
					CONV_STD_LOGIC_VECTOR(data(x+2,y),8) &
					CONV_STD_LOGIC_VECTOR(data(x+1,y),8) &
					CONV_STD_LOGIC_VECTOR(data(x,y),8);
				wait until rising_edge(clk2);
				NEWLINE <= '0';
				NEWSLICE <= '0';
			end loop;
			STROBEI <= '0';
			wait until rising_edge(clk2);
		end loop;
		--cr
		for y in 0 to 7 loop
			if READYI='0' then wait until READYI='1'; end if;
			for xx in 2 to 3 loop
				x := xx*4;
				STROBEI <= '1';
				DATAI <=
					CONV_STD_LOGIC_VECTOR(data(x+3,y),8) &
					CONV_STD_LOGIC_VECTOR(data(x+2,y),8) &
					CONV_STD_LOGIC_VECTOR(data(x+1,y),8) &
					CONV_STD_LOGIC_VECTOR(data(x,y),8);
				wait until rising_edge(clk2);
				NEWLINE <= '0';
				NEWSLICE <= '0';
			end loop;
			STROBEI <= '0';
			wait until rising_edge(clk2);
		end loop;
		--END UUT INPUT
		--
		-- wait for UUT to want another mb
		if READYI='0' then wait until READYI='1'; end if;
		--
		y := 0;
		if mbx = MB_PER_LINE-1 then
			mbx <= 0;
			left0valid := false;
			top0valid := true;
		else
			mbx <= mbx + 1;
			left0valid := true;
		end if;
		--
		if slow then
			wait for 2000 ns;
		end if;
		--
	end loop;	--all input lines
	if not slow then
		wait for 1500 ns;
	end if;
	write(sout,"#end of input");
	writeline(output,sout);
	assert false report "DONE" severity FAILURE;
end process;
	--
process(CLK2)		--output from uut
	variable sout : line;
	variable submb : integer := 0;
	--variable verbose : boolean := false;
	variable wasvalid : boolean := false;
	type Tfb is array(0 to 15) of std_logic_vector(35 downto 0);
	variable fbdelay: Tfb;	--delay for FEEDB
	variable fbptr : integer := 0;
	variable fbptro : integer := 0;
	variable FEEDBI32 : std_logic_vector(31 downto 0) := (others => '0');
	variable x : integer;
	variable y : integer := 0;
	variable quad : integer := 0;
	variable sum : integer := 0;
	variable q : integer := 0;
	type Tdc is array(3 downto 0) of integer;
	variable dcresid : Tdc;
begin
	if rising_edge(CLK2) then
		if strobeod='1' and STROBEO='0' then
			READYO <= '0';
		elsif CHREADY='1' then
			READYO <= '0';
		else
			READYO <= '1';
		end if;
		strobeod <= STROBEO;
		--
		if DCSTROBEO='1' then			write(sout,"dcresidual ");
			write(sout,conv_integer_signed(DCDATAO));
			writeline(output,sout);
			dcresid(q) := conv_integer_signed(DCDATAO);
			q := q+1;
		end if;
		if STROBEO='1' then
			if not wasvalid then
				write(sout,"quad ");
				write(sout,quad);
				write(sout," CMODEO ");
				write(sout,conv_integer(CMODEO));
				writeline(output,sout);
				wasvalid := true;
				sum := 0;
				q := 0;
			end if;
			fbdelay(fbptr) := DATAO;	--prior to reconstruct
			write(sout,"residual ");
			write(sout,conv_integer_signed(DATAO(8 downto 0)));
			write(sout," ");
			write(sout,conv_integer_signed(DATAO(17 downto 9)));
			write(sout," ");
			write(sout,conv_integer_signed(DATAO(26 downto 18)));
			write(sout," ");
			write(sout,conv_integer_signed(DATAO(35 downto 27)));
			sum := sum +
				conv_integer_signed(DATAO(8 downto 0)) +
				conv_integer_signed(DATAO(17 downto 9)) +
				conv_integer_signed(DATAO(26 downto 18)) +
				conv_integer_signed(DATAO(35 downto 27));
			if y=3 then
				write(sout," sum ");
				write(sout,sum);
				if dcresid(quad)/=sum then
					write(sout," *** DIFFER ***");
					assert false report "DC sums differ" severity warning; 
				end if;
			end if;
			writeline(output,sout);
			y := y + 1;
			if y=4 then
				quad := quad + 1;
				y := 0;
				if quad=4 then
					quad := 0;
				end if;
			end if;
--			assert conv_std_logic_vector(result(x,y),9) = DATAO(8 downto 0) report "result(x,y)/=DATAO [UUT/=REF]" severity warning;
--			assert conv_std_logic_vector(result(x+1,y),9) = DATAO(17 downto 9) report "result(x+1,y)/=DATAO [UUT/=REF]" severity warning;
--			assert conv_std_logic_vector(result(x+2,y),9) = DATAO(26 downto 18) report "result(x+2,y)/=DATAO [UUT/=REF]" severity warning;
--			assert conv_std_logic_vector(result(x+3,y),9) = DATAO(35 downto 27) report "result(x+3,y)/=DATAO [UUT/=REF]" severity warning;
--			if conv_std_logic_vector(result(x,y),9) /= DATAO(8 downto 0) or
--				conv_std_logic_vector(result(x+1,y),9) /= DATAO(17 downto 9) or
--				conv_std_logic_vector(result(x+2,y),9) /= DATAO(26 downto 18) or
--				conv_std_logic_vector(result(x+3,y),9) /= DATAO(35 downto 27) then
--					write(sout,"** DIFFERS ***");
--					writeline(output,sout);
--			end if;
			--fbdelay(fbptr) := data(x,y)/QX*QX + data(x+1,y)/QX*QX*256 + data(x+2,y)/QX*QX*256*256 + data(x+3,y)/QX*QX*256*256*256;
			--
			assert fbptr < 4 and fbptr >= 0;
			--no more
			fbptr := fbptr + 1;
		elsif STROBEO='0' and wasvalid then
			wasvalid := false;
		end if;
		--UUT: reconstruct and feedback
		if STROBEO='0' and fbptro < fbptr then
			recon_strobe <= '1';
			recon_data <= fbdelay(fbptro);
			--FBSTROBE <= '1';
			--feedbi32 := CONV_STD_LOGIC_VECTOR(fbdelay(fbptro),32);
			--FEEDBI <= feedbi32(31 downto 24);
			if fbptro+1 < fbptr then
				fbptro := fbptro + 1;
			else
				fbptro := 0;
				fbptr := 0;
			end if;
		else
			recon_strobe <= '0';
		end if;
		if XXINC='1' and NEWLINE='0' then
			mbxx <= mbxx+1;
		elsif NEWLINE='1' then
			mbxx <= 0;
		end if;
--		if MSTROBEO='1' then
--			topmode(mbxx*4 + conv_integer(XXO)) <= conv_integer(MODEO);
--		end if;
		if fbstrobe='1' then
			toppix(mbxx*4 + conv_integer(XXO)) <= conv_integer_signed(FEEDBI);
		end if;
		--END UUT
	end if;
end process;
	--
end testcc;
