-- BBC Micro Video ULA replacement using Xilinx Coolrunner XCR3182XL
--
-- Copyright (c) 2020 Jon Sole
--
-- Based on previous work by Mike Stirling.  Modified to increase compatibility with original VIDPROC ULA.
--
-- BBC Micro for Altera DE1
--
-- Copyright (c) 2011 Mike Stirling
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in synthesized form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name of the author nor the names of other contributors may
--   be used to endorse or promote products derived from this software without
--   specific prior written agreement from the author.
--
-- * License is granted for non-commercial use only.  A fee may not be charged
--   for redistributions as source code or in synthesized/hardware form without
--   specific prior written agreement from the author.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- BBC Micro "VIDPROC" Video ULA
--
-- Synchronous implementation for FPGA
--
-- (C) 2011 Mike Stirling
--

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity vidproc is
    port (
		-- Debug
		--DBG_CLK_CNT		: 	out std_logic_vector(3 downto 0);
        --DBG_CRTC_CLKEN	:	out std_logic;
        --DBG_PIXEL_CLKEN	:	out std_logic

		-- Clocks
		CLK_16M     :   in  std_logic;
		CLK_8M		:	out std_logic;			
		CLK_4M		:	out std_logic;
		CLK_2M		:	out std_logic;
		CLK_1M		:	out std_logic;		
        CLK_CRTC  	:   out std_logic;

        -- Bus interface
        nCS         :   in  std_logic;
        A           :   in  std_logic;
        D      		:   in  std_logic_vector(7 downto 0);
		  
        -- Control interface
        nINVERT     :   in  std_logic;
        DISEN       :   in  std_logic;
        CURSOR      :   in  std_logic;

        -- Video in (teletext mode)
        R_IN        :   in  std_logic;
        G_IN        :   in  std_logic;
        B_IN        :   in  std_logic;

        -- Video out
        R           :   out std_logic;
        G           :   out std_logic;
        B           :   out std_logic		
		);
end entity;

architecture rtl of vidproc is

	-- ULA standard write-only registers
    signal r0_cursor0       :   std_logic := '0';
    signal r0_cursor1       :   std_logic := '0';
    signal r0_cursor2       :   std_logic := '0';
    signal r0_crtc_2mhz     :   std_logic := '0';
    signal r0_pixel_rate    :   std_logic_vector(1 downto 0) := "00";
    signal r0_teletext      :   std_logic := '0';
    signal r0_flash         :   std_logic := '0';

	-- Palette memory
    type palette_t is array(0 to 15) of std_logic_vector(3 downto 0);
    signal palette          :   palette_t;

	-- Pixel shift register
    signal pixel_reg        :   std_logic_vector(7 downto 0);

	-- CRTC display enable
    signal crtc_disen    	:   std_logic;

	-- Internal clock enable generation
    signal pixel_clken      :   std_logic;
    signal crtc_clken       :   std_logic;
	signal r0_clken			:	std_logic;

    signal clk_counter    	:   unsigned(3 downto 0) := "0000";
	
	-- Cursor generation - can span up to 32 pixels
	-- Segments 0 and 1 are 8 pixels wide
	-- Segment 2 is 16 pixels wide
    signal cursor_invert    :   std_logic_vector(3 downto 0);
    signal cursor_active    :   std_logic;
    signal cursor_counter   :   unsigned(1 downto 0);

    signal RR               :   std_logic;
    signal GG               :   std_logic;
    signal BB               :   std_logic;
	
begin

    -- clock divider
    process (CLK_16M)
    begin
        if rising_edge(CLK_16M) then
            clk_counter <= clk_counter + 1;
        end if;
    end process;
	--DBG_CLK_CNT <= std_logic_vector(clk_counter);

	-- Output divided clocks
	CLK_8M <= clk_counter(0);
	CLK_4M <= clk_counter(1);
	CLK_2M <= clk_counter(2);
	CLK_1M <= clk_counter(3);

    -- R0 register write clock enable generation, !CS low on rising edge of 2Mhz clock.
	r0_clken <= '1' when (nCS = '0' and clk_counter(2 downto 0) = "100") else '0';
		
    -- Asynchronous register access
    process(CLK_16M)
    begin
        if rising_edge(CLK_16M) then
			if r0_clken = '1' then
				if A = '0' then
					-- Access control register
					r0_cursor0 		<= D(7);
					r0_cursor1 		<= D(6);
					r0_cursor2 		<= D(5);
					r0_crtc_2mhz 	<= D(4);
					r0_pixel_rate 	<= D(3 downto 2);
					r0_teletext 	<= D(1);
					r0_flash 		<= D(0);
				else
					-- Access palette register
					palette(to_integer(unsigned(D(7 downto 4)))) <= D(3 downto 0);	
				end if;
			end if;
        end if;
    end process;
	
	-- Output CRTC clock, either 2M or 1M
	CLK_CRTC <= clk_counter(2) when r0_crtc_2mhz = '1' else clk_counter(3);
		
    -- The result is fetched from the CRTC on falling edge of CLK_CRTC
    crtc_clken <= '1' when (r0_crtc_2mhz = '1' and clk_counter(2 downto 0) = "000") or (r0_crtc_2mhz = '0' and clk_counter(3 downto 0) = "0000") else '0';
	--DBG_CRTC_CLKEN <= crtc_clken;
	
	-- Pixel clock enable generation.
    -- Pixel clock can be divided by 1,2,4 or 8 depending on the value
    -- programmed at r0_pixel_rate
    -- 00 = /8, 01 = /4, 10 = /2, 11 = /1
    pixel_clken <= '1'                                           when r0_pixel_rate = "11" else
			       (not clk_counter(0))                          when r0_pixel_rate = "10" else
				   (not (clk_counter(0) or clk_counter(1)))      when r0_pixel_rate = "01" else
				   (not (clk_counter(0) or clk_counter(1) or clk_counter(2)));
	--DBG_PIXEL_CLKEN <= pixel_clken;
						  
    -- Fetch control
    process(CLK_16M)
    begin
        if rising_edge(CLK_16M) then	
            if pixel_clken = '1' then
                if crtc_clken = '1' then
                    -- Fetch next byte from RAM into shift register.
                    pixel_reg <= D;
					
					-- Sample DISEN
					crtc_disen <= DISEN;
                else
                    -- Clock shift register and input '1' at LSB
                    pixel_reg <= pixel_reg(6 downto 0) & '1';
                end if;
            end if;
        end if;
    end process;

    -- Cursor generation
    cursor_invert(0) <= cursor_active and
                     ((r0_cursor0 and not (cursor_counter(0) or      cursor_counter(1))) or
                      (r0_cursor1 and      cursor_counter(0) and not cursor_counter(1))  or
                      (r0_cursor2 and      cursor_counter(1)));

    process(CLK_16M)
    begin
        if rising_edge(CLK_16M) then
		
			cursor_invert(1) <= cursor_invert(0);
			cursor_invert(2) <= cursor_invert(1);
			cursor_invert(3) <= cursor_invert(2);
			
            if crtc_clken = '1' then
			
                if CURSOR = '1' or cursor_active = '1' then
                    -- Latch cursor
                    cursor_active <= '1';

                    -- Reset on counter wrap
                    if cursor_counter = "11" then
                        cursor_active <= '0';
                    end if;

                    -- Increment counter
                    if cursor_active = '0' then
                        -- Reset
                        cursor_counter <= (others => '0');
                    else
                        -- Increment
                        cursor_counter <= cursor_counter + 1;
                    end if;
                end if;
            end if;
        end if;
    end process;

    -- Pixel generation
    -- The new shift register contents are loaded during
    -- cycle 0 (and 8) but will not be read here until the next cycle.
    -- By running this process on every single video tick instead of at
    -- the pixel rate we ensure that the resulting delay is minimal and
    -- constant (running this at the pixel rate would cause
    -- the display to move slightly depending on which mode was selected).
    process(CLK_16M)
        variable log_col : std_logic_vector(3 downto 0);
        variable phy_col : std_logic_vector(3 downto 0);
        variable red_val : std_logic;
        variable green_val : std_logic;
        variable blue_val : std_logic;
    begin
        if rising_edge(CLK_16M) then

			-- Extract logical colour from pixel shift register
			log_col := pixel_reg(7) & pixel_reg(5) & pixel_reg(3) & pixel_reg(1);

			-- Look up dot value in the palette.  Bits are as follows:
			-- bit 3 - FLASH
			-- bit 2 - Not BLUE
			-- bit 1 - Not GREEN
			-- bit 0 - Not RED
			phy_col := palette(to_integer(unsigned(log_col)));

			-- Apply flash inversion if required
			red_val   := (phy_col(3) and r0_flash) xor not nINVERT xor not phy_col(0);
			green_val := (phy_col(3) and r0_flash) xor not nINVERT xor not phy_col(1);
			blue_val  := (phy_col(3) and r0_flash) xor not nINVERT xor not phy_col(2);
			
			-- To output
			-- FIXME: INVERT option
			RR <= (red_val   and crtc_disen) xor cursor_invert(0);
			GG <= (green_val and crtc_disen) xor cursor_invert(0);
			BB <= (blue_val  and crtc_disen) xor cursor_invert(0);

		end if;
    end process;

    -- Allow the 12MHz teletext signals to pass through without re-sampling
    R <= RR when r0_teletext = '0' else R_IN xor cursor_invert(3);
    G <= GG when r0_teletext = '0' else G_IN xor cursor_invert(3);
    B <= BB when r0_teletext = '0' else B_IN xor cursor_invert(3);

end architecture;