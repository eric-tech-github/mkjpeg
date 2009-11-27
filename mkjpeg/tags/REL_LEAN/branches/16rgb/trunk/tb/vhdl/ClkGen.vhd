

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----------------------------------- LIBRARY/PACKAGE ---------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- generic packages/libraries:
-------------------------------------------------------------------------------
library ieee;
	use ieee.std_logic_1164.all;
	use ieee.numeric_std.all;

-------------------------------------------------------------------------------
-- user packages/libraries:
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----------------------------------- ENTITY ------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
entity ClkGen is
	port (
	     CLK                             : out  std_logic;
	     RST                             : out  std_logic
	     );
end entity ClkGen;


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----------------------------------- ARCHITECTURE ------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
architecture ClkGen_rtl of ClkGen is


	constant CLOCK_PERIOD : time := 10 ns;

	signal clk_s : std_logic := '0'; 
	signal rst_s : std_logic := '0'; 
	

begin

  -- Clock generator (50% duty cycle)
	clk_gen: process
	begin
		clk_s <= '0';
		wait for CLOCK_PERIOD/2;
		clk_s <= '1';
		wait for CLOCK_PERIOD/2;
	end process clk_gen;
  
	CLK <= clk_s;
  

	reset_gen: process
	begin
		wait until rising_edge(clk_s);
		rst_s <= '0';
		wait until rising_edge(clk_s);
		rst_s <= '1';
		wait until rising_edge(clk_s);
		rst_s <= '0';
		wait;
	end process reset_gen;

	RST <= rst_s;


end architecture ClkGen_rtl;