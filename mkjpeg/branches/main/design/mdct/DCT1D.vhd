--------------------------------------------------------------------------------
--                                                                            --
--                          V H D L    F I L E                                --
--                          COPYRIGHT (C) 2006                                --
--                                                                            --
--------------------------------------------------------------------------------
--
-- Title       : DCT1D
-- Design      : MDCT Core
-- Author      : Michal Krepa
--
--------------------------------------------------------------------------------
--
-- File        : DCT1D.VHD
-- Created     : Sat Mar 5 7:37 2006
--
--------------------------------------------------------------------------------
--
--  Description : 1D Discrete Cosine Transform (1st stage)
--
--------------------------------------------------------------------------------


library IEEE;
  use IEEE.STD_LOGIC_1164.all;
  use IEEE.NUMERIC_STD.all; 

library WORK;
  use WORK.MDCT_PKG.all;

--------------------------------------------------------------------------------
-- ENTITY
--------------------------------------------------------------------------------
entity DCT1D is	 
	port(	  
		  clk          : in STD_LOGIC;  
		  rst          : in std_logic;
      dcti         : in std_logic_vector(IP_W-1 downto 0);
      idv          : in STD_LOGIC;
      romedatao    : in T_ROM1DATAO;
      romodatao    : in T_ROM1DATAO;

      odv          : out STD_LOGIC;
      dcto         : out std_logic_vector(OP_W-1 downto 0);
      romeaddro    : out T_ROM1ADDRO;
      romoaddro    : out T_ROM1ADDRO;
      ramwaddro    : out STD_LOGIC_VECTOR(RAMADRR_W-1 downto 0);
      ramdatai     : out STD_LOGIC_VECTOR(RAMDATA_W-1 downto 0);
      ramwe        : out STD_LOGIC;
      wmemsel      : out STD_LOGIC		
		);
end DCT1D;

--------------------------------------------------------------------------------
-- ARCHITECTURE
--------------------------------------------------------------------------------
architecture RTL of DCT1D is   
  
  type INPUT_DATA is array (N-1 downto 0) of SIGNED(IP_W downto 0);
  
  signal databuf_reg     : INPUT_DATA;
  signal latchbuf_reg    : INPUT_DATA;
  signal col_reg         : UNSIGNED(RAMADRR_W/2-1 downto 0);
  signal row_reg         : UNSIGNED(RAMADRR_W/2-1 downto 0);
  signal rowr_reg        : UNSIGNED(RAMADRR_W/2-1 downto 0);
  signal inpcnt_reg      : UNSIGNED(RAMADRR_W/2-1 downto 0);
  signal dcto_1          : STD_LOGIC_VECTOR(DA_W-1 downto 0);
  signal dcto_2          : STD_LOGIC_VECTOR(DA_W-1 downto 0);
  signal ramwe_s         : STD_LOGIC;
  signal ramwe_d1        : STD_LOGIC;
  signal ramwe_d2        : STD_LOGIC;
  signal wmemsel_reg     : STD_LOGIC;	
  signal wmemsel_d1      : STD_LOGIC;	
  signal wmemsel_d2      : STD_LOGIC;	
  signal stage2_reg      : STD_LOGIC; 
  signal stage2_cnt_reg  : UNSIGNED(RAMADRR_W-1 downto 0); 
  signal col_2_reg       : UNSIGNED(RAMADRR_W/2-1 downto 0); 
  signal ramwaddro_s     : STD_LOGIC_VECTOR(RAMADRR_W-1 downto 0);
  signal ramwaddro_d1    : STD_LOGIC_VECTOR(RAMADRR_W-1 downto 0);
  signal ramwaddro_d2    : STD_LOGIC_VECTOR(RAMADRR_W-1 downto 0);
  signal even_not_odd    : std_logic;
  signal even_not_odd_d1 : std_logic;
  signal romedatao_d1    : T_ROM1DATAO;
  signal romodatao_d1    : T_ROM1DATAO;
  
begin

  ramwaddro <= ramwaddro_d2;
  ramwe     <= ramwe_d2;
  ramdatai  <= dcto_2(DA_W-1 downto 12);
  wmemsel   <= wmemsel_d2;
 
  process(clk,rst)
  begin
    if rst = '1' then
      inpcnt_reg      <= (others => '0');
      latchbuf_reg    <= (others => (others => '0')); 
      databuf_reg     <= (others => (others => '0'));
      stage2_reg      <= '0';
      stage2_cnt_reg  <= (others => '1');
      dcto_1          <= (others => '0');
      dcto_2          <= (others => '0');
      ramwe_s         <= '0';
      ramwe_d1        <= '0';
      ramwe_d2        <= '0';
      ramwaddro_s     <= (others => '0');
      ramwaddro_d1    <= (others => '0');
      ramwaddro_d2    <= (others => '0');
      col_reg         <= (others => '0');
      row_reg         <= (others => '0');
      wmemsel_reg     <= '0';
      wmemsel_d1      <= '0';
      wmemsel_d2      <= '0';
      col_2_reg       <= (others => '0'); 
      ramwaddro_d1    <= (others => '0'); 
      even_not_odd    <= '0';
      even_not_odd_d1 <= '0';
    elsif clk = '1' and clk'event then
      stage2_reg     <= '0';
      ramwe_s        <= '0';
 
      --------------------------------
      -- 1st stage
      --------------------------------
      if idv = '1' then
      
        inpcnt_reg    <= inpcnt_reg + 1;

        -- right shift input data
        latchbuf_reg(N-2 downto 0) <= latchbuf_reg(N-1 downto 1);
        latchbuf_reg(N-1)          <= SIGNED('0' & dcti) - LEVEL_SHIFT;

        if inpcnt_reg = N-1 then
          -- after this sum databuf_reg is in range of -256 to 254 (min to max) 
          databuf_reg(0)  <= latchbuf_reg(1)+(SIGNED('0' & dcti) - LEVEL_SHIFT);
          databuf_reg(1)  <= latchbuf_reg(2)+latchbuf_reg(7);
          databuf_reg(2)  <= latchbuf_reg(3)+latchbuf_reg(6);
          databuf_reg(3)  <= latchbuf_reg(4)+latchbuf_reg(5);
          databuf_reg(4)  <= latchbuf_reg(1)-(SIGNED('0' & dcti) - LEVEL_SHIFT);
          databuf_reg(5)  <= latchbuf_reg(2)-latchbuf_reg(7);
          databuf_reg(6)  <= latchbuf_reg(3)-latchbuf_reg(6);
          databuf_reg(7)  <= latchbuf_reg(4)-latchbuf_reg(5);
          stage2_reg      <= '1';
        end if;
      end if;
      --------------------------------
      
      --------------------------------
      -- 2nd stage
      --------------------------------
      if stage2_cnt_reg < N then
        
        stage2_cnt_reg <= stage2_cnt_reg + 1;
        
        -- write RAM
        ramwe_s   <= '1';
        -- reverse col/row order for transposition purpose
        ramwaddro_s <= STD_LOGIC_VECTOR(col_2_reg & row_reg);
        -- increment column counter
        col_reg   <= col_reg + 1;
        col_2_reg <= col_2_reg + 1;
        
        -- finished processing one input row
        if col_reg = 0 then
          row_reg         <= row_reg + 1;
          -- switch to 2nd memory
          if row_reg = N - 1 then
            wmemsel_reg <= not wmemsel_reg;
            col_reg         <= (others => '0');
          end if;
        end if;  
 
      end if;
      
      if stage2_reg = '1' then
        stage2_cnt_reg <= (others => '0');
        col_reg        <= (0=>'1',others => '0');
        col_2_reg      <= (others => '0');
      end if;
      ----------------------------------   
      
      even_not_odd    <= stage2_cnt_reg(0);
      even_not_odd_d1 <= even_not_odd;
      ramwe_d1        <= ramwe_s;
      ramwe_d2        <= ramwe_d1;
      ramwaddro_d1    <= ramwaddro_s;
      ramwaddro_d2    <= ramwaddro_d1;
      wmemsel_d1      <= wmemsel_reg;
      wmemsel_d2      <= wmemsel_d1;
      
      if even_not_odd = '0' then
        dcto_1 <= STD_LOGIC_VECTOR(RESIZE
          (RESIZE(SIGNED(romedatao(0)),DA_W) + 
          (RESIZE(SIGNED(romedatao(1)),DA_W-1) & '0') +
          (RESIZE(SIGNED(romedatao(2)),DA_W-2) & "00") + 
          (RESIZE(SIGNED(romedatao(3)),DA_W-3) & "000") +
          (RESIZE(SIGNED(romedatao(4)),DA_W-4) & "0000"),
          DA_W));
      else
        dcto_1 <= STD_LOGIC_VECTOR(RESIZE
          (RESIZE(SIGNED(romodatao(0)),DA_W) + 
          (RESIZE(SIGNED(romodatao(1)),DA_W-1) & '0') +
          (RESIZE(SIGNED(romodatao(2)),DA_W-2) & "00") + 
          (RESIZE(SIGNED(romodatao(3)),DA_W-3) & "000") +
          (RESIZE(SIGNED(romodatao(4)),DA_W-4) & "0000"),
          DA_W));
      end if;
      
      if even_not_odd_d1 = '0' then
        dcto_2 <= STD_LOGIC_VECTOR(RESIZE
          (signed(dcto_1) +
          (RESIZE(SIGNED(romedatao_d1(5)),DA_W-5) & "00000") +
          (RESIZE(SIGNED(romedatao_d1(6)),DA_W-6) & "000000") + 
          (RESIZE(SIGNED(romedatao_d1(7)),DA_W-7) & "0000000") -
          (RESIZE(SIGNED(romedatao_d1(8)),DA_W-8) & "00000000"),
          DA_W));
      else
        dcto_2 <= STD_LOGIC_VECTOR(RESIZE
          (signed(dcto_1) + 
          (RESIZE(SIGNED(romodatao_d1(5)),DA_W-5) & "00000") +
          (RESIZE(SIGNED(romodatao_d1(6)),DA_W-6) & "000000") + 
          (RESIZE(SIGNED(romodatao_d1(7)),DA_W-7) & "0000000") -
          (RESIZE(SIGNED(romodatao_d1(8)),DA_W-8) & "00000000"),
          DA_W));
      end if;
    end if;
  end process;
  
  -- read precomputed MAC results from LUT
  p_romaddr : process(CLK, RST)
  begin
    if RST = '1' then
      romeaddro   <= (others => (others => '0')); 
      romoaddro   <= (others => (others => '0')); 
    elsif CLK'event and CLK = '1' then
      for i in 0 to 8 loop
        -- even
        romeaddro(i) <= STD_LOGIC_VECTOR(col_reg(RAMADRR_W/2-1 downto 1)) & 
                 databuf_reg(0)(i) & 
                 databuf_reg(1)(i) &
                 databuf_reg(2)(i) &
                 databuf_reg(3)(i);
        -- odd
        romoaddro(i) <= STD_LOGIC_VECTOR(col_reg(RAMADRR_W/2-1 downto 1)) & 
                 databuf_reg(4)(i) & 
                 databuf_reg(5)(i) &
                 databuf_reg(6)(i) &
                 databuf_reg(7)(i);
      end loop;
    end if;
  end process; 
  
  p_romdatao_d1 : process(CLK, RST)
  begin
    if RST = '1' then
      romedatao_d1    <= (others => (others => '0'));
      romodatao_d1    <= (others => (others => '0'));
    elsif CLK'event and CLK = '1' then
      romedatao_d1   <= romedatao;
      romodatao_d1   <= romodatao;
    end if;
  end process;
  
end RTL;
--------------------------------------------------------------------------------
