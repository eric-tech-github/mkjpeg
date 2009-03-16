-------------------------------------------------------------------------------
-- File Name :  Huffman.vhd
--
-- Project   : JPEG_ENC
--
-- Module    : Huffman
--
-- Content   : Huffman Encoder
--
-- Description : Huffman encoder core
--
-- Spec.     : 
--
-- Author    : Michal Krepa
--
-------------------------------------------------------------------------------
-- History :
-- 20090228: (MK): Initial Creation.
-------------------------------------------------------------------------------

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
entity Huffman is
  port 
  (
        CLK                : in  std_logic;
        RST                : in  std_logic;
        -- CTRL
        start_pb           : in  std_logic;
        ready_pb           : out std_logic;
        
        -- HOST IF
        sof                : in  std_logic;
        img_size_x         : in  std_logic_vector(15 downto 0);
        img_size_y         : in  std_logic_vector(15 downto 0);
        cmp_max            : in  std_logic_vector(1 downto 0);
        
        -- RLE
        rle_buf_sel        : out std_logic;
        rd_en              : out std_logic;
        runlength          : in  std_logic_vector(3 downto 0);
        VLI_size           : in  std_logic_vector(3 downto 0);
        VLI                : in  std_logic_vector(11 downto 0);
        d_val              : in  std_logic;
        rle_fifo_empty     : in  std_logic;
     
        -- Byte Stuffer
        bs_buf_sel         : in  std_logic;
        bs_fifo_empty      : out std_logic;
        bs_rd_req          : in  std_logic;
        bs_packed_byte     : out std_logic_vector(7 downto 0)        
    );
end entity Huffman;

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
----------------------------------- ARCHITECTURE ------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
architecture RTL of Huffman is

  constant C_M             : integer := 34;
  constant BLK_SIZE        : integer := 64;

  type T_STATE is (IDLE, RUN_VLC, PAD);
  type T_VLX_IDX_ARR       is array(0 to C_M-1) of unsigned(4 downto 0);
  type T_WORD_IDX_ARR      is array(0 to C_M-1) of unsigned(5 downto 0);

  signal state             : T_STATE;
  signal rle_buf_sel_s     : std_logic;
  signal word_reg          : unsigned(C_M-1 downto 0);
  signal bit_ptr           : unsigned(5 downto 0);
  signal num_fifo_wrs      : unsigned(2 downto 0);
  signal VLI_ext           : unsigned(15 downto 0);
  signal VLI_ext_size      : unsigned(4 downto 0);
  signal ready_HFW         : std_logic;
  signal fifo_wbyte        : std_logic_vector(7 downto 0);
  signal fifo_wrt_cnt      : unsigned(2 downto 0);
  signal fifo_wrt_cnt_d1   : unsigned(2 downto 0);
  signal fifo_wren         : std_logic;
  signal last_block        : std_logic;
  signal image_area_size   : unsigned(33 downto 0);
  signal block_cnt         : unsigned(27 downto 0);
  signal VLC_size          : unsigned(4 downto 0);
  signal VLC               : unsigned(15 downto 0);
  signal VLC_DC_size_ext   : unsigned(4 downto 0);
  signal VLC_DC_ext        : unsigned(15 downto 0);
  signal VLC_DC_size       : std_logic_vector(3 downto 0);
  signal VLC_DC            : unsigned(8 downto 0);
  signal VLC_AC_size       : unsigned(4 downto 0);
  signal VLC_AC            : unsigned(15 downto 0);
  signal d_val_d1          : std_logic;    
  signal d_val_d2          : std_logic;
  signal d_val_d3          : std_logic;
  signal d_val_d4          : std_logic;
  signal VLI_size_d        : std_logic_vector(3 downto 0);
  signal VLI_d             : std_logic_vector(11 downto 0);
  signal VLI_size_d1       : std_logic_vector(3 downto 0);
  signal VLI_d1            : std_logic_vector(11 downto 0);
  signal HFW_running       : std_logic;
  signal runlength_r       : std_logic_vector(3 downto 0);
  signal VLI_size_r        : std_logic_vector(3 downto 0);
  signal VLI_r             : std_logic_vector(11 downto 0);
  signal dc_idx            : std_logic;
  signal VLC_plus_VLI_size : unsigned(4 downto 0);
  signal word_idx          : T_WORD_IDX_ARR;
  signal vlx_idx           : T_VLX_IDX_ARR;
  signal VLC_d             : unsigned(15 downto 0);
  signal VLC_size_d        : unsigned(4 downto 0);
  signal VLI_ext_d         : unsigned(15 downto 0);
  signal VLI_ext_size_d    : unsigned(4 downto 0);
  signal pad_reg           : std_logic;
  signal pad_reg_d1        : std_logic;
  signal pad_byte          : std_logic_vector(7 downto 0);
  signal word_reg_d1       : unsigned(C_M-1 downto 0);
  signal fifo_wren_d1      : std_logic;
  signal pad_byte_d1       : std_logic_vector(7 downto 0);
  
-------------------------------------------------------------------------------
-- Architecture: begin
-------------------------------------------------------------------------------
begin

  rle_buf_sel <= rle_buf_sel_s;
  
  -------------------------------------------------------------------
  -- latch FIFO Q
  -------------------------------------------------------------------
  p_latch_fifo : process(CLK, RST)
  begin
    if RST = '1' then
      VLI_size_r  <= (others => '0'); 
      VLI_r       <= (others => '0');
      runlength_r <= (others => '0');
    elsif CLK'event and CLK = '1' then
      if d_val = '1' then
        runlength_r <= runlength;
        VLI_size_r  <= VLI_size; 
        VLI_r       <= VLI;            
      end if;
    end if;
  end process;
  
  -------------------------------------------------------------------
  -- DC_ROM
  -------------------------------------------------------------------
  U_DC_ROM : entity work.DC_ROM
  port map
  (
        CLK                => CLK,
        RST                => RST,
        VLI_size           => VLI_size,
                           
        VLC_DC_size        => VLC_DC_size,
        VLC_DC             => VLC_DC
  );
    
  -------------------------------------------------------------------
  -- AC_ROM
  -------------------------------------------------------------------
  U_AC_ROM : entity work.AC_ROM
  port map
  (
        CLK                => CLK,
        RST                => RST,
        runlength          => runlength,
        VLI_size           => VLI_size,
                           
        VLC_AC_size        => VLC_AC_size,
        VLC_AC             => VLC_AC
    );
   
  -------------------------------------------------------------------
  -- Double Fifo
  -------------------------------------------------------------------
  U_DoubleFifo : entity work.DoubleFifo
  port map
  (
        CLK                => CLK,
        RST                => RST,
        -- HUFFMAN
        data_in            => fifo_wbyte,
        wren               => fifo_wren_d1,
        -- BYTE STUFFER
        buf_sel            => bs_buf_sel,
        rd_req             => bs_rd_req,
        fifo_empty         => bs_fifo_empty,
        data_out           => bs_packed_byte
    );
  
  -------------------------------------------------------------------
  -- RLE buf_sel
  -------------------------------------------------------------------
  p_rle_buf_sel : process(CLK, RST)
  begin
    if RST = '1' then
      rle_buf_sel_s   <= '0'; 
    elsif CLK'event and CLK = '1' then
      if start_pb = '1' then
        rle_buf_sel_s <= not rle_buf_sel_s;
      end if;
    end if;
  end process;
  
  -------------------------------------------------------------------
  -- mux for DC/AC ROM
  -------------------------------------------------------------------
  p_mux : process(CLK, RST)
  begin
    if RST = '1' then
      VLC_size <= (others => '0');
      VLC      <= (others => '0');
    elsif CLK'event and CLK = '1' then
      if dc_idx = '1' then
        VLC_size <= unsigned('0' & VLC_DC_size);
        VLC      <= resize(VLC_DC, VLC'length);
      else
        VLC_size <= VLC_AC_size;
        VLC      <= VLC_AC;
      end if;
    end if;
  end process;
  
  
  
  -------------------------------------------------------------------
  -- Block Counter / Last Block detector
  -------------------------------------------------------------------
  p_blk_cnt : process(CLK, RST)
  begin
    if RST = '1' then
      image_area_size <= (others => '0');
      last_block      <= '0';
      block_cnt       <= (others => '0');
    elsif CLK'event and CLK = '1' then
      image_area_size <= unsigned(cmp_max)*
                         unsigned(img_size_x)*unsigned(img_size_y);
      
      if sof = '1' then
        block_cnt <= (others => '0');
      elsif start_pb = '1' then
        block_cnt <= block_cnt + 1;
      end if;
      
      if block_cnt = image_area_size(33 downto 6) then
        last_block <= '1';
      else
        last_block <= '0';
      end if;
      
    end if;
  end process;
  
  VLI_ext      <= unsigned("0000" & VLI_d);
  VLI_ext_size <= unsigned('0' & VLI_size_d);
  
  -------------------------------------------------------------------
  -- delay line
  -------------------------------------------------------------------
  p_vli_dly : process(CLK, RST)
  begin
    if RST = '1' then
      VLI_d       <= (others => '0');
      VLI_size_d  <= (others => '0');
      VLI_d1      <= (others => '0');
      VLI_size_d1 <= (others => '0');
      d_val_d1    <= '0';
      d_val_d2    <= '0';
      d_val_d3    <= '0';
      d_val_d4    <= '0';
    elsif CLK'event and CLK = '1' then
      VLI_d1      <= VLI;
      VLI_size_d1 <= VLI_size;
      
      VLI_d       <= VLI;
      VLI_size_d  <= VLI_size;
      
      d_val_d1   <= d_val;
      d_val_d2   <= d_val_d1;
      d_val_d3   <= d_val_d2;
      d_val_d4   <= d_val_d3;
    end if;
  end process;
  
  -------------------------------------------------------------------
  -- HandleFifoWrites
  -------------------------------------------------------------------
  p_HandleFifoWrites : process(CLK, RST)
  begin
    if RST = '1' then
      ready_HFW    <= '0';
      fifo_wrt_cnt <= (others => '0');
      fifo_wren    <= '0';
      fifo_wbyte   <= (others => '0');
      rd_en        <= '0';
      dc_idx       <= '0';
    elsif CLK'event and CLK = '1' then
      fifo_wren <= '0';
      ready_HFW <= '0';
      rd_en     <= '0';
      fifo_wren_d1 <= fifo_wren;
      fifo_wrt_cnt_d1 <= fifo_wrt_cnt;
      
      if state = IDLE and start_pb = '1' then
        rd_en       <= '1';
        dc_idx      <= '1';
      end if;
    
      if HFW_running = '1' and ready_HFW = '0' then
        -- there is no at least one integer byte to write this time
        if num_fifo_wrs = 0 then
          ready_HFW    <= '1';
          rd_en        <= '1' and not rle_fifo_empty;
        -- single byte write to FIFO
        else
          fifo_wrt_cnt <= fifo_wrt_cnt + 1;
          fifo_wren    <= '1';

          -- last byte write
          if fifo_wrt_cnt + 1 = num_fifo_wrs then
            ready_HFW    <= '1';
            fifo_wrt_cnt <= (others => '0');
            rd_en        <= '1' and not rle_fifo_empty;
          end if;
        end if;
      end if;
      
      case fifo_wrt_cnt_d1 is 
        when "000" =>
          fifo_wbyte <= std_logic_vector(word_reg_d1(C_M-1 downto C_M-8));
        when "001" =>
          fifo_wbyte <= std_logic_vector(word_reg_d1(C_M-8-1 downto C_M-16));
        when "010" =>
          fifo_wbyte <= std_logic_vector(word_reg_d1(C_M-16-1 downto C_M-24));  
        when "011" =>
          fifo_wbyte <= std_logic_vector(word_reg_d1(C_M-24-1 downto C_M-32)); 
        when others =>
          fifo_wbyte <= (others => '0');
      end case;
      if pad_reg_d1 = '1' then
        fifo_wbyte <= pad_byte_d1;
      end if;
      
      if ready_HFW = '1' then
        dc_idx <= '0';
      end if;
    end if;
  end process;
  
  -- divide by 8
  num_fifo_wrs <= bit_ptr(5 downto 3);
  
  -------------------------------------------------------------------
  -- Variable Length Processor FSM
  -------------------------------------------------------------------
  p_vlp : process(CLK, RST)
  begin
    if RST = '1' then
      ready_pb     <= '0';
      state        <= IDLE;
      word_reg     <= (others => '0');
      bit_ptr      <= (others => '0');
      HFW_running  <= '0';
      VLC_plus_VLI_size <= (others => '0');
      VLC_d             <= (others => '0');
      VLC_size_d        <= (others => '0');
      VLI_ext_d         <= (others => '0');
      VLI_ext_size_d    <= (others => '0');
      word_reg_d1       <= (others => '0');
      pad_byte_d1       <= (others => '0');
    elsif CLK'event and CLK = '1' then
      ready_pb   <= '0';
      pad_reg    <= '0';
      
      word_reg_d1    <= word_reg;
      pad_byte_d1    <= pad_byte;
      pad_reg_d1     <= pad_reg;
      
      VLC_plus_VLI_size <= resize(VLC_size,5) + resize(VLI_ext_size,5);
      VLC_d             <= VLC;
      VLC_size_d        <= VLC_size;
      VLI_ext_d         <= VLI_ext;
      VLI_ext_size_d    <= VLI_ext_size;
      
      for i in 0 to C_M-1 loop
        if d_val_d2 = '1' then
          if i < to_integer(VLC_size) then
            vlx_idx(i) <= resize(VLC_size,vlx_idx(0)'length)-
                          to_unsigned(1+i,vlx_idx(0)'length);
            word_idx(i)  <= to_unsigned(C_M-1-i, word_idx(0)'length)-
                            resize(bit_ptr,word_idx(0)'length);
          elsif i < VLC_size + VLI_ext_size then
            vlx_idx(i) <= resize(VLC_size,vlx_idx(0)'length)+
                          resize(VLI_ext_size,vlx_idx(0)'length)-
                          to_unsigned(1+i,vlx_idx(0)'length);
            word_idx(i) <= to_unsigned(C_M-1-i, word_idx(0)'length)-
                           resize(bit_ptr,word_idx(0)'length);
          end if;
        elsif d_val_d3 = '1' then
          if i < to_integer(VLC_size_d) then
            word_reg(to_integer(word_idx(i))) <= VLC_d(to_integer(vlx_idx(i)));
          elsif i < VLC_plus_VLI_size then
            word_reg(to_integer(word_idx(i))) <= VLI_ext_d(to_integer(vlx_idx(i)));
          end if;
        elsif ready_HFW = '1' then
          -- shift word reg left to skip bytes already written to FIFO
          word_reg <= shift_left(word_reg, to_integer(num_fifo_wrs & "000"));
        end if;
      end loop;
        
      case state is
      
        when IDLE =>
          if start_pb = '1' then
            state       <= RUN_VLC;
          end if;
        
        when RUN_VLC =>
          -- data valid DC or data valid AC
          if d_val_d3 = '1' then
            bit_ptr <= bit_ptr + resize(VLC_size_d,bit_ptr'length) + 
                       resize(VLI_ext_size_d,bit_ptr'length);
            
            -- HandleFifoWrites
            HFW_running  <= '1';
          -- HandleFifoWrites completed
          elsif ready_HFW = '1' then
            -- shift word reg left to skip bytes already written to FIFO
            --word_reg <= shift_left(word_reg, to_integer(num_fifo_wrs & "000"));
            -- adjust bit pointer after some bytes were written to FIFO
            -- modulo 8 operation
            bit_ptr <= bit_ptr - (num_fifo_wrs & "000"); 
            HFW_running <= '0';

            -- end of block
            if rle_fifo_empty = '1' then
              -- end of segment
              if bit_ptr - (num_fifo_wrs & "000") /= 0 and last_block = '1' then
                state <= PAD;
              else
                ready_pb <= '1';
                state    <= IDLE;
              end if;
            end if;
          end if;
          
        -- end of segment which requires bit padding
        when PAD =>
          if HFW_running = '0' then
            -- 1's bit padding to integer number of bytes            
            --for i in 0 to C_M-1 loop        
            --  if i < 8 then
            --     word_reg(C_M-1-to_integer(bit_ptr)-i) <= '1';
            --  end if;
            --end loop;
            
            for i in 0 to 7 loop
              if i < bit_ptr then
                pad_byte(7-i) <= word_reg(C_M-1-i);
              else
                pad_byte(7-i) <= '1';
              end if;
            end loop;
            
            pad_reg <= '1';
                     
            bit_ptr <= to_unsigned(8, bit_ptr'length);         

            -- HandleFifoWrites
            HFW_running <= '1';
         elsif ready_HFW = '1' then
           bit_ptr      <= (others => '0');
           HFW_running  <= '0';
           pad_reg      <= '0';
           
           ready_pb <= '1';
           state <= IDLE;
         end if;
        
        when others =>
        
      end case;
      
      if sof = '1' then
        bit_ptr <= (others => '0');
      end if;
      
    end if;
  end process;


end architecture RTL;
-------------------------------------------------------------------------------
-- Architecture: end
-------------------------------------------------------------------------------