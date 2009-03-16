--------------------------------------------------------------------------------
--                                                                            --
--                          V H D L    F I L E                                --
--                          COPYRIGHT (C) 2009                                --
--                                                                            --
--------------------------------------------------------------------------------
--
-- Title       : JPEG_PKG
-- Design      : JPEG_ENC
-- Author      : Michal Krepa
--
--------------------------------------------------------------------------------
--
-- File        : JPEG_PKG.VHD
-- Created     : Sat Mar 7 2009
--
--------------------------------------------------------------------------------
--
--  Description : Package for JPEG core
--
--------------------------------------------------------------------------------

library IEEE;
  use IEEE.STD_LOGIC_1164.all;
  use ieee.numeric_std.all;
  
package JPEG_PKG is

  constant C_HDR_SIZE  : integer      := 338;
  
  constant C_MAX_LINE_WIDTH : integer := 1280;
    
  type T_SM_SETTINGS is record
    x_cnt               : unsigned(15 downto 0);
    y_cnt               : unsigned(15 downto 0);
    cmp_idx             : unsigned(1 downto 0);
  end record;
  
  constant C_SM_SETTINGS : T_SM_SETTINGS := 
  (
    (others => '0'),
    (others => '0'),
    (others => '0')
  );
  
  function log2(n : natural) return natural;
  
end package JPEG_PKG;

package body JPEG_PKG is

  -----------------------------------------------------------------------------
  function log2(n : natural) 
  return natural is
  begin
    for i in 0 to 31 loop
      if (2**i) >= n then
        return i;
      end if;
    end loop;
    return 32;
  end log2;
  -----------------------------------------------------------------------------

end package body JPEG_PKG;