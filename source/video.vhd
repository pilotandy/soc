----------------------------------------------------------------------------------
-- video.vhd
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

library XESS;
use XESS.CommonPckg.all;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library unisim;
use unisim.VComponents.all;

entity video is
  Port ( 
    --master clock
    clk        : in std_logic;
    clk_raw    : in std_logic;                      -- 12 Mhz domain clock
    rst        : in std_logic;
    
    -- Video / Memory Interface
    addr     : out std_logic_vector(23 downto 0);
    data     : in  std_logic_vector(15 downto 0);
    wr       : in  std_logic;
    rd       : out std_logic;
    rd_begun : in  std_logic;
    
    --VLDS output
    CK1IN_p    : out  STD_LOGIC;
    CK1IN_n    : out  STD_LOGIC;
    RXIN2_p    : out  STD_LOGIC;
    RXIN2_n    : out  STD_LOGIC;
    RXIN1_p    : out  STD_LOGIC;
    RXIN1_n    : out  STD_LOGIC;
    RXIN0_p    : out  STD_LOGIC;
    RXIN0_n    : out  STD_LOGIC
  );
end video;

architecture arch of video is

  -- NEW - fast clock, 12 Mhz (clk_in) x 13 = 156 Mhz, which gives us a pixel clock of 22.25 Mhz
  signal clk_100_ubuf : std_logic;
  signal clk_100 : std_logic;
  signal clk_157_ubuf : std_logic;
  signal clk_fast : std_logic;
  
  -- working pixel
  signal pixel : std_logic_vector(15 downto 0);
  
  -- colors
  signal red : std_logic_vector(5 downto 0) := "000000";
  signal green : std_logic_vector(5 downto 0) := "000000";
  signal blue : std_logic_vector(5 downto 0) := "000000";
  
  -- which slot are we in right now?
  signal slot : integer range 0 to 6;
  
  -- control signals
  signal hsync : std_logic := '0';
  signal vsync : std_logic := '0';
  signal dataenable : std_logic := '0';
  
  -- display parameters
  constant htotal : integer := 1200; -- screen size, with back porch
  constant hfront : integer := 0; -- front porch
  constant hactive : integer := 1024; -- display size
  signal hcurrent : integer range 0 to htotal := 0;
  constant vtotal : integer := 625; -- screen size, with back porch
  constant vfront : integer := 0; -- front porch
  constant vactive : integer := 600; -- display size
  signal vcurrent : integer range 0 to vtotal := vtotal - 1;
  
  -- the signals holding the data to be sent to the lcd on each slot.
  -- this is hardwired on the RGB, hsync and vsync signals.
  signal RX0DATA : std_logic_vector(0 to 6) := "0000000";
  signal RX1DATA : std_logic_vector(0 to 6) := "0000000";
  signal RX2DATA : std_logic_vector(0 to 6) := "0000000";
  constant CK1DATA : std_logic_vector(0 to 6) := "1100011"; -- this is per spec, the clock 
                                          -- is always the same 
  -- internal LVDS signal inputs
  signal CK1IN : std_logic;
  signal RXIN0 : std_logic;
  signal RXIN1 : std_logic;
  signal RXIN2 : std_logic;
  
  --video fifo
  signal fifo_rst    : std_logic;
  signal fifo_rd     : std_logic;
  signal fifo_out    : std_logic_vector(15 downto 0);
  signal fifo_low    : std_logic;
  signal fifo_full   : std_logic;
  signal fifo_empty  : std_logic;
  
  --double framing
  signal eof : std_logic;
  signal frame : std_logic;
  
  
  -- address pointer
  signal mem_addr : std_logic_vector(23 downto 0);
  
  signal change_frame : std_logic := '0';

begin

clk_1_dcm : DCM_SP
  generic map (  
    CLKFX_MULTIPLY => 25,   -- 12 Mhz * (25 / 3) = 100 Mhz
    CLKFX_DIVIDE => 3
  )
  port map (
    CLKFX => clk_100_ubuf,
    CLKIN => clk_raw,
    RST => '0'
  );
clk_100_buf: BUFG port map( I => clk_100_ubuf, O => clk_100);

clk_157_dcm : DCM_SP
  generic map (  
    CLKFX_MULTIPLY => 11,   -- 100 Mhz * (11/7) = 157.14 Mhz
    CLKFX_DIVIDE => 7
  )
  port map (
    CLKFX => clk_157_ubuf,
    CLKIN => clk_100,
    RST => '0'
  );
clk_157_buf: BUFG port map( I => clk_157_ubuf, O => clk_fast);

OBUFDS_CK1IN_inst : OBUFDS
  generic map (IOSTANDARD => "BLVDS_25")
  port map (
    O => CK1IN_p,    -- Diff_p output (connect directly to top-level port)
    OB => CK1IN_n,   -- Diff_n output (connect directly to top-level port)
    I => CK1IN       -- Buffer input 
  );

OBUFDS_RXIN0_inst : OBUFDS
  generic map (IOSTANDARD => "BLVDS_25")
  port map (
    O => RXIN0_p,    -- Diff_p output (connect directly to top-level port)
    OB => RXIN0_n,   -- Diff_n output (connect directly to top-level port)
    I => RXIN0       -- Buffer input 
  );

OBUFDS_RXIN1_inst : OBUFDS
  generic map (IOSTANDARD => "BLVDS_25")
  port map (
    O => RXIN1_p,    -- Diff_p output (connect directly to top-level port)
    OB => RXIN1_n,   -- Diff_n output (connect directly to top-level port)
    I => RXIN1       -- Buffer input 
  );

OBUFDS_RXIN2_inst : OBUFDS
  generic map (IOSTANDARD => "BLVDS_25")
  port map (
    O => RXIN2_p,    -- Diff_p output (connect directly to top-level port)
    OB => RXIN2_n,   -- Diff_n output (connect directly to top-level port)
    I => RXIN2       -- Buffer input 
  );

-- data enable: should be high when the data is valid for display
dataenable <= vsync and hsync;

-- RX2DATA is (DE, vsync, hsync, blue[5:2])
RX2DATA(0) <= dataenable;
RX2DATA(1) <= vsync;
RX2DATA(2) <= hsync;
RX2DATA(3 to 6) <= blue(5 downto 2);

-- RX1DATA is (blue[1:0], green[5:1])
RX1DATA(0 to 1) <= blue(1 downto 0);
RX1DATA(2 to 6) <= green(5 downto 1);

-- RX1DATA is (green[0], red[5:0])
RX0DATA(0) <= green(0);
RX0DATA(1 to 6) <= red(5 downto 0);

-- connect signals with the appropriate slot
RXIN0 <= RX0DATA(slot);
RXIN1 <= RX1DATA(slot);
RXIN2 <= RX2DATA(slot);
CK1IN <= CK1DATA(slot);


addr <= mem_addr;

-- 100 Mhz side
process(clk, rst)
begin
    if rst = YES then
        frame <= '0';
    elsif rising_edge(clk) then
        if change_frame = YES then
            frame <= not frame;
        end if;
    end if;
end process;
    
    
process(clk, rst)
begin
    if rst = YES then
        rd <= NO;
        mem_addr <= x"ED4000";
    elsif rising_edge(clk) then
        if eof = YES then
            if frame = '0' then
                mem_addr <= x"ED4000";
            else
                mem_addr <= x"F6A000";
            end if;
        end if;
        
        -- start reading when FIFO is low
        if fifo_low = YES and eof = NO then
            rd <= YES;
        end if;
        
        -- read 256 words
        if rd_begun = YES then
            mem_addr <= mem_addr + 1;   
            if mem_addr(7 downto 0) = "11111111" then
                rd <= NO;
            end if;
        end if;
    end if;
end process;

--video_fifo
fifo : entity work.fifo_1024x16_ic
    port map (
    rst    => fifo_rst,
    wr_clk => clk,
    rd_clk => clk_fast,
    din    => data,
    wr_en  => wr,
    rd_en  => fifo_rd,
    dout   => fifo_out,
    full   => fifo_full,
    empty  => fifo_empty,
    prog_empty  => fifo_low
);


-- 157 Mhz side

pixel <= fifo_out;
   
process(clk_fast, rst) is
begin
    if rst = YES then
        eof <= YES;
        fifo_rst <= YES;
        fifo_rd <= NO;
    elsif rising_edge(clk_fast) then
        fifo_rst <= NO;
        eof <= NO;
        fifo_rd <= NO;
    
        -- end of frame
        if vcurrent = (vfront+vactive) then
            fifo_rst <= YES;
            eof <= YES;
        end if;
 
        if slot = 0 then 
            --prefetch first pixel
            if hcurrent = htotal - 1 and vcurrent = vtotal - 1 then
                fifo_rd <= YES;
            end if;
      
            --get the next pixel
            if hcurrent < hactive and vcurrent < vactive then
                fifo_rd <= YES;
            end if;
 
            if hcurrent = htotal-1 then
                hcurrent <= 0;
                -- if this is the last line in the screen, wrap around.
                if vcurrent = vtotal - 1 then
                    vcurrent <= 0;
                else
                    vcurrent <= vcurrent + 1;
                end if;
            else
                hcurrent <= hcurrent + 1;
            end if;
        end if;

        if slot = 6 then
            -- this is the last slot, wrap around
            slot <= 0;
      
            -- set hsync and vsync
            if hcurrent < hfront or (hcurrent >= (hfront+hactive)) then
                hsync <= '0';
            else
                hsync <= '1';
            end if;
            if vcurrent < vfront or (vcurrent >= (vfront+vactive)) then
                vsync <= '0';
            else
                vsync <= '1';
            end if;
      
            -- set red green and blue components      
            red <= pixel(15 downto 11) & "0";
            green <= pixel(10 downto 5);
            blue <= pixel(4 downto 0) & "0";
        else
            slot <= slot + 1;
        end if;
    end if;
end process;
end arch;
