----------------------------------------------------------------------------------
-- flighdisplay.vhd
-- This is the top heirarchy.
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

library unisim;
use unisim.vcomponents.all;

library xess;
use xess.CommonPckg.all;
use xess.SdramCntlPckg.all;
use xess.ClkgenPckg.all;
use xess.XessBoardPckg.all;

entity soc16 is
    generic(
        BASE_FREQ_G : real             := 12.0;   -- Base frequency in MHz.
        CLK_MUL_G   : natural          := 25;   -- Multiplier for base frequency.
        CLK_DIV_G   : natural          := 3;    -- Divider for base frequency.
        PIPE_EN_G   : boolean          := true; -- Enable SDRAM controller pipelining.
    
        LCD_WIDTH   : natural          := 1024;
        LCD_HEIGHT  : natural          := 600
    );
    port(
        --master clock
        clk_012m0 : in std_logic;
        --video 
        lcd_ck1in_p : out  STD_LOGIC;
        lcd_ck1in_n : out  STD_LOGIC;
        lcd_rxin2_p : out  STD_LOGIC;
        lcd_rxin2_n : out  STD_LOGIC;
        lcd_rxin1_p : out  STD_LOGIC;
        lcd_rxin1_n : out  STD_LOGIC;
        lcd_rxin0_p : out  STD_LOGIC;
        lcd_rxin0_n : out  STD_LOGIC;
   
        --SDRAM
        sd_clk  : out std_logic;           -- Clock 100 Mhz
        sd_clkfb: in  std_logic;           -- clock from SDRAM after PCB delays    
        sd_cke  : out std_logic;           -- Clock-enable to SDRAM
        sd_ce   : out std_logic;           -- Chip-select to SDRAM
        sd_ras  : out std_logic;           -- SDRAM row address strobe
        sd_cas  : out std_logic;           -- SDRAM column address strobe
        sd_we   : out std_logic;           -- SDRAM write enable
        sd_dqm  : out std_logic_vector(1 downto 0);     -- Enable SDRAM databus if true
        sd_addr : out std_logic_vector(12 downto 0);   -- SDRAM row/column address
        sd_ba   : out std_logic_vector(1 downto 0);    -- SDRAM bank address
        sd_data : inout std_logic_vector(15 downto 0); -- Data to/from SDRAM
    
        -- Simple UART
        uart_rx: in std_logic;
        uart_tx: out std_logic;
  
        --fifo status
        led    : out std_logic
    );
end soc16;

architecture arch of soc16 is
  constant FREQ_C : real := (BASE_FREQ_G * real(CLK_MUL_G)) / real(CLK_DIV_G);
  
  signal clk_100m0  : std_logic;
  signal clkP_s     : std_logic;
  signal clkN_s     : std_logic;
  
  signal led_status : std_logic := NO;

  --internal reset
  signal reset : std_logic := YES;
  signal reset_cnt : natural range 0 to 7;

  -- SDRAM->CPU interface
  signal ram_addr       : std_logic_vector(23 downto 0);
  signal ram_rd_data    : std_logic_vector(15 downto 0);
  signal ram_wr_data    : std_logic_vector(15 downto 0);
  signal ram_rd         : std_logic;
  signal ram_wr         : std_logic;
  signal ram_begun      : std_logic;
  signal ram_rd_done    : std_logic;
  signal ram_wr_done    : std_logic;
  
  -- SDRAM->VGA interface
  signal video_addr     : std_logic_vector(23 downto 0);
  signal video_data     : std_logic_vector(15 downto 0);
  signal video_rd       : std_logic;
  signal video_rd_done  : std_logic;
  signal video_rd_begun : std_logic;
  
  signal change_frame   : std_logic;

begin

    -- Generate a 100 MHz clock from the 12 MHz input clock.
    u0 : Clkgen
    generic map (BASE_FREQ_G => BASE_FREQ_G, CLK_MUL_G => CLK_MUL_G, CLK_DIV_G => CLK_DIV_G)
    port map(I               => clk_012m0, O => clkP_s, O_b => clkN_s);
    u1 : ClkToLogic
    port map(clk_i => clkP_s, clk_ib => clkN_s, clk_o => sd_clk);

    u2 : IBUFG port map( I => sd_clkfb, O => clk_100m0);  -- main clock is SDRAM clock fed back into FPGA
  
    -- Generate a reset signal for the SDRAM controller.  
    process(clk_100m0)
        constant reset_dly_c : natural                        := 10;
        variable rst_cntr    : natural range 0 to reset_dly_c := 0;
    begin
        if rising_edge(clk_100m0) then
            reset <= NO;
            if rst_cntr < reset_dly_c then
                reset <= YES;
                rst_cntr     := rst_cntr + 1;
            end if;
        end if;
    end process;

    uart_tx <= NO;
    led <= NO;
    
    cpu : entity work.cpu16
    port map (
        clk => clk_100m0,
        rst => reset,
        
        mem_addr => ram_addr,
        mem_rd_data => ram_rd_data,
        mem_wr_data => ram_wr_data,
        mem_rd => ram_rd,
        mem_wr => ram_wr,
        mem_begun => ram_begun,
        mem_rd_done => ram_rd_done,
        
        change_frame => change_frame
    );
    
    sdram : DualPortSdram
    generic map(
        PORT_TIME_SLOTS_G => "1111111111111111",  -- port 1 gets priority
        FREQ_G            => FREQ_C,
        PIPE_EN_G         => PIPE_EN_G  -- If true, enable pipelined read operations.
    )
    port map(
        clk_i             => clk_100m0,
  
        -- Host interface.
        rst0_i            => reset,
        rd0_i             => ram_rd,
        wr0_i             => ram_wr,
        earlyOpBegun0_o   => ram_begun,
        opBegun0_o        => open,
        rdPending0_o      => open,
        done0_o           => open,
        rdDone0_o         => ram_rd_done,
        addr0_i           => ram_addr,
        data0_i           => ram_wr_data,
        data0_o           => ram_rd_data,
        status0_o         => open,

        -- Video interface
        rst1_i            => reset,
        rd1_i             => video_rd,
        wr1_i             => NO,
        earlyOpBegun1_o   => video_rd_begun,
        opBegun1_o        => open,
        rdPending1_o      => open,
        done1_o           => open,
        rdDone1_o         => video_rd_done,
        addr1_i           => video_addr,
        data1_i           => X"0000",
        data1_o           => video_data,
        status1_o         => open,

        -- SDRAM side.
        sdCke_o           => sd_cke,
        sdCe_bo           => sd_ce,
        sdRas_bo          => sd_ras,
        sdCas_bo          => sd_cas,
        sdWe_bo           => sd_we,
        sdBs_o            => sd_ba,
        sdAddr_o          => sd_addr,
        sdData_io         => sd_data,
        sdDqmh_o          => sd_dqm(1),
        sdDqml_o          => sd_dqm(0)
    );
    
    video : entity work.video
    port map (
        --master clock
        clk       => clk_100m0,
        clk_raw   => clk_012m0,
        rst       => reset,
        
        -- Device Interface
        
        -- Video / Ram Interface
        addr => video_addr,
        data => video_data,
        wr   => video_rd_done,
        rd   => video_rd,
        rd_begun => video_rd_begun,

        --VLDS output
        CK1IN_p => lcd_ck1in_p,
        CK1IN_n => lcd_ck1in_n,
        RXIN2_p => lcd_rxin2_p,
        RXIN2_n => lcd_rxin2_n,
        RXIN1_p => lcd_rxin1_p,
        RXIN1_n => lcd_rxin1_n,
        RXIN0_p => lcd_rxin0_p,
        RXIN0_n => lcd_rxin0_n
    );

end arch;
