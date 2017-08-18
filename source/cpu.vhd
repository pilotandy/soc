library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;


library xess;
use xess.CommonPckg.all;

entity cpu16 is
    port (
        clk : in std_logic;
        rst : in std_logic;
        
        mem_addr     : out std_logic_vector(23 downto 0);
        mem_rd_data  : in  std_logic_vector(15 downto 0);
        mem_wr_data  : out std_logic_vector(15 downto 0);
        mem_rd       : out std_logic;
        mem_wr       : out std_logic;
        mem_begun    : in std_logic;
        mem_rd_done  : in std_logic;
        
        change_frame : out std_logic
    );
end cpu16;

architecture arch of cpu16 is

    type cpu_state_type is (fetch, decode, execute, update);
    signal cpu_state : cpu_state_type;
    
    signal pc       : std_logic_vector(23 downto 0);
    signal inst     : std_logic_vector(15 downto 0);
    
    signal video_addr : integer range 0 to (1024 * 600) - 1 := 0;
    constant frame_0_base : integer := 16#ED4000#;
    constant frame_1_base : integer := 16#F6A000#;
    signal frame_0 : std_logic;
    
begin

    process(clk, rst)
    begin
        if rst = YES then
            cpu_state <= fetch;
            pc <= x"096000";
            mem_addr <= (others => '0');
            mem_rd <= NO;
            mem_wr <= NO;
            change_frame <= NO;
            frame_0 <= YES;
        elsif rising_edge(clk) then
            mem_rd <= NO;
            mem_wr <= NO;
            change_frame <= NO;
            
            case cpu_state is
            when fetch =>
                --mem_addr <= pc;
                --mem_rd <= YES;
                
                --if mem_begun = YES then
                --    mem_rd <= NO;
                    cpu_state <= decode;
                --    pc <= pc + 1; 
                --end if;
                


            when decode =>
                -- if mem_rd_done = YES then
                --    inst <= mem_rd_data;
                    -- do some awesome lut in a PROM

                cpu_state <= execute;
                -- end if;
                
            when execute =>
                
                mem_wr <= YES;
                cpu_state <= update;
                video_addr <= video_addr + 1;
                if video_addr = 16#95FFF# then
                    video_addr <= 0;
                    change_frame <= YES;
                    frame_0 <= not frame_0;
                end if;
                
                if frame_0 = YES then
                    -- write to frame 1
                    mem_wr_data <= conv_std_logic_vector(video_addr / 8,16);
                    mem_addr <= conv_std_logic_vector(frame_1_base + video_addr, 24);
                else
                    --write to frame 0
                    mem_wr_data <= conv_std_logic_vector(video_addr / 8,16);
                    mem_addr <= conv_std_logic_vector(frame_0_base + video_addr, 24);
                end if;
                
            when update =>
                mem_wr <= YES;
                if mem_begun = YES then
                    mem_wr <= NO;
                    cpu_state <= fetch;    
                end if;
            
            end case;
        end if;
    
    end process;

end arch;

