//
// ddram.v
// Copyright (c) 2017 Sorgelig
//
//
// This source file is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version. 
//
// This source file is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of 
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License 
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// ------------------------------------------
//

// 16-bit version
// Some mods by Alynna
// Thank you @grablosaure for rewriting this 
// I removed the instantiation parameter logic, so I can use
// one module for multiple devices.
// Caveats:
// This module lets you access all 512m of the HPS DDR3.
// $20000000-$3FFFFFFF
// At the very least, ASCAL uses $20000000-$21FFFFFF
// It is smart to reserve $20000000-$27FFFFFF for MiSTer functionality.
// I recommend working your way down from the top of memory ($3FFFFFFF).
module ddram
(
	input         DDRAM_CLK,
	
	input         DDRAM_BUSY, // waitrequest
	output [7:0]  DDRAM_BURSTCNT, // burstcount
	output [28:0] DDRAM_ADDR, // address (in HPS reserved memory $20000000-$3FFFFFFF)
	input  [63:0] DDRAM_DOUT, // readdata
	input         DDRAM_DOUT_READY, // readdatavalid
	output        DDRAM_RD, // read
	output [63:0] DDRAM_DIN, // readdata
	output [7:0]  DDRAM_BE, // byteenable
	output        DDRAM_WE, // write
	
	input [29:0]  wr_a,
	input [15:0]  wr_d,
	input [1:0]   wr_be,
	input         wr_req,
	output        wr_ack,
	
	
	input  [29:0] rd_a,
	output [15:0] rd_d,
	input         rd_req,
	output        rd_ack
);

// I will probably be implementing a write cache.
// Some of these new regs are for this.
reg [7:0]     ram_burst;
reg [63:0]    ram_rq, next_rq, ram_wq;
reg [63:0]    ram_data;
reg [29:0]    ram_address, cache_addr, wcache_addr;
reg           ram_read = 0;
reg           ram_write = 0;
reg [7:0]     ram_be, ram_wbe;
reg           wr_pend = 0;
reg [1:0]     rd_pend = 0;
reg           rc_valid;
reg           wc_valid;

reg    rd_acki,wr_acki;
assign rd_ack=rd_acki;
assign wr_ack=wr_acki;

// Constrain assignments to $20xxxxxx-$3Fxxxxxx.  Lower addresses will echo to upper ones.
// Addresses sent to this module represent their exact address in HPS RAM.
assign DDRAM_ADDR     = {3'b001,ram_address[28:3]};
assign DDRAM_BURSTCNT = ram_burst;
assign DDRAM_BE       = ram_be;
assign DDRAM_RD       = ram_read;
assign DDRAM_DIN      = ram_data;
assign DDRAM_WE       = ram_write;

assign rd_d = ram_rq[{rd_a[2:1], 4'b0000} +:16];

always @(posedge DDRAM_CLK) begin
	if (!rd_req && wr_req && !wr_pend) begin
		ram_data <= {4{wr_d}};
		ram_address <= {wr_a[29:3],3'b000};
		ram_be   <= ({6'b0,wr_be} << {wr_a[2:1],1'b0});
		ram_write<=1;
		ram_burst<=1;
		wr_pend  <=1;
		rc_valid <=0;
		wr_acki  <=0;
	end else if (rd_req && cache_addr[29:3] == rd_a[29:3] && rc_valid) begin
		rd_acki <= 1;      
	end else if (rd_req && rd_pend==2'b00 && (cache_addr[29:3]+1'd1) == rd_a[29:3] && rc_valid) begin
		rd_acki     <= 1;
		ram_rq       <= next_rq;
		cache_addr  <= {rd_a[29:3],3'b000};
		ram_address <= {rd_a[29:3]+1'd1,3'b000};
		ram_read    <= 1;
		ram_be      <= {8{1'b1}};
		ram_burst   <= 1;
		rd_pend     <= 1;
	end else if (rd_req && rd_pend==0) begin
		ram_address <= {rd_a[29:3],3'b000};
		ram_be      <= {8{1'b1}};
		cache_addr  <= {rd_a[29:3],3'b000};
		ram_read    <= 1;
		ram_burst   <= 2;
		rd_pend     <= 2;
		rc_valid    <= 0;
	end		
	if (!DDRAM_BUSY && ram_write) begin
		  ram_write<=0;
		  wr_pend<=0;
   	  wr_acki<=1;
	end
	if (!wr_req) wr_acki<=0;
	if (!rd_req) rd_acki<=0;
	if (!DDRAM_BUSY && ram_read) ram_read<=0;
	if (DDRAM_DOUT_READY) begin
		rd_pend <= rd_pend-1'b1;
		if (rd_pend == 2'b10) begin 
			ram_rq  <= DDRAM_DOUT; 
			rd_acki<=1;
		end
		if (rd_pend == 2'b01) begin
			next_rq <= DDRAM_DOUT;
			rc_valid<=1;
		end
	end
end
endmodule
