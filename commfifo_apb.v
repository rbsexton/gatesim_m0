//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Abstract : Communications FIFO
//-----------------------------------------------------------------------------
//-------------------------------------
// Programmer's model
//  0x000   R/W    FIFO RW
//  0x004     R    H2D Status
//                 Bit1: Error  
//                 Bit0: Data available to read.  
//  0x008     R    D2H Status
//                 Bit1: Error  
//                 Bit0: Room for write 
//
// These are two 'WRITE' ufifos.

module commfifo_apb(
  // IO declaration
  input  wire                    PCLK,     // clock
  input  wire                    PRESETn,  // reset

  // APB interface inputs
  input  wire                    PSEL,
  input  wire [11:2]             PADDR,
  input  wire                    PENABLE,
  input  wire                    PWRITE,
  input  wire [7:0]             PWDATA,

  // APB interface outputs
  output wire  [31:0]            PRDATA,
  output wire                    PREADY,
  output wire                    PSLVERR

  );

  // These are not published ports.  They exist to interface with 
  // Cosimulation routines. 
  reg             d2h_host_rd            /*verilator public_flat_rw @(posedge PCLK) */;
  wire  [7:0]     d2h_host_data          /*verilator public_flat_rd*/ ;
  wire            d2h_host_not_empty     /*verilator public_flat_rd*/ ;

  reg             h2d_host_wr            /*verilator public_flat_rw @(posedge PCLK) */;
  reg       [7:0] h2d_dut_din            /*verilator public_flat_rw @(posedge PCLK) */;
  wire            h2d_host_not_full      /*verilator public_flat_rd*/;


//------------------------------------------------------------------------------
// internal wires
//------------------------------------------------------------------------------

  wire [15:0] h2d_status;
  wire        h2d_err;
  wire        h2d_rd;
  wire        h2d_empty_n;
  wire  [7:0] h2d_data;
  
  wire [15:0] d2h_status;
  wire        d2h_wr;
  wire        d2h_err;
  
// --------------------------------------------------------------------
// Instantiate FIFOs
// --------------------------------------------------------------------
// Instantiate two Units Under Test (UUT)
// with shared inputs.
ufifo #( .BW(8),
    .LGFLEN(6), // Log FIFO Length.   
    .RXFIFO(0) ) 
  commfifo_h2d ( .i_clk(PCLK), .i_reset(!PRESETn),
    
    .i_wr     (h2d_host_wr),
    .i_data   (h2d_dut_din),
    .o_empty_n(h2d_empty_n),
    
    .i_rd     (h2d_rd),  
    .o_data   (h2d_data), 
    .o_status (h2d_status),
    .o_err    (h2d_err)  
  );


// The DUT 2 Host FIFO
// and the input with PSEL so that its easier to work with the traces.
ufifo #( .BW(8),
    .LGFLEN(6), // Log FIFO Length.   
    .RXFIFO(0) )
  commfifo_d2h ( .i_clk(PCLK), .i_reset(!PRESETn),
    
    .i_wr     (d2h_wr),   
    .i_data   (PWDATA[7:0] & {8{PSEL}} ), 
    .o_empty_n(d2h_host_not_empty),  
    
    .i_rd     (d2h_host_rd),
    .o_data   (d2h_host_data),
    .o_status (d2h_status),
    .o_err    (d2h_err)

  );

assign h2d_host_not_full = h2d_status[0];

//------------------------------------------------------------------------------
// Main code
//------------------------------------------------------------------------------
wire select0 = (PADDR == 10'd0); // FIFO 
wire select1 = (PADDR == 10'd1); // Host to DUT Status
wire select2 = (PADDR == 10'd2); // DUT to Host Status 

// DUT to host FIFO Writes 
assign d2h_wr = select0 & PSEL & PENABLE & PWRITE; 

// Host to DUT - DUT Read strobe
assign h2d_rd = select0 & PSEL & PENABLE & ~PWRITE; 

// Read data multiplexer
wire [31:0] read_data_mux = 
  ( {32{select0}} & { 24'b0 , h2d_data} ) |
  ( {32{select1}} & { 30'b0 , h2d_err, h2d_empty_n} ) |
  ( {32{select2}} & { 30'b0 , d2h_err, d2h_status[0]} );

assign PRDATA  = (PSEL & PENABLE & (~PWRITE)) ? read_data_mux : {32{1'b0}};
  
// Always ready, never an error.
assign PSLVERR = 1'b0;
assign PREADY  = 1'b1;

// ------------------------------------------------------------------
// Integration with Veri lator 

// ------------------------------------------------
// veril ator public
// Publish the interface signals.
//`ifdef verilator

  // The DUT to Host interface.

//   function [0:0] get_d2h_host_not_empty;
//      get_d2h_host_not_empty = d2h_host_not_empty;
//   endfunction // get_d2h_host_not_empty

//   function [7:0] get_d2h_host_data;
//      get_d2h_host_data = d2h_host_data;
//    endfunction // get_d2h_host_data
      
//   function void set_d2h_host_rd;
//      input data;
      
//      d2h_host_rd = data;
//   endfunction // set_d2h_host_rd

   // Host to DUT 
//   function [0:0] get_h2d_host_not_full;
//      get_h2d_host_not_full = h2d_host_not_full;
//   endfunction // get_h2d_host_not_full
//   
//   function void set_h2d_host_data;
//      input [7:0] data; 
//      h2d_dut_din = data;
//    endfunction // set_h2d_host_data
//      
//   function void set_h2d_host_wr;
//      input data;
//      h2d_host_wr = data;
//   endfunction // set_h2d_host_wr
//`endif


endmodule
