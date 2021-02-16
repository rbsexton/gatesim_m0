#include <iostream>

#include <poll.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

#include "Vcmsdk_mcu.h"

#include <verilated_vcd_c.h>

#include "verilated.h"
#include "verilated_vpi.h"

// Current simulation time (64-bit unsigned)
vluint64_t main_time = 0;

// Called by $time in Verilog
double sc_time_stamp() {
    return main_time;  // Note does conversion to real, to match SystemC
}

int main(int argc, char **argv, char **env)
{

      // Verilator must compute traced signals
      Verilated::traceEverOn(true);

      // Pass arguments so Verilated code can see them, e.g. $value$plusargs
      // This needs to be called before you create any model
      Verilated::commandArgs(argc, argv);

      // Pull handles for the FIFO interfaces.
      //vpiHandle vh_d2h_dout = vpi_handle_by_name((PLI_BYTE8*)"TOP.cmsdk_mcu.u_cmsdk_mcu_system.u_apb_subsystem.u_commfifo.d2h_host_data", NULL);
      //if (!vh_d2h_dout) { vl_fatal(__FILE__, __LINE__, "sim_main", "No handle found"); }
      //const char* name = vpi_get_str(vpiName, vh_d2h_dout);
      //    printf("Module name: %s\n", name);  // Prints "readme"
      
      Vcmsdk_mcu *top = new Vcmsdk_mcu;
      
      top->NRST  = 0;
      top->XTAL1 = 0; 
      
      int read_state  = 0;
      int write_state = 0;
      
      struct pollfd p;
      p.fd = 0; // stdin 
      p.events = POLLIN;        

      while (!Verilated::gotFinish() && main_time < 10000 ) {
           main_time++;  // Time passes...

           // Toggle a fast (time/2 period) clock
           top->XTAL1 = !top->XTAL1;

           // Toggle control signals on an edge that doesn't correspond
           // to where the controls are sampled; in this example we do
           // this only on a negedge of clk, because we know
           // reset is not sampled there.
           if (!top->XTAL1) {
              if (main_time > 1 && main_time < 5) {
                    top->NRST = !1;  // Assert reset
                } else {
                    top->NRST = !0;  // Deassert reset
                }
           }
           
           // Evaluate model
           top->eval();

           {             
             // We need a little state machine to do this neatly because the 
             // assertion of signals by code doesn't propagate instantly.
             // Detect(0) -> Strobe/Read(1) -> De-assert(2) -> Detect(0) 
             if ( top->XTAL1 ) {
               if ( top->cmsdk_mcu__DOT__u_cmsdk_mcu_system__DOT__u_apb_subsystem__DOT__u_commfifo__DOT__d2h_host_not_empty &&
                    read_state == 0) {
               read_state = 1;
              } 
            }
            
            else if ( read_state == 1 && !top->XTAL1 ) {
               printf("%c",
                top->cmsdk_mcu__DOT__u_cmsdk_mcu_system__DOT__u_apb_subsystem__DOT__u_commfifo__DOT__d2h_host_data );

               top->cmsdk_mcu__DOT__u_cmsdk_mcu_system__DOT__u_apb_subsystem__DOT__u_commfifo__DOT__d2h_host_rd = 1;
               read_state = 2; 
             }
             
             else if ( read_state == 2 && !top->XTAL1 ) {
               // de-assert read strobe
               top->cmsdk_mcu__DOT__u_cmsdk_mcu_system__DOT__u_apb_subsystem__DOT__u_commfifo__DOT__d2h_host_rd = 0;
               read_state = 0; 
             }
           } // Read data state machine.

           // Data from stdin.
           {
             if ( ( main_time > 100 ) && top->XTAL1 ) {
                if (  write_state == 1 ) { // De-assert the write strobe 
                  top->cmsdk_mcu__DOT__u_cmsdk_mcu_system__DOT__u_apb_subsystem__DOT__u_commfifo__DOT__h2d_host_wr = 0;
                  write_state = 0;
                } 
                else {
                  int ret = poll(&p,1,0);
                  if ( ret < 0) { perror("poll() error"); exit(1); }

                  if ( ( ret > 0 ) && (p.revents & POLLIN) ) {
                    char buf[1];
                    int ret = read(0,buf,1); // Read from stdin
                    if ( ret == 1 ) {  
                      //printf("Processing %c", buf[0]);
                      top->cmsdk_mcu__DOT__u_cmsdk_mcu_system__DOT__u_apb_subsystem__DOT__u_commfifo__DOT__h2d_dut_din = buf[0];
                      top->cmsdk_mcu__DOT__u_cmsdk_mcu_system__DOT__u_apb_subsystem__DOT__u_commfifo__DOT__h2d_host_wr = 1;
                      write_state = 1;   
                    }
                  }
                
                } 
             }
           }
         
           // Read outputs
           // if ( top->XTAL1 ) printf("Addr: %04x\n",top->HADDR_mon);
           //VL_PRINTF("[%" VL_PRI64 "d] clk=%x rstl=%x iquad=%" VL_PRI64 "x"
           //         " -> oquad=%" VL_PRI64 "x owide=%x_%08x_%08x\n",
           //           main_time, top->clk, top->reset_l, top->in_quad, top->out_quad, top->out_wide[2],
          //         top->out_wide[1], top->out_wide[0]);
       }

       // Final model cleanup
       top->final();
}
