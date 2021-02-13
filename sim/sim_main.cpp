#include <iostream>

#include "Vcmsdk_mcu.h"
// #include "Vm6809_integration_m6809_integration.h"
// #include "Vm6809_integration.h"


#include <verilated_vcd_c.h>

#include "verilated.h"

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

      Vcmsdk_mcu *top = new Vcmsdk_mcu;

      top->NRST  = 0;
      top->XTAL1 = 0;        

      while (!Verilated::gotFinish() && main_time < 1000 ) {
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

           // Read outputs
           if ( top->XTAL1 ) printf("Addr: %04x\n",top->HADDR_mon);
           //VL_PRINTF("[%" VL_PRI64 "d] clk=%x rstl=%x iquad=%" VL_PRI64 "x"
           //         " -> oquad=%" VL_PRI64 "x owide=%x_%08x_%08x\n",
           //           main_time, top->clk, top->reset_l, top->in_quad, top->out_quad, top->out_wide[2],
          //         top->out_wide[1], top->out_wide[0]);
       }

       // Final model cleanup
       top->final();
}
