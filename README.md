# Simulating the Cortex-M0 with Verilator

## Prerequisites 

- ARM Cortex-M0 Designstart files.  This project was originally 
built with *AT510-MN-80001-r2p0-00rel0*.   The default project 
from ARM includes a few peripherals and loads *image.hex* into 
memory at address 0x0.
- image.hex file with your binary for execution.  One byte per line, in hex:
```
80
7a
00
89
00
```

```
hexdump -v -e '1/0 "" 1/1 "%02x" 1/0 "\n"' binary.bin 
```
## Running the simuation.

- cd to *sim* and run *make* 
- From the top level directory, run *./sim/obj_dir/Vcmsdk_mcu*.   The simulator 
will ingest the image.hex file and run.    It will produce a *cmsdk_mcu.vcd* file that 
you can inspect with gtkWave.      The distribution includes a basic MPE Forth image.
- 

The *./sim* directory contains a makefile to verilate the project into c++ and 
run it.   Setting up and running verilator is way beyond the scope of this document.

## Verilator notes

Verilator appears to flatten the design.  It doesn't give you easy ways to look at internal 
signals.   The approved way to do that is with functions in verilog.

## Memory Map 

The default memory map is defined in *./AT510-MN-80001-r2p0-00rel0/systems/cortex_m0_mcu/verilog/cmsdk_mcu_addr_decode.v*

- 0x4000:000 APB defined by cmsdk_apb_slave_mux
     -  timer 0
     -  timer 1
     -  dual timer 0
     -  :3000 // CommFIFO
     -  // uart 0
     -  // uart 1
     -  // uart 2
     -  // not used
     -  // watchdog
     -  // not used
     -  // not used
     -  (INCLUDE_APB_TEST_SLAVE), // test slave for validation purpose
     -  (APB_EXT_PORT12_ENABLE),
     -  (APB_EXT_PORT13_ENABLE),
     -  (APB_EXT_PORT14_ENABLE),
     -  (APB_EXT_PORT15_ENABLE)
- 0x4001:0000 GPIO0 
- 0x4001:1000 GPIO1 
- 0x4001:F000 System Control 
- 0xF000:0000 System ROM 




## UART FIFO interface

Encoding/Decoding of UART bits consumes compute and simulation 
time that isn't necessary.  The cosimulation implements the following interface for communication with the 
host environment.  

Base address: ```0x4800:0000```

```
  Offsets:
  0x000 Write to host (8-bit)
  0x004 Write to host free bytes 

  0x010 Read from host (8-bit)
  0x014 Read from host available byte count
```
   
## Revision History 

- 2021 Feb 13  First Draft

The default project has a .hex file that you can use as a sample. 
