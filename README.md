# Simulating the Cortex-M0 with Verilator

## Prerequisites 

- ARM Cortex-M0 Designstart files.  This project was originally 
built with *AT510-MN-80001-r2p0-00rel0*.   The default project 
from ARM includes a few peripherals and loads *image.hex* into 
memory at address 0x9.
- image.hex file with your binary for execution.  One byte per line, in hex:
```
80
7a
00
20
9d
00
00
00
89
00
```

## Running the simuation.

- cd to *sim* and run *make* 
- From the top level directory, run *./sim/obj_dir/Vcmsdk_mcu*.   The simulator 
will ingest the image.hex file and run.    It will produce a *cmsdk_mcu.vcd* file that 
you can inspect with gtkWave.   
- 

The *./sim* directory contains a makefile to verilate the project into c++ and 
run it.   Setting up and running verilator is way beyond the scope of this document.

## Verilator notes

Verilator appears to flatten the design.  It doesn't give you easy ways to look at internal 
signals.   The approved way to do that is with functions in verilog.

## Revision History 

- 2021 Feb 13  First Draft

The default project has a .hex file that you can use as a sample. 