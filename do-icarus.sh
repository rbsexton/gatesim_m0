#!/bin/sh

ARMIP=
LOCALIP=


iverilog -o tb_cm0  -s tb_cmsdk_mcu -cm0.cfg  

