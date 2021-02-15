#!/bin/sh

ARMIP=
LOCALIP=


iverilog -g2009  -o tb_cmsdk_mcu  -s tb_cmsdk_mcu -cm0.cfg  

