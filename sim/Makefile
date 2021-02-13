run: obj_dir/Vcmsdk_mcu
	obj_dir/Vcmsdk_mcu +trace | head -100

obj_dir/Vcmsdk_mcu: ../cmsdk_mcu.v sim_main.cpp
	verilator -Wall -Wno-fatal --trace \
	--cc cmsdk_mcu.v --exe sim_main.cpp \
	-I..  \
	-I../AT510-MN-80001-r2p0-00rel0/systems/cortex_m0_mcu/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/models/memories \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_apb_watchdog/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_apb_dualtimers/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/systems/cortex_m0_mcu/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/cores/cortexm0_designstart_r2p0/logical/cortexm0_integration/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_ahb_slave_mux/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_ahb_default_slave/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_ahb_gpio/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_apb_subsystem/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_apb_slave_mux/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_apb_timer/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_apb_uart/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_apb_dualtimers/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_apb_watchdog/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_iop_gpio/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/cmsdk_ahb_to_apb/verilog \
	-I../AT510-MN-80001-r2p0-00rel0/logical/models/memories
	make -C obj_dir -j -f Vcmsdk_mcu.mk Vcmsdk_mcu
