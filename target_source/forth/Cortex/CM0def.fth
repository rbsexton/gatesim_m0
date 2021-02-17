\ CortexDef.fth - default defininitions for Cortex CPUs

((
Copyright (c) 2009
MicroProcessor Engineering
133 Hill Lane
Southampton SO15 5AF
England

tel: +44 (0)23 8063 1441
fax: +44 (0)23 8033 9691
net: mpe@mpeforth.com
     tech-support@mpeforth.com
web: www.mpeforth.com

From North America, our telephone and fax numbers are:
  011 44 23 8063 1441
  011 44 23 8033 9691


Absolute addresses start with an '_' underscore character.
Offsets and masks do not.


To do
=====

Change history
==============
))


\ ***************
\ Version numbers
\ ***************

 char 7 equ mpe-rel		\ x in Vx.yz
 char 1 equ mpe-ver		\ y in Vx.yz
 char 0 equ usrver		\ z in Vx.yz

\ ***********
\ Bit equates
\ ***********

$00000001 equ BIT0
$00000002 equ BIT1
$00000004 equ BIT2
$00000008 equ BIT3
$00000010 equ BIT4
$00000020 equ BIT5
$00000040 equ BIT6
$00000080 equ BIT7
$00000100 equ BIT8
$00000200 equ BIT9
$00000400 equ BIT10
$00000800 equ BIT11
$00001000 equ BIT12
$00002000 equ BIT13
$00004000 equ BIT14
$00008000 equ BIT15
$00010000 equ BIT16
$00020000 equ BIT17
$00040000 equ BIT18
$00080000 equ BIT19
$00100000 equ BIT20
$00200000 equ BIT21
$00400000 equ BIT22
$00800000 equ BIT23
$01000000 equ BIT24
$02000000 equ BIT25
$04000000 equ BIT26
$08000000 equ BIT27
$10000000 equ BIT28
$20000000 equ BIT29
$40000000 equ BIT30
$80000000 equ BIT31


\ *******
\ Cortex-M0 CPU
\ *******

\ =======================
\ CPSR and mode switching
\ =======================

\ CPSR/SPSR definitions _F _S _X _C
\ _F Flag bits (31..24)
bit31 equ N_bit		\ negative
bit30 equ Z_bit		\ zero
bit29 equ C_bit		\ carry
bit28 equ V_bit		\ overflow
bit27 equ Q_bit		\ sticky overflow (saturation)

bit24 equ T_bit		\ Jazelle


\ ***************************************
\ Interrupt handler equates for IntCortex.fth
\ ***************************************

0 equ StackVec#		\ holds initial stack pointer
1 equ ResetVec#		\ reset
2 equ NMIVec#		\ NMI
3 equ HardVec#		\ hard fault
4 equ MemVec#		\ memory fault
5 equ BusVec#		\ bus fault
6 equ UseVec#		\ usage fault

#11 equ SvcVec#		\ SVC/SWI
#12 equ DbgVec#		\ Debug, e.g. breakpoint

#14 equ PendVec#	\ Pendable request
#15 equ SysTickVec#	\ System Ticker

\ Vectors 16..255 are implementation defined


\ ******************************
\ Standard Cortex-M0 peripherals
\ ******************************

$E000:E000 equ _SCS	\ -- addr
\ Base of System Control Space

  $0004 equ scsICTR	\ Interrupt Controller Type Register - r/o

  $0010 equ stCSR	\ systick Control and Status Register
  $0014 equ stRVR	\         Reload Value Register
  $0018 equ stCVR	\         Current Value Register
  $001C equ stCalib	\         Calibration value register

  $0100 equ nvSetEnR0	\ nvic    Set Enable Reg 0..7
  $0180 equ nvClrEnR0	\         Clear Enable reg 0..7
  $0200 equ nvSetPendR0 \        Set Pending Reg 0..7
  $0280 equ nvClrPendR0 \        Clr Pending Reg 0..7
\  $0300 equ nvABR0	\         Active Bit Reg 0..7
  $0400 equ nvPR0	\         Priority Reg 0..

  $0D00 equ scsCPUID	\ sys     CPUID Base Register
  $0D04 equ scsICSR	\         Interrupt Control State Reg
\  $0D08 equ scsVTOR	\         Vector table Offset Reg
  $0D0C equ scsAIRCR	\         Application Interrupt/Reset Control Reg
  $0D10 equ scsSCR	\         System Control Reg
  $0D14 equ scsCCR	\         Configuration Control reg
\  $0D18 equ scsSHPRB0	\         System Handler Priority Reg 2
  $0D1C equ scsSHPR2	\         System Handler Priority Reg 2
  $0D20 equ scsSHPR3	\         System Handler Priority Reg 2


\ *******************************************
\ SYSm constants for MSR and MRS instructions
\ *******************************************

decimal

 0 equ APSR		\ just the flags
 5 equ IPSR		\ just the exception number
 6 equ EPSR		\ just the execution state

 1 equ IAPSR		\ flags + exception
 2 equ EAPSR		\ flags + execution state
 3 equ XPSR		\ flags + execution + exception
 7 equ IEPSR		\ exception + execution

 8 equ SP_main		\ Main stack pointer (exceptions)
 9 equ SP_process	\ Process stack pointer (threads)

16 equ PRIMASK		\ bit0=1 to enable NMI and HardFault
17 equ BASEPRI		\ 9 bit priority (lower=higher priority)
18 equ BASEPRI_MAX
19 equ FAULTMASK	\ bit0=1 to enable NMI only
20 equ CONTROL		\ bit1=1 for SP_process, bit0=1 for User state in thread mode
			\ we have bit0=0 always

