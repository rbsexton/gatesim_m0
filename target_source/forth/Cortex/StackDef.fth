\ StackDef.fth - default stack layout

((
Copyright (c) 2010
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


To do
=====

Change history
==============
))

decimal

\ ===========
\ *! stackdef
\ *T Default stack layout
\ ===========
\ *P The code in *\i{StackDef.fth} provides the default task
\ ** and stack reservations. It assumes that you will be using
\ ** tasks and high-level Forth interrupt handlers.

\ *P The Cortex-M3 has two stack pointers, SP_main and SP_process.
\ ** These are accessed as register R13 which is is also known as
\ ** RSP in the Forth assembler. After reset, SP_main is used and
\ ** SP_main is always used for exceptions/interrupts. To reduce
\ ** the overall RAM requirement, the initialisation code switches
\ ** the main task and all subsequent tasks) to use SP_process,
\ ** but leaves the CPU in privileged mode.


\ ***************************
\ *S Task area and Stack equates
\ ***************************
\ *P A task consists of three areas
\ *(
\ *B USER area, pointed to by UP.
\ *B Data stack, pointed to by PSP (usually R12).
\ *B Return stack, pointed to by RSP (always R13).
\ *)
\ *P The sizes of these areas are defined by the equates
\ ** *\fo{UP-SIZE}, *\fo{SP-SIZE}, and *\fo{RP-SIZE} in
\ ** the control file.

\ *P The return stack must be the lowest of RSP, PSP and UP
\ ** in order to permit fast interrupt nesting. In order for
\ ** the task initialisation code in *\i{MultiCortex.fth} to
\ ** work correctly, *\fo{INIT-U0} must be the highest.

rp-size sp-size + equ TASK-U0	\ -- offset
\ *G Initial offset of user area from the task base address.
rp-size sp-size + equ TASK-S0	\ -- offset
\ *G Initial offset of data stack from the task base address.
rp-size           equ TASK-R0	\ -- offset
\ *G Initial offset of return stack from the task base address.

task-size reserve equ INIT-T0	\ -- addr
\ *G Base of main task area.
init-t0 task-u0 + equ INIT-U0	\ -- addr
\ *G Base of main user area
init-t0 task-s0 + equ INIT-S0	\ -- addr
\ *G Top of main data stack
init-t0 task-r0 + equ INIT-R0	\ -- addr
\ *G Top of main return stack. The set code sets this as the
\ ** initial value of R13/SP_process.

\ *P All tasks run in privileged Thread mode. Defining additional
\ ** tasks reserves RAM for the tasks.

[defined] tib-len [if]
tib-len reserve equ INIT-TIB	\ -- addr
\ Standalone Forths require a terminal input buffer. *\fo{INIT-TIB}
\ ** is defined if the equate *\fo{TIB-LEN} is defined.
[then]


\ ***************************************
\ *S Exception/Interrupt stack allocation
\ ***************************************
\ *P The amount of RAM required for exception handlers is
\ ** affected by the number of nexsted interrupt handlers
\ ** and the number of SVC/SWI calls that can be nested.

\ IRQ stacks ; nestable up to #IRQs
\ We assume that high level interrupt handlers will be used

#IRQs #SVCs + task-size * equ IRQ_STACK_SIZE	\ -- len
\ *G Total RAM required for exception/interrupt handlers stacks.

0 reserve equ INT_STACK_TOP	\ -- addr
\ *G Top of exception stacks
IRQ_STACK_SIZE reserve equ INT_STACK_BASE
\ *G Bottom of exception stacks


\ ======
\ *> ###
\ ======

decimal

