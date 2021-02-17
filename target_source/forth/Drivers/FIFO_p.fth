\ fifo_p.fth - Polled Driver for FIFO-base comms.
\
\ For BASE, use the offset of the FIFO registers

$4000:3000 equ FIFOBASE

\ Offsets
0    equ FIFO_DATA
bit0 equ FIFO_STATUS_RXNE \ Not Empty
bit1 equ FIFO_STATUS_TXNF

4    equ FIFO_H2D_STATUS 
bit0 equ  FIFO_H2D_STATUS_DATA_NOTEMPTY

8    equ FIFO_D2H_STATUS 
bit0 equ FIFO_D2H_STATUS_DATA_NOTFULL

\ ********************
\ *S Serial primitives
\ ********************

: +FaultConsole	( -- )  ;
\ *G Because this is a polled driver, *\fo{+FaultConsole} for
\ ** fault handling is a *\fo{NOOP}. See *\i{Cortex/FaultCortex.fth}
\ ** for more details.

: (seremit)	\ char base --
\ *G Transmit a character on the given UART.
  
  begin
    dup FIFO_D2H_STATUS + @ FIFO_D2H_STATUS_DATA_NOTFULL and	\ Tx FIFO full test
  until
  FIFO_DATA + !
;

: (sertype)	\ caddr len base --
\ *G Transmit a string on the given UART.
  -rot bounds
  ?do  i c@ over (seremit)  loop
  drop
;

: (sercr)	\ base --
\ *G Transmit a CR/LF pair on the given UART.
  $0D over (seremit)  $0A swap (seremit)
;

: (serkey?)	\ base -- t/f
\ *G Return true if the given UART has a character avilable to read.
  FIFO_H2D_STATUS + @ FIFO_H2D_STATUS_DATA_NOTEMPTY and \ Rx FIFO empty test
;

: (serkey)	\ base -- char
\ *G Wait for a character to come available on the given UART and
\ ** return the character.
  begin
    dup (serkey?) 
  until
  FIFO_DATA + c@
;

: initUART	\ divisor22 base --
  2drop 
;

external


\ ********
\ *S UART0
\ ********

useUART0? [if]

: init-ser0	; 

: seremit0	\ char --
\ *G Transmit a character on UART0.
  FIFOBASE (seremit)  ;
: sertype0	\ c-addr len --
\ *G Transmit a string on UART0.
  FIFOBASE (sertype)  ;
: sercr0	\ --
\ *G Issue a CR/LF pair to UART0.
  FIFOBASE (sercr)  ;
: serkey?0	\ -- flag
\ *G Return true if UART0 has a character available.
  FIFOBASE (serkey?)  ;
: serkey0	\ -- char
\ *G Wait for a character on UART0.
  FIFOBASE (serkey)  ;
create Console0	\ -- addr ; OUT managed by upper driver
\ *G Generic I/O device for UART0.
  ' serkey0 ,		\ -- char ; receive char
  ' serkey?0 ,		\ -- flag ; check receive char
  ' seremit0 ,		\ -- char ; display char
  ' sertype0 ,		\ caddr len -- ; display string
  ' sercr0 ,		\ -- ; display new line
console-port 0 = [if]
  console0 constant console
\ *G *\fo{CONSOLE} is the device used by the Forth system for interaction.
\ ** It may be changed by one of the phrases of the form:
\ *C   <device>  dup opvec !  ipvec !
\ *C   <device>  SetConsole
[then]

[then]


\ ************************
\ *S System initialisation
\ ************************

: init-ser	\ --
\ *G Initialise all serial ports
  [ useUART0? ] [if]  init-ser0  [then]
;


\ ======
\ *> ###
\ ======

decimal
