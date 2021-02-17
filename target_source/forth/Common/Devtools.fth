\ DEVTOOLS.FTH - Tools for development use

((
Copyright (c) 2002, 2003, 2004, 2006, 2009, 2017
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
20170309 BJC006 Corrected conditional compilation closures.
20090213 SFP005 Added more conditional compilation.
20061025 SFP004 Simplified "simple" versions of .WORD and .LWORD.
20040107 SFP003 Added option of .WORD and friends without division.
20030320 SFP002 Added FlushKeys where appropriate
20020915 SFP001 Removed INTERNAL/EXTERNAL.
		Made DUMP safe for len=0.
		Added DOCGEN comments
))

decimal

\ ===========
\ *! devtools
\ *T Development tools
\ ===========
\ *P The file *\i{Common\Devtools.fth} supplies words that are
\ ** mostly used during development and debugging.

[undefined] HexTools? [if]
1 equ HexTools?	\ -- flag
\ *G If non-zero, *\fo{.DWORD} and friends will be compiled.
[then]

[undefined] DUMP? [if]
1 equ DUMP?	\ -- flag
\ *G If non-zero, *\fo{DUMP} and friends will be compiled.
[then]


\ ****************************
\ *S Hexadecimal display tools
\ ****************************

HexTools? [if]

\ SFP003...
1 equ simple?	\ -- n
\ *G Set this flag non-zero to generate *\fo{.xWORD} to avoid
\ ** divisions, which are slow on some CPUs.

simple? [if]

internal
: .nibble	\ n --
\ *G Convert a nibble to a hex ASCII digit and display it.
  $0F and  dup 9 >
  if  7 +  then
  $30 + emit
;
external

: .BYTE		\ b --
  dup #4 rshift .nibble .nibble
;

: .WORD		\ w --
  dup 8 rshift .byte .byte
;
32bit? [if]
: .lword	\ x --
  dup #16 rshift .word ." :" .word
;

: .DWORD	\ x --
  .lword
;
[then]		\ 32bit?

[else]		\ simple?

: .BYTE		\ b --
\ *G Display b as two hex digits.
  base @ hex  swap
  0 <# # # #> type
  base !
;

: .WORD		\ w --
\ *G Display w as four hex digits.
  base @ hex  swap
  0 <# # # # # #> type
  base !
;

32bit? [if]
: .LWORD	\ x --
\ *G Display x as eight hex digits.
\ ** The separator ":" makes the output easier to read.
\ ** Future releases of MPE Forths will treat the ":"
\ ** character as having no effect on number input parsing.
\ ** This character is chosen because it does not conflict
\ ** with the current use of the "." and "," characters
\ ** for numbers.
\ ** This word is only compiled for 32 bit targets.
  base @ hex  swap
  0 <# # # # # [char] : hold # # # # #> type
  base !
;

: .DWORD	\ x --
\ *G A synonym for .LWORD.
  .lword
;
[then]		\ 32 bit?

[then]		\ simple?
\ ...SFP003

[then]		\ HexTools? ; BJC006


\ *******************
\ *S DUMP and friends
\ *******************

DUMP? [if]

: .ASCII	\ char --
\ *G The top bit of char is zeroed. If char is in the range
\ ** 32..126 it is displayed, otherwise a "." is displayed.
  $7F and  dup $20 $7E within? 0=
  if  drop $2E  then
  emit
;

: DUMP		\ addr len --
\ *G Display (dump) len bytes of memory starting at addr.
[defined] FlushKeys [if] FlushKeys [then]	\ SFP002
  bounds ?do				\ get limits
    cr  i .lword  space   i 8 + i       \ print address
    do  space i c@ .byte  loop          \ first eight bytes
    space  i $010 + i 8 +               \ gap
    do  space i c@ .byte  loop          \ second eight bytes
    2 spaces  i $010 + i                \ gap
    do  i c@ .ascii  loop               \ ascii o/p
    halt? ?leave			\ quit or halt
  $010 +loop                            \ until done
  cr
;

32bit? [if]
: LDUMP		\ addr len -- ; dump 32 bit long words
\ *G Display (dump) len bytes of memory starting at addr
\ ** as 32 bit words.
[defined] FlushKeys [if] FlushKeys [then]	\ SFP002
  bounds ?do				\ get limits
    cr  i .lword  space   i 8 + i       \ print address
    do  space i @ .lword  4 +loop	\ first eight bytes
    space  i $010 + i 8 +               \ gap
    do  space i @ .lword  4 +loop	\ second eight bytes
    2 spaces  i $010 + i                \ gap
    do  i c@ .ascii  loop               \ ascii o/p
    halt? ?leave			\ quit or halt
  $010 +loop                            \ until done
  cr  					\ restore base
;

: PDUMP		\ addr len -- ; dump 32 bit long words
\ *G Display (dump) len bytes of memory starting at addr
\ ** as 32 bit words with *\b{no} ASCII dump. This word
\ ** may be necessary when accessing peripherals that must
\ ** only be addressed on 32 bit boundaries.
[defined] FlushKeys [if] FlushKeys [then]	\ SFP002
  bounds ?do				\ get limits
    cr  i .lword  space   i 8 + i       \ print address
    do  space i @ .lword  4 +loop	\ first eight bytes
    space  i $010 + i 8 +               \ gap
    do  space i @ .lword  4 +loop	\ second eight bytes
    halt? ?leave			\ quit or halt
  $010 +loop                            \ until done
  cr  					\ restore base
;

: WDUMP		\ addr len -- ; dump 16 bit half words
\ *G Display (dump) len bytes of memory starting at addr
\ ** as 16 bit half-words.
[defined] FlushKeys [if] FlushKeys [then]	\ SFP002
  bounds ?do				\ get limits
    cr  i .lword  space   i 8 + i       \ print address
    do  space i w@ .word  2 +loop	\ first eight bytes
    space  i $010 + i 8 +               \ gap
    do  space i w@ .word  2 +loop	\ second eight bytes
    2 spaces  i $010 + i                \ gap
    do  i c@ .ascii  loop               \ ascii o/p
    halt? ?leave			\ quit or halt
  $010 +loop                            \ until done
  cr  					\ restore base
;
[then]		\ 32bit? ; BJC006

[then]		\ DUMP?

: .S		\ --
\ *G Display the contents of the data stack without affecting it.
  cr  depth ?dup 0= if
    ." empty stack"  cr  exit
  endif
  ."      top"
  dup 0< #-4 ?throw  0 do
    i pick
    cr  dup s>d #15 d.r space	        \ show word in current base
    base @ #16 <>			\ if base is not HEX
    if  .lword  else  drop  then	\ also show it in HEX
    key? ?leave
  loop
  cr
;

: ?		\ a-addr --
\ *G Display contents of a memory location as a cell.
  @ .
;

decimal


\ ======
\ *> ###
\ ======

