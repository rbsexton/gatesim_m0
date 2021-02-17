\ A library of useful words for Forth6 cross-compiled targets

((
Copyright (c) 1988-2004
MicroProcessor Engineering
133 Hill Lane
Southampton SO15 5AF
England

tel: +44 (0)2380 631441
fax: +44 (0)2380 339691
net: mpe@mpeforth.com
     tech-support@mpeforth.com
web: www.mpeforth.com


To do
=====
DocGen everything

	
Change history
==============
20040216 MPE006 Added words from LIB.FTH and deleted LIB.FTH.
		Added SETCONSOLE.
20011113 SFP005 Added INIT-IO
20000112 SFP004 Added S.DEC and SN.DEC
19981023 MSD003 Removed the LIBRARIES and END-LIBS invocations as
		they should have been placed outside this source.
		(Not flagged as they were just deletions.)
19981023 MSD002 Added .HEX as a synonym for U.HEX.
19981023 MSD001 Remove the use of S>D in the *hex* display wordset.
		In this case, we do *not* want the top bit to be
		rippled across the most significant cell since
		what we are displaying is *hex*.
))


\ ********************
\ *S Memory mapped I/O
\ ********************

[required] init-io [if]
: init-io	\ addr --
\ *G Copy the contents of an I/O set up table to an I/O device.
\ ** Each element of the table is of the form addr (cell) followed
\ ** by data (cell). The table is terminated by an address of 0.
\ ** A table of a single 0 address performs no action. Note
\ ** that this word is for memory-mapped I/O only.
  begin
    dup @
   while
    dup 2@ !  2 cells +
  repeat
  drop
;
[then]


\ **************
\ *S Generic I/O
\ **************

[required] ConsoleIO [if]
: ConsoleIO	\ --
\ *G Use the default console defined by CONSOLE as the
\ ** current terminal device.
  console SetConsole
;
[then]

[required] SetConsole [if]
: SetConsole	\ device --
\ *G Sets KEY and EMIT and frieds to use the given device
\ ** for terminal I/O.
  dup ipvec !  opvec !
;
[then]


\ **********
\ *S Strings
\ **********

[required] bounds [if]
: BOUNDS	\ addr len -- addr+len addr
\ *G Modify the address and length parameters to provide an end-address
\ ** and start-address pair suitable for a DO ... LOOP construct.
  over + swap
;
[then]

[required] Erase [if]
: Erase		\ addr n -- 
\ *G Fill U bytes of memory from A-ADDR with 0.
  0 Fill  ;
[then]

[required] 2@ [if]
: 2@		\ addr -- x1 x2
\ *G Fetch and return the two CELLS from memory ADDR and ADDR+sizeof(CELL).
\ ** The cell at the lower address is on the top of the stack.
  dup cell+ @  swap @
;
[then]

[required] 2! [if]
: 2!            \ x1 x2 addr -- ; store into DATA memory
\ *G Store the two CELLS x1 and x2 at memory ADDR.
\ ** X2 is stored at ADDR and X1 is stored at ADDR+CELL.
  swap over !  cell+ !
;
[then]


\ ******************
\ *S Numeric display
\ ******************

[required] hold
[required] sign or
[required] #    or
[required] #s   or
[required] <#   or
[required] #>   or
[if]

  $80 equ holdsize
  $80 equ padsize

  holdsize buffer: holdarea
  padsize  buffer: pad

  variable hld

  : HOLD	\ char --
    -1 hld +!  hld @ c!
  ;

  : SIGN	\ n --
    0< if  [char] - hold  then
  ;

  : #		\ ud1 -- ud2
    base @ mu/mod rot 9 over <
    if  7 +  then
    [char] 0 + hold
  ;

  : #S		\ ud1 -- ud2
    begin  #  2dup or 0=  until
  ;

  : <#		\ --
    pad hld !
  ;

  : #>		\ xd -- c-addr u
    2drop hld @ pad over -
  ;

[then]

\ A set of words to display a cell's value, saving and restoring BASE
\ during the period that it is doing so.

[required] .hex                                                                 \ MSD002
[required] u.hex   or                                                           \ MSD002
[required] uN.hex  or
[required] u2.hex  or
[required] u4.hex  or
[required] u6.hex  or
[required] u8.hex  or
[required] $u.hex  or
[required] $uN.hex or
[required] $u2.hex or
[required] $u4.hex or
[required] $u6.hex or
[required] $u8.hex or
[if]

  : u.hex	\ value -- ; Variable width.
    base @ swap hex
    0 <# #S #> type                                           			\ MSD001
    base !
  ;

  : uN.hex	\ value width -- ; User-specified width.
    base @ -rot hex
    swap 0                                                                      \ MSD001
    <# rot 0 ?do # loop #> type
    base !
  ;

  : .hex      u.hex ;     ( value -- )                                          \ MSD002
  : u2.hex 2 uN.hex ;     ( value -- )
  : u4.hex 4 uN.hex ;     ( value -- )
  : u6.hex 6 uN.hex ;     ( value -- )
  : u8.hex 8 uN.hex ;     ( value -- )

  : $u.hex  ." $" u.hex ;      ( value -- )
  : $uN.hex ." $" uN.hex ;     ( value width -- )
  : $u2.hex ." $" u2.hex ;     ( value -- )
  : $u4.hex ." $" u4.hex ;     ( value -- )
  : $u6.hex ." $" u6.hex ;     ( value -- )
  : $u8.hex ." $" u8.hex ;     ( value -- )

[then]

[required] u.dec
[required] uN.dec or
[required] s.dec  or
[required] sN.dec or
[required] u2.dec or
[required] u4.dec or
[required] u6.dec or
[required] u8.dec or
[if]

  : u.dec	\ value -- ; Variable width.
    base @ swap decimal
    0 <# #S #> type
    base !
  ;

  : uN.dec	\ value width -- ; User-specified width.
    base @ -rot decimal
    swap 0
    <# rot 0 ?do # loop #> type
    base !
  ;

  : u2.dec 2 uN.dec ;     ( value -- )
  : u4.dec 4 uN.dec ;     ( value -- )
  : u6.dec 6 uN.dec ;     ( value -- )
  : u8.dec 8 uN.dec ;     ( value -- )

  : s.dec	\ value -- ; Variable width.
    base @ swap decimal
    s>d tuck dabs <# #S rot sign #> type
    base !
  ;

  : sN.dec	\ value width -- ; User-specified width.
    >r
    base @ swap  decimal
    s>d tuck dabs
    <#  r> 0 ?do # loop  rot sign  #> type
    base !
  ;

[then]

[required] u.both [if]
  : u.both	\ value --
    dup u.dec ." =" $u.hex ;
[then]

[required] .ascii [if]
: .ascii        \ char -- ;
  dup bl <
  over [char] ~ > or
  if  drop [char] .  endif
  emit
;
[then]


\ ******************
\ *S Debugging tools
\ ******************

[required] ?            [if] : ? @ . ;                         [then]
[required] hex          [if] : hex #16 base ! ;                [then]
[required] decimal      [if] : decimal #10 base ! ;            [then]
[required] octal        [if] : octal #8 base ! ;               [then]
[required] binary       [if] : binary #2 base ! ;              [then]
[required] bl           [if] $20 constant bl                   [then]
[required] space        [if] : space bl emit ;                 [then]
[required] s>d          [if] : dup 0< if -1 else 0 then ;      [then]
[required] depth        [if] : depth s0 @ sp@ - cell- cell/ ;  [then]
[required] cell-        [if] : cell- cell - ;                  [then]
[required] cell+        [if] : cell+ cell + ;                  [then]
[required] cells        [if] : cells cell * ;                  [then]
[required] led-init     [if] : led-init ;                      [then]
[required] led-on       [if] : led-on ;                        [then]
[required] led-off      [if] : led-off ;                       [then]
[required] .            [if] : . s.dec space ;                 [then]
[required] abort        [if] : abort begin cr .cpu cr again ;  [then]
[required] true         [if] -1 constant true                  [then]
[required] false        [if] 0 constant false                  [then]

[required] .shex [if]
  : .shex       (  --  )
    [char] [ emit space
    depth
    dup 0>
    if
      0 do
        [char] $ emit
        depth i - 1- pick u.hex
        space
      loop
    else
      drop
    then
    [char] ] emit
  ;
[then]

[required] dump [if]
  : dump        (  addr u  --  )
    bounds
    do
      cr   i u8.hex space
      i $04 + i $00 + do i c@ space u2.hex loop
      space
      i $08 + i $04 + do i c@ space u2.hex loop
      space
      i $0c + i $08 + do i c@ space u2.hex loop
      space
      i $10 + i $0c + do i c@ space u2.hex loop
      space
      i $10 + $00 i + do  i c@ .ascii  loop
    $10 +loop
    cr
  ;
[then]

