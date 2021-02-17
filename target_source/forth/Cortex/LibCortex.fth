\ ARM Cortex CPU specific library code

((
Copyright (c) 2000..2003, 2010
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
20100204 MPE002 Cortex conversion.
20040211 MPE001 Added @ON and @OFF.
))

only forth definitions
decimal

\ =========
\ *! libcortex
\ *T ARM Cortex specific library code
\ =========
\ *P The code in *\i{Cortex/LibCortex.fth} is conditionally
\ ** compiled by the following code fragment to be found at the
\ ** end of many control files.
\ *E libraries	\ to resolve common forward references
\ **   include %CpuDir%/LibCortex
\ **   include %CommonDir%/library
\ ** end-libs
\ *P Each definition in a library file is surrounded by
\ ** a phrase of the form:
\ *C [required] <name> [if] : <name> ... ;  [then]
\ *P The phrase *\fo{[REQUIRED] <name>} returns true if *\fo{<name>}
\ ** has been forward referenced and is still unresolved.
\ ** The code between *\fo{LIBRARIES} and *\fo{END-LIBS} is repeatedly
\ ** processed until no further references are resolved.


\ ******************
\ *S I/O initialisation
\ ******************

[required] init-io [if]
: init-io	\ addr --
\ *G Copy the contents of the I/O set up table to an I/O device.
\ ** Each element of the table is of the form addr (cell) followed
\ ** by data (cell). The table is terminated by an address of 0.
\ ** A table of a single 0 address performs no action.
  begin
    dup @
   while
    dup 2@ !  2 cells +
  repeat
  drop
;
[then]


\ **************************
\ *S interrupt enable and disable
\ **************************

[required] di [if]
code di		\ --
\ *G Disable interrupts.
  cps     .id .i
  next,
end-code
[then]

[required] ei [if]
code ei		\ --
\ *G Enable interrupts.
  cps     .ie .i
  next,
end-code
[then]

[required] dfi [if]
code dfi	\ --
\ *G Disable fault exceptions.
  cps     .id .f
  next,
end-code
[then]

[required] efi [if]
code efi		\ --
\ *G Enable fault exceptions.
  cps     .ie .f
  next,
end-code
[then]

[required] [i [if]
code [I		\ R: -- x1 x2
\ *G Preserve interrupt/exception status on the return stack,
\ ** and disable interrupts/exceptions except reset, NMI and
\ ** HardFault. The state is restored by *\fo{I]}.
  mrs     r0, PRIMASK			\ get status
  mrs     r1, FAULTMASK
  cps     .id .i .f
  push .n { r0, r1 }
  next,
end-code
[then]

[required] i] [if]
code I]		\ R: x1 x2 --
\ *G Restore interrupt status saved by *\fo{[I} from the return
\ ** stack.
  pop .n  { r0, r1 }
  msr     FAULTMASK r1
  msr     PRIMASK r0
  next,
end-code
[then]


\ ****************
\ *S Miscellaneous
\ ****************

[required] @off [if]
: @OFF		\ addr -- x
\ *G Read cell at addr, and set it to 0.
  dup @ swap off
;
[then]

[required] @on [if]
: @on           \ addr -- val
\ *G Fetch contents of cell at addr and set it to -1.
  dup @ swap on
;
[then]


\ ======
\ *> ###
\ ======

