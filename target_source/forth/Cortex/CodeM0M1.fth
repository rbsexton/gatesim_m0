\ CodeM0M1.fth - ARM Cortex-M0/M1 code file

((
Copyright (c) 2009, 2010, 2011, 2014
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
Recode FILL to check for word start address, and copy words if possible.
This change may have a good speed/size trade-off.

Change history
==============
20140107 SFP006 Corrected FM/MOD.
20110601 SFP005 Cortex M0/M1 conversion.
20100430 SFP004 Corrected !LCALL and (;CODE).
20100427 SFP003 Corrected ROLL.
20100108 MPE002 First release
20091209 SFP001 Started
))

\ ==========
\ *! codem0m1lite
\ *T Cortex code definitions
\ ==========
\ *P The file *\i{Cortex/CodeM0/M1.fth} contains primitives for
\ ** the standalone Forth kernel.

\ ********
\ *S Notes
\ ********
\ *P Some words and code routines are marked in the documentation
\ ** as *\fo{INTERNAL}. These are factors used by other words and do
\ ** not have dictionary entries in the standalone Forth. They
\ ** are only accessible to users of the VFX Forth ARM Cross
\ ** Compiler. This also applies to definitions of the form:
\ *C n EQU <name>
\ *C PROC <name>
\ *C L: <name>

\ *****************
\ *S Register usage
\ *****************
\ *P For Cortex-M0/M1 the following register usage is the default:
\ *E   r15         pc      program counter
\ **   r14         link    link register; bit0=1=Thumb, usually set
\ **   r13         rsp     return stack pointer
\ **   r12         --
\ **   r11         up      user area pointer
\ **   r10         --
\ **   r9          lp      locals pointer
\ **   r8          --
\ **   r7          tos     cached top of stack
\ **   r6          psp     data stack pointer
\ **   r0-r5       scratch
\ *P The VFX optimiser reserves R0 and R1 for internal operations.
\ ** *\fo{CODE} definitions must use R7 as TOS with NOS pointed
\ ** to by R6 as a full descending stack in ARM terminology.
\ ** R0..R5, R12 are free for use by *\fo{CODE} definitions and
\ ** need not be preserved or restored. You should assume that
\ ** any register can be affected by other words.

only forth definitions
decimal


\ ***********************************
\ *S Logical and relational operators
\ ***********************************

: AND		\ x1 x2 -- x3
\ *G Perform a logical AND between the top two stack items and retain
\ ** the result in top of stack.
  and  ;

: OR		\ x1 x2 -- x3
\ *G Perform a logical OR between the top two stack items and retain
\ ** the result in top of stack.
  or  ;

: XOR		\ x1 x2 -- x3
\ *G Perform a logical XOR between the top two stack items and retain
\ ** the result in top of stack.
  xor  ;

: INVERT 	\ x -- x'
\ *G Perform a bitwise inversion.
  invert  ;

: 0=	 	\ x -- flag
\ *G Compare the top stack item with 0 and return TRUE if equals.
  0=  ;

: 0<> 		\ x -- flag
\ *G Compare the top stack item with 0 and return TRUE if not-equal.
  0<>  ;

: 0<	 	\ x -- flag
\ *G Return TRUE if the top of stack is less-than-zero.
  0<  ;

: 0> 		\ x -- flag
\ *G Return TRUE if the top of stack is greater-than-zero.
  0>  ;

: = 		\ x1 x2 -- flag
\ *G Return TRUE if the two topmost stack items are equal.
  =  ;

: <> 		\ x1 x2 -- flag
\ *G Return TRUE if the two topmost stack items are different.
  <>  ;

: < 		\ n1 n2 -- flag
\ *G Return TRUE if n1 is less than n2.
  <  ;

: > 		\ n1 n2 -- flag
\ *G Return TRUE if n1 is greater than n2.
  >  ;

: <= 		\ n1 n2 -- flag
\ *G Return TRUE if n1 is less than or equal to n2.
  <=  ;

: >= 		\ x1 x2 -- flag
\ *G Return TRUE if n1 is greater than or equal to n2.
  >=  ;

: U> 		\ u2 u2 -- flag
\ *G An UNSIGNED version of >.
  u>  ;

: U< 		\ u1 u2 -- flag
\ *G An UNSIGNED version of <.
  u<  ;

UseDoubles [if] 
: D0<	 	\ d -- flag
\ *G Returns true if signed double d is less than zero.
  nip 0<  ;

: D0=	\ xd -- flag
\ *G Returns true if xd is 0.
  or  0=  ;


CODE D= 	\ xd1 xd2 -- flag
\ *G Return TRUE if the two double numbers are equal.
  ldmia psp ! { r0-r2 }                 \ r0=d2 low, r1=d1 high, r2=d1 low
  mov .s r3, tos			\ r3=d2h
  mov .s tos, # 0			\ assume false
  sub .s r0, r0, r2                     \ d2l - d1l
  sbc .s r3, r3, r1			\ d2h - d1h
  orr .s r0, r0, tos
  eq, if,
    mvn .s tos, tos			\ true
  endif,
  next,
END-CODE

CODE D< 	\ d1 d2 -- flag
\ *G Return TRUE if the double number d1 is (signed) less than the
\ ** double number d2.
  ldmia psp ! { r0-r2 }                 \ r0=d2 low, r1=d1 high, r2=d1 low
  mov .s r3, tos			\ r3=d2h
  mov .s tos, # 0			\ assume false
  sub .s r2, r2, r0                     \ d1l - d2l
  sbc .s r1, r1, r3			\ d1h - d2h
  lt, if,
    mvn .s  tos, tos			\ true
  endif,
  next,
END-CODE

CODE DU< 	\ ud1 ud2 -- flag
\ *G Returns true if ud1 (unsigned double) is less than ud2.
  ldmia psp ! { r0-r2 }                 \ r0=d2 low, r1=d1 high, r2=d1 low
  mov .s r3, tos			\ r3=d2h
  mov .s tos, # 0			\ assume false
  sub .s r2, r2, r0                     \ d1l - d2l
  sbc .s r1, r1, r3			\ d1h - d2h
  cc, if,
    mvn .s tos, tos
  endif,
  next,
END-CODE

: D> 		\ d1 d2 -- flag
\ *G Return TRUE if the double number d1 is (signed) greater than the
\ ** double number d2.
  2swap d<  ;

: DU>	 	\ ud1 ud2 -- flag
\ *G Returns true if ud1 (unsigned double) is greater than ud2.
  2swap du<  ;

: DMAX 	\ d1 d2 -- d3 ; d3=max of d1/d2
\ *G Return the maximum double number from the two supplied.
  2over 2over d<
  if  2swap  then
  2drop
;

: DMIN 	\ d1 d2 -- d3 ; d3=min of d1/d2
\ *G Return the minimum double number from the two supplied.
  2over 2over d>
  if  2swap  then
  2drop
;
[then] \ UseDoubles


CODE MIN	\ n1 n2 -- n1|n2
\ *G Given two data stack items preserve only the smaller.
\  ldr     r0, [ psp ], # 4
  ldmia   psp, ! { r0 }
  cmp     tos, r0
  gt, if,
    mov     tos, r0
  endif,
  next,
END-CODE

CODE MAX	\ n1 n2 -- n1|n2
\ *G Given two data stack items preserve only the larger.
\  ldr     r0, [ psp ], # 4
  ldmia   psp, ! { r0 }
  cmp     tos, r0
  lt, if,
    mov  tos, r0
  endif,
  next,
END-CODE

CODE WITHIN? 	\ n1 n2 n3 -- flag
\ *G Return TRUE if N1 is within the range N2..N3.
\ ** This word uses signed arithmetic.
  ldmia   psp ! { r0, r1 }
  mov .s  r2, # 0
  mov .s  r3, # 0
  cmp     r1, r0
  ge, if,
    mov .s  r2, # 1
  endif,
  cmp     r1, tos
  le, if,
    mov .s  r3, # 1
  endif,
  mov .s  tos, # 0		\ flag=0
  tst     r2, r3
  ne, if,
    mvn .s  tos, tos		\ flag=-1
  endif,
  next,
END-CODE

CODE WITHIN 	\ n1|u1 n2|u2 n3|u3 -- flag
\ *G The ANS version of WITHIN?.
\ ** This word uses unsigned arithmetic, so that signed compares are
\ ** treated as existing on a number circle.
  mov     r2, tos			\ save tos
  ldmia   psp ! { r0, r1 }              \ r2 = n3, r0 = n2, r1 = n1
  mov .s  tos, # 0			\ assume false
  sub .s  r2, r2, r0
  sub .s  r1, r1, r0
  cmp     r1, r2
  lo, if,
    mvn .s  tos, tos
  endif,
  next,
END-CODE

CODE LSHIFT 	\ x1 u -- x2
\ *G Logically shift X1 by U bits left.
  mov     r0, tos
  ldmia   psp ! { tos }
  lsl .s  tos, r0
  next,
END-CODE

CODE RSHIFT 	\ x1 u -- x2
\ *G Logically shift X1 by U bits right.
  mov     r0, tos
  ldmia   psp ! { tos }
  lsr .s  tos, r0
  next,
END-CODE


\ ***************
\ *S Control flow
\ ***************

CODE EXECUTE 	\ xt --
\ *G Execute the code described by the XT. This is a Forth equivalent
\ ** to an assembler JSR/CALL instruction.
  mov .s  r0, # 1
  orr .s  r0, tos		        \ move CFA, setting Thumb bit
  ldmia   psp ! { tos }
  bx      r0				\ execute CFA - link contains ret addr of execute
END-CODE

internal

CODE BRANCH	\ --
\ *G The run time action of unconditional branches compiled on the target.
\ ** The branch target address is in-line and must have the T bit set.
\ ** INTERNAL.
l: takebranch
  mov .s  r1, # 1
  mov     r0, link
  bic .s  r0, r1
  ldr     r2, [ r0 ]			\ get address and branch
  bx      r2
END-CODE

CODE ?BRANCH	\ n --
\ *G The run time action of conditional branches compiled on the target.
\ ** The branch target address is in-line and must have the T bit set.
\ ** INTERNAL.
  mov .s  r1, tos
  ldmia   psp ! { tos }
  b .eq   takebranch
l: skipbranch
  mov     r0, link
  add .s  r0, # 4
  bx      r0
END-CODE

CODE (OF) 	\ n1 n2 -- n1|--
\ *G The run time action of OF compiled on the target.
\ ** The branch target address is in-line and must have the T bit set.
\ ** INTERNAL.
  ldmia   psp ! { r0 }			\ get n1
  cmp     r0, tos			\ compare n1 and n2
  b .ne   takebranch
  ldmia   psp ! { tos }			\ equal so get new tos
  b       skipbranch
END-CODE

CODE (LOOP) 	\ --
\ *G The run time action of *\fo{LOOP} compiled on the target.
\ ** The branch target address is in-line and must have the T bit set.
\ ** INTERNAL.
  ldr     r1, [ rsp ]			\ fetch index
  add .s  r1, r1, # 1			\ increment index
  str     r1, [ rsp ]			\ store new index]
  b .vc   takebranch
  add     rsp, rsp, # $0C		\ drop 3 items from return stack
  b       skipbranch
END-CODE

CODE (+LOOP) 	\ n --
\ *G The run time action of *\fo{+LOOP} compiled on the target.
\ ** The branch target address is in-line and must have the T bit set.
\ ** INTERNAL.
  ldr    r1, [ rsp ]			\ fetch index
  add .s r1, r1, tos                    \ increment index by n
  str    r1, [ rsp ]			\ store new index
  ldmia   psp ! { tos }			\ update tos
  b .vc   takebranch
  add     rsp, rsp, # $0C		\ drop 3 items from return stack
  b       skipbranch
END-CODE

CODE (DO)	\ limit index --
\ *G The run time action of *\fo{DO} compiled on the target.
\ ** The branch target address is in-line and must have the T bit set.
\ ** INTERNAL.
  ldmia   psp ! { r1 }			\ get limit
L: PDO
  mov .s  r4, # 1
  lsl .s  r4, r4, # #31			\ r4 := $8000:0000
  mov     r3, link
  sub .s  r3, r3, # 1			\ clear T bit
  ldr     r2, [ r3 ]			\ get LEAVE address, compiler sets T bit
  add .s  r1, r1, r4			\ limit+$8000:0000
  sub .s  r0, tos, r1			\ index-limit-$8000:0000

  push    { r0, r1, r2 }		\ push LEAVE then limit, then index on ret. stack
  ldmia   psp ! { tos }			\ update tos
  b       skipbranch
END-CODE

CODE (?DO) 	\ limit index --
\ *G The run time action of *\fo{?DO} compiled on the target.
\ ** The branch target address is in-line and must have the T bit set.
\ ** INTERNAL.
  ldmia   psp ! { r1 }			\ get limit
  cmp     r1, tos			\ check not equal
  b .ne   pdo				\ take DO ?
  ldmia   psp ! { tos }			\ update tos
  b       takebranch
END-CODE

external

CODE LEAVE 	\ --
\ *G Remove the current *\fo{DO..LOOP} parameters and jump to the
\ ** end of the *\fo{DO..LOOP} structure.
  add     rsp, rsp, # 8			\ remove limit, index
  pop     { pc }			\ jump to exit address, Thumb bit set by DO/?DO
END-CODE

CODE ?LEAVE 	\ flag --
\ *G If flag is non-zero, remove the current *\fo{DO..LOOP} parameters
\ ** and jump to the end of the *\fo{DO..LOOP} structure.
  mov .s  tos, tos                      \ set flags
  ldmia   psp ! { tos }			\ update tos
  eq, if,
    bx      r14				\ flag false so continue
  endif,
  add rsp, rsp, # 8                     \ flag true so remove limit, index  - if old TOS<>0
  pop     { pc }			\ flag true so jump to exit address
END-CODE

CODE I		\ -- n
\ *G Return the current index of the inner-most DO..LOOP.
  sub .s  psp, psp, # 4
  str     tos, [ psp ]
  ldr     tos, [ rsp, # 0 ]
  ldr     r0, [ rsp, # 4 ]
  add .s  tos, tos, r0
  next,
END-CODE

CODE J 		\ -- n
\ *G Return the current index of the second DO..LOOP.
  sub .s  psp, psp, # 4
  str     tos, [ psp ]
  ldr     tos, [ rsp, # $0C ]		\ index
  ldr     r0, [ rsp, # $010 ]
  add .s  tos, tos, r0
  next,
END-CODE

CODE UNLOOP 	\ -- ; R: loop-sys --
\ *G Remove the DO..LOOP control parameters from the return stack.
  add     rsp, rsp, # $0C		\ remove loop parameters from return stack
  next,                                 \ jump to return address
END-CODE


\ *******************
\ *S Basic arithmetic
\ *******************

: S>D 	\ n -- d
\ *G Convert a single number to a double one.
  s>d  ;

: D>S	 	\ d -- n
\ *G Convert a double number to a single.
  drop  ;

: NOOP  ;	\ --
\ *G A NOOP, null instruction.

CODE M+     \ d1|ud1 n -- d2|ud2
\ *G Add double d1 to sign extended single n to form double d2.
   ldmia  psp ! { r0 r1 }		\ r0 = d1 high, r1 = d1 low
   asr .s r2, tos, # 31			\ d2h = sex(n)
   add .s r1, r1, tos                   \ d2l = d1l + n
   adc .s r0, r0, r2			\ d2h = d1h + d2h + c
   mov    tos, r0
   sub .s psp, psp, # 4
   str    r1, [ psp ]
   next,
END-CODE

: 1+	 	\ n1|u1 -- n2|u2
\ *G Add one to top-of stack.
  1 +  ;

: 2+		\ n1|u1 -- n2|u2
\ *G Add two to top-of stack.
  2 +  ;

: 4+	 	\ n1|u1 -- n2|u2
\ *G Add four to top-of stack.
  4 +  ;

: 1-	 	\ n1|u1 -- n2|u2
\ *G Subtract one from top-of stack.
  1 -  ;

: 2-	 	\ n1|u1 -- n2|u2
\ *G Subtract two from top-of stack.
  2 -  ;

: 4-	 	\ n1|u1 -- n2|u2
\ *G Subtract four from top-of stack.
  4 -  ;

: 2*		\ x1 -- x2
\ *G Multiply top of stack by 2.
  1 lshift  ;

: 4*		\ x1 -- x2
\ *G Multiply top of stack by 4.
  2 lshift  ;

: 2/		\ x1 -- x2
\ *G Signed divide top of stack by 2.
  1 arshift  ;

: U2/		\ x1 -- x2
\ *G Unsigned divide top of stack by 2.
  1 rshift  ;

: 4/		\ x1 -- x2
\ *G Signed divide top of stack by 4.
  2 arshift  ;

: U4/		\ x1 -- x2
\ *G Unsigned divide top of stack by 4.
  2 rshift  ;

CODE +  	\ n1|u1 n2|u2 -- n3|u3
\ *G Add two single precision integer numbers.
  ldr     r0, [ psp ]
  add .s  psp, psp, # 4
  add .s  tos, tos, r0
  next,
END-CODE

CODE -  	\ n1|u1 n2|u2 -- n3|u3
\ *G Subtract two integers. N3|u3=n1|u1-n2|u2.
  ldr     r0, [ psp ]
  add .s  psp, psp, # 4
  sub .s  tos, r0, tos
  next,
END-CODE

CODE NEGATE 	\ n1 -- n2
\ *G Negate an integer.
  rsb .s  tos, tos, # 0
  next,
END-CODE

CODE D+ 	\ d1 d2 -- d3
\ *G Add two double precision integers.
  ldmia   psp ! { r0-r2 }		\ r0 = d2 low, r1 = d1 high, r2 = d1 low
  add .s  r0, r0, r2			\ d3l = d2l + d1l
  adc .s  tos, tos, r1			\ d3h = d2h + d1h
  sub .s  psp, psp, # 4
  str     r0, [ psp ]			\ push d3l
  next,
END-CODE

CODE D- 	\ d1 d2 -- d3
\ *G Subtract two double precision integers. D3=D1-D2.
  ldmia   psp ! { r0-r2 }		\ r0 = d2 low, r1 = d1 high, r2 = d1 low
  sub .s  r0, r2, r0			\ d3l = d2l - d1l
  sbc .s  r1, r1, tos			\ d3h = d2h - d1h
  mov     tos, r1
  sub .s  psp, psp, # 4
  str     r0, [ psp ]			\ push d3l
  next,
END-CODE

CODE DNEGATE 	\ d1 -- -d1
\ *G Negate a double number.
L: DNEG1
  ldr     r0, [ psp ]			\ r0 = d1l, tos = d1h
  mov .s  r1, # 0
  rsb .s  r0, r0, # 0			\ negate low
  sbc .s  r1, r1, tos			\ negate high carrying through
  mov     tos, r1
  str     r0, [ psp ]
  next,
END-CODE

CODE ?NEGATE 	\ n1 flag -- n1|n2
\ *G If flag is negative, then negate n1.
  mov .s  tos, tos			\ set processor flags
  ldmia   psp ! { tos }
  mi, if,
    rsb .s  tos, tos, # 0		\ negate n1 if flag=true
  endif,
  next,
END-CODE

CODE ?DNEGATE 	\ d1 flag -- d1|d2
\ *G If flag is negative, then negate d1.
  mov .s  tos, tos			\ set processor flags
  ldmia   psp ! { tos }			\ discard flag
  b .mi   DNEG1
  next,
END-CODE

CODE ABS	\ n -- u
\ *G If n is negative, return its positive equivalent (absolute value).
  cmp     tos, # 0
  mi, if,
    rsb .s  tos, tos, # 0
  endif,
  next,
END-CODE

CODE DABS 	\ d -- ud
\ *G If d is negative, return its positive equivalent (absolute value).
  mov .s  tos, tos			\ set processor flags
  b .mi   DNEG1				\ negate d if flag=true
  bx      r14				\ restore link and exit
END-CODE

UseDoubles [if] 
CODE D2* 	\ xd1 -- xd2
\ *G Multiply the given double number by two.
  ldr     r0, [ psp ]			\ low portion
  add .s  r0, r0, r0
  adc .s  tos, tos, tos
  str     r0, [ psp ]
  next,
END-CODE

CODE D2/ 	\ xd1 -- xd2
\ *G Divide the given double number by two.
  ldr     r0, [ psp ]			\ low portion
  asr .s  r0, r0, # 1			\ low portion shift right
  asr .s  tos, tos, # 1			\ high portion shift right
  sbc .s  r1, r1, r1			\ r1 -> 0/-1
  lsl .s  r1, r1, # #31			\ r1 -> 8/$8000:0000
  orr .s  r0, r0, r1			\ apply to low portion
  str     r0, [ psp ]
  next,
END-CODE
[then]

\ *****************
\ *S Multiplication
\ *****************

: *		\ n1 n2 -- n3
\ *G Standard signed multiply. N3 = n1 * n2.
  *  ;

\ *P For Cortex-M0/M1 the following register usage is the default:
\ *E   r15         pc      program counter
\ **   r14         link    link register; bit0=1=Thumb, usually set
\ **   r13         rsp     return stack pointer
\ **   r12         --
\ **   r11         up      user area pointer
\ **   r10         --
\ **   r9          lp      locals pointer
\ **   r8          --
\ **   r7          tos     cached top of stack
\ **   r6          psp     data stack pointer
\ **   r0-r5       scratch

get-tos 7 =  get-psp 6 =  and [if]
code UM*	\ u1 u2 -- ud
\ *G Perform unsigned-multiply between two numbers and return double
\ ** result.
  mov     r0, tos			\ r0=u2
  ldr     r1, [ psp, # 0 ]		\ r1=u1
  push    { psp }
\ build result in r7:r6
\ build temps in r5:r4:r3:r2
  lsl .s  r2, r1 # #16
  lsr .s  r2, r2 # #16			\ r2=u1l
  lsr .s  r3, r1 # #16			\ r3=u1h
  lsl .s  r4, r0 # 16
  lsr .s  r4, r4 # 16			\ r4=u2l
  lsr .s  r5, r0 # 16			\ r5=u2h
\ scratch = r0, r1
\ multiply low portions
  mov     r6, r2
  mul .s  r6, r4			\ r6=u1l*u2l
\ multiply high portions
  mov     r7, r3
  mul .s  r7, r5			\ r7=u1h*u2h
\ multiply and accumulate u1h * u2l
  mov     r0, r3
  mul .s  r0, r4			\ r0=u1h*u2l=xxxx.yyyy
  lsr .s  r1, r0 # #16			\ r1=r0h
  lsl .s  r0, r0 # #16			\ r1:r0=0000.xxxx:yyyy.0000
  add .s  r6, r6, r0			\ add into result
  adc .s  r7, r7, r1
\ multiply and accumulate u2h * u1l
  mov     r0, r5
  mul .s  r0, r2			\ r0=u2h*u1l=xxxx.yyyy
  lsr .s  r1, r0 # #16			\ r1=r0h
  lsl .s  r0, r0 # #16			\ r1:r0=0000.xxxx:yyyy.0000
  add .s  r6, r6, r0			\ add into result
  adc .s  r7, r7, r1

  mov    r5, r6
  pop    { psp }
  str    r5, [ psp, # 0 ]

  next,
end-code
[then]

: m*		\ n1 n2 -- d
\ *G Signed multiply yielding double result.
  2dup xor >r				\ sign of result
  abs swap abs swap um*			\ process unsigned
  r> 0<					\ apply sign of result
  if  dnegate  endif
;


\ ***********
\ *S Division
\ ***********
\ *P ARM Cortex-M0 provides no division instructions.

((
macro: udiv64_step	\ --
\ *G Cross compiler macro to perform one step of the unsigned
\ ** 64 bit by 32 bit division
  add .s  r1, r1, r1			\ 64 bit shift of dividend
  adc .s  r0, r0, r0
  mov     r3, r2			\ must not affect flags
  adc .s  r3, r3, r2			\ preserve dividend bit 63, R2=0
  sub .s  r0, r0, tos			\ trial subtraction, carry set (no borrow) if ok
  adc .s  r3, r3, r3			\ success if bit 0 or 1 set
  ne, if,
    add .s  r1, r1, # 1			\ succeeded, update quotient
  else,
    add .s  r0, r0, tos			\ failed, undo subtraction
  endif,
;m
))
code um/mod	\ ud1 u2 -- urem uquot
\ *G Full 64 by 32 unsigned division subroutine.
\ ** This routine uses a loop for code size.
  ldmia   psp ! { r0 }			\ dividend high
  ldr     r1, [ psp ]			\ dividend low
  mov .s  r4, # #32			\ loop counter
  mov .s  r2, # 0			\ always 0
  begin,
\    udiv64_step
    add .s  r1, r1, r1			\ 64 bit shift of dividend
    adc .s  r0, r0, r0
    mov     r3, r2			\ must not affect flags
    adc .s  r3, r3, r2			\ preserve dividend bit 63, R2=0
    sub .s  r0, r0, tos			\ trial subtraction, carry set (no borrow) if ok
    adc .s  r3, r3, r3			\ success if bit 0 or 1 set
    ne, if,
      add .s  r1, r1, # 1		\ succeeded, update quotient
    else,
      add .s  r0, r0, tos		\ failed, undo subtraction
    endif,
    sub .s  r4, r4, # 1
  eq, until,
  mov     tos, r1			\ move quotient
  str     r0, [ psp ]			\ move remainder
  next,
end-code

UseDoubles [if] 
: fm/mod	\ d n -- rem quot ; floored division
\ *G Perform a signed division of double number *\i{d} by single
\ ** number *\i{n} and return remainder and quotient using floored
\ ** division. See the ANS Forth specification for more details
\ ** of floored division.
  dup >r				\ sign of divisor
  2dup xor >r				\ sign of quotient
  >r					\ divisor
  dabs r@ abs um/mod $7FFF:FFFF and	\ unsigned divide, truncate overflow ; SFP006
  swap r> 0< ?negate swap		\ remainder takes sign of divisor
  r> 0< if				\ if quotient negative
    negate				\ apply sign of quotient
    over if				\ if remainder non-zero
      1-				\ decrement quotient
      r@ rot - swap			\ rem := divisor - rem
    endif
  endif
  r> drop
;
[then]

: sm/rem	\ d n -- rem quot ; symmetric division
\ *G Perform a signed division of double number *\i{d} by single
\ ** number *\i{n} and return remainder and quotient using
\ ** symmetric (normal) division.
  over >r                               \ save sign of dividend
  >r  dabs  r@ abs  um/mod              \ unsigned division
  r> r@ xor ?negate                     \ correct sign of quot.
  swap r> ?negate swap                  \ correct sign of rem.
;

: /mod          \ n1 n2 -- rem quot
\ *G Signed symmetric division of N1 by N2 single-precision
\ ** returning remainder and quotient. Symmetric.
  >r s>d r> sm/rem
;

: / 		\ n1 n2  -- n3
\ *G Standard signed division operator. n3 = n1/n2. Symmetric.
  >r s>d r> sm/rem nip
;

: u/ 		\ u1 u2  -- u3
\ *G Unsigned division operator. u3 = u1/u2.
  0 swap um/mod nip
;

: MOD 		\ n1 n2 -- n3
\ *G Return remainder of division of N1 by N2. n3 = n1 mod n2.
  /mod drop  ;

: M/ 		\ d n1 -- n2
\ *G Signed divide of a double by a single integer.
  sm/rem nip  ;

: MU/MOD 	\ d n -- rem d#quot
\ *G Perform an unsigned divide of a double by a single, returning
\ ** a single remainder and a double quotient.
  >r  0  r@  um/mod  r> swap >r  um/mod  r>  ;


\ *********************************
\ *S Scaling - multiply then divide
\ *********************************
\ *P These operations perform a multiply followed by a divide.
\ ** The intermediate result is in an extended form.  The point
\ ** of these operations is to avoid loss of precision.

: */MOD 	\ n1 n2 n3 -- n4 n4
\ *G Multiply n1 by n2 to give a double precision result, and then
\ ** divide it by n3 returning the remainder and quotient.
  >r m* r> sm/rem  ;

: */ 		\ n1 n2 n3 -- n4
\ *G Multiply n1 by n2 to give a double precision result, and then
\ ** divide it by n3 returning the quotient.
  */mod nip  ;

: m*/		\ d1 n2 n3 -- dquot
\ *G The result dquot=(d1*n2)/n3. The intermediate value d1*n2
\ ** is triple-precision to avoid loss of precision. In an ANS
\ ** Forth standard program n3 can only be a positive signed
\ ** number and a negative value for n3 generates an ambiguous
\ ** condition, which may cause an error on some implementations,
\ ** but not in this one.
  s>d >r abs >r				\ -- d1 n2 ; R: -- sign(n3) |n3|
  s>d >r abs				\ -- d1 |n2| ; R: -- sign(n3) |n3| sign(n2)
  -rot					\ -- |n2| d1 ; R: -- sign(n3) |n3| sign(n2)
  s>d r> xor				\ -- |n2| d1 sign(d1*n2) ; R: -- sign(n3) |n3|
  r> swap >r >r				\ -- |n2| d1 ; R: -- sign(n3) sign(d1*n2) |n3|
  dabs rot				\ -- |d1| |n2| ; R: -- sign(n3) sign(d1*n2) |n3|
  tuck um* 2swap um*			\ -- d1h*n2 d1l*n2 ; R: -- sign(n3) sign(d1*n2) |n3|
  swap >r  0 d+ r> -rot			\ -- t ; R: -- sign (n3) sign(d1*n2) |n3|
  r@ um/mod -rot r> um/mod nip swap	\ -- d ; R: -- sign(n3) sign(d1*n2)
  r> r> xor IF dnegate THEN		\ -- d
;


\ *********************
\ *S Stack manipulation
\ *********************

: NIP 		\ x1 x2 -- x2
\ *G Dispose of the second item on the data stack.
  nip  ;

: TUCK 		\ x1 x2 -- x2 x1 x2
\ *G Insert a copy of the top data stack item underneath the current
\ ** second item.
  tuck  ;

: PICK	 	\ xu .. x0 u -- xu .. x0 xu
\ *G Get a copy of the Nth data stack item and place on top of stack.
\ ** 0 PICK is equivalent to DUP.
  pick  ;

CODE ROLL 	\ xu xu-1 .. x0 u -- xu-1 .. x0 xu
\ *G Rotate the order of the top N stack items by one place such that
\ ** the current top of stack becomes the second item and the Nth item
\ ** becomes TOS. See also *\fo{ROT}.
  lsl .s  r0, tos # 2			\ r0=position of xu in bytes
  cmp     r0, # 0			\ u is valid?
  le, if,
    ldmia   psp ! { tos }		\   no - get new tos
    bx      link			\   no - exit
  endif,
  ldr     tos, [ psp ++ r0 ]		\ put xu in tos
L: ROLL1
  sub .s  r1, r0, # 4
  ldr     r2, [ psp ++ r1 ]
  str     r2, [ psp ++ r0 ]
  mov     .s r0, r1
  b .ne   ROLL1
  add .s  psp, psp, # 4
  next,
END-CODE

: ROT 	\ x1 x2 x3 -- x2 x3 x1
\ *G ROTate the positions of the top three stack items such that the
\ ** current top of stack becomes the second item. See also *\fo{ROLL}.
  rot  ;

: -ROT 	\ x1 x2 x3 -- x3 x1 x2
\ *G The inverse of *\fo{ROT}.
  -rot  ;

CODE >R 	\ x -- ; R: -- x
\ *G Push the current top item of the data stack onto the top of the
\ ** return stack.
  push    { tos }
  ldmia   psp ! { tos }
  next,
END-CODE

CODE R> 	\ -- x ; R: x --
\ *G Pop the top item from the return stack to the data stack.
  sub .s  psp, psp, # 4
  str     tos, [ psp ]
  pop     { tos }
  next,
END-CODE

CODE R@ 	\ --  x  ; R:  x -- x
\ *G Copy the top item from the return stack to the data stack.
  sub .s  psp, psp, # 4
  str     tos, [ psp ]
  ldr     tos, [ rsp ]
  next,
END-CODE

CODE 2>R 	\ x1 x2 -- ; R:  -- x1 x2
\ *G Transfer the two top data stack items to the return stack.
  ldmia   psp ! { r0, r1 }		\ r0=x1, r1=new tos
  push    { r0 }			\ push x1
  push    { tos }			\ push x2
  mov .s  tos, r1			\ new tos=r1
  next,
END-CODE

CODE 2R> 	\ -- x1 x2 ; R: x1 x2 --
\ *G Transfer the top two return stack items to the data stack.
  pop     { r0 r1 }			\ r0=x2, r1=x1
  sub .s  psp, psp, # 8
  str     r1 [ psp, # 0 ]
  str     tos [ psp, # 4 ]
  mov     tos, r0			\ tos=x2
  next,
END-CODE

UseDoubles [if] 
CODE 2R@ 	\ --  x1 x2  ; R:  x1 x2 -- x1 x2
\ *G Copy the top two return stack items to the data stack.
  ldr     r0, [ rsp, # 0 ]		\ r0=x2
  ldr     r1, [ rsp, # 4 ]		\ r1=x1
  sub .s  psp, psp, # 8
  str     r1 [ psp, # 0 ]
  str     tos [ psp, # 4 ]
  mov     tos, r0                       \ tos=x2
  next,
END-CODE
[then]

: SWAP		\ x1 x2 -- x2 x1
\ *G Exchange the top two data stack items.
  swap  ;

: DUP	 	\ x -- x x
\ *G DUPlicate the top stack item.
  dup  ;

: OVER	 	\ x1 x2 -- x1 x2 x1
\ *G Copy NOS to a new top-of-stack item.
  over  ;

: DROP 		\ x --
\ *G Lose the top data stack item and promote NOS to TOS.
  drop  ;

: 2DROP 	\ x1 x2 -- )
\ *G Discard the top two data stack items.
  2drop  ;

: 2SWAP 	\ x1 x2 x3 x4 -- x3 x4 x1 x2
\ *G Exchange the top two cell-pairs on the data stack.
  2swap  ;

code 2ROT 	\ x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2
\ *G Perform the *\fo{ROT} operation on three cell-pairs.
  ldmia   psp ! { r0-r4 }		\ tos=x6, r0=x5, r4=x1
  sub .s  psp, psp, # 5 cells		\ restore stack depth
  str     r4, [ psp, # 0 cells ]	\ x1
  str     tos [ psp, # 1 cells ]	\ x6
  str     r0, [ psp, # 2 cells ]	\ x5
  str     r1, [ psp, # 3 cells ]	\ x4
  str     r2, [ psp, # 4 cells ]	\ x3
  mov     tos, r3			\ x2 to tos
  next,
END-CODE

: 2DUP		\ x1 x2 -- x1 x2 x1 x2
\ *G DUPlicate the top cell-pair on the data stack.
  2dup  ;

: 2OVER 	\ x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2
\ *G As *\fo{OVER} but works with cell-pairs rather than single-cell items.
  2over  ;

: ?DUP		\ x -- | x
\ *G DUPlicate the top stack item only if it non-zero.
  ?dup  ;

CODE SP@ 	\ -- x
\ *G Get the current address value of the data-stack pointer.
  sub .s  psp, psp, # 4
  str     tos, [ psp ]
  mov     tos, psp
  next,
END-CODE

CODE SP! 	\ x --
\ *G Set the current address value of the data-stack pointer.
  mov     psp, tos
  ldmia   psp ! { tos }
  next,
END-CODE

CODE RP@ 	\ -- x
\ *G Get the current address value of the return-stack pointer.
  sub .s  psp, psp, # 4
  str     tos, [ psp ]
  mov     tos, rsp
  next,
END-CODE

CODE RP! 	\ x --
\ *G Set the current address value of the return-stack pointer.
  mov     rsp, tos
  ldmia   psp ! { tos }
  next,
END-CODE

CODE >RR 	\ x -- ; R: -- x
\ *G Push the current top item of the data stack onto the top of the
\ ** return stack as a return address
  mov .s  r0, # 1			\ set the T bit
  orr .s  tos, tos, r0
  push    { tos }
  ldmia   psp ! { tos }
  next,
END-CODE

CODE RR> 	\ -- x ; R: x --
\ *G Pop the caller's return address from the return stack.
  sub .s  psp, psp, # 4
  str     tos, [ psp ]
  pop     { tos }
  mov .s  r0, # 1			\ clear the T bit
  bic .s  tos, tos, r0
  next,
END-CODE

CODE RR@ 	\ --  x  ; R:  x -- x
\ *G Copy the top item from the return stack to the data stack.
  sub .s  psp, psp, # 4
  str     tos, [ psp ]
  ldr     tos, [ rsp ]
  mov .s  r0, # 1			\ clear the T bit
  bic .s  tos, tos, r0
  next,
END-CODE


\ ******************************
\ *S String and memory operators
\ ******************************

: COUNT 	\ c-addr1 -- c-addr2' u
\ *G Given the address of a counted string in memory this word will
\ ** return the address of the first character and the length in
\ ** characters of the string.
  count  ;

: /STRING 	\ c-addr1 u1 n -- c-addr2 u2
\ *G Modify a string address and length to remove the first N characters
\ ** from the string.
  /string  ;

CODE SKIP 	\ c-addr1 u1 char -- c-addr2 u2
\ *G Modify the string description by skipping over leading occurrences of
\ ** 'char'.
  ldmia   psp ! { r0, r1 }		\ tos=char, r0=len, r1=adr1
  add .s  r0, r0, # 1
L: SK1
  sub .s  r0, r0, # 1
  b .eq   SKIPDONE
  ldrb    r2, [ r1 ]
  cmp     r2, tos
  eq, if,
    add .s  r1, r1, # 1
    b       SK1
  endif,
L: SKIPDONE
  sub .s  psp, psp, # 4
  str     r1, [ psp ]			\ push c-addr2
  mov     tos, r0			\ u2
  next,
END-CODE

CODE SCAN 	\ c-addr1 u1 char -- c-addr2 u2
\ *G Look for first occurrence of *\i{char} in the string and
\ ** return a new string. *\i{C-addr2/u2} describes the string
\ ** with *\i{char} as the first character.
  ldmia   psp ! { r0, r1 }		\ tos=char, r0=len, r1=adr1
  add .s  r0, r0, # 1
L: SC1
  sub .s  r0, r0, # 1
  b .eq   SCANDONE
  ldrb    r2, [ r1 ]
  cmp r2, tos
  ne, if,
    add .s  r1, r1, # 1
    b       SC1
  endif,
L: SCANDONE
  sub .s  psp, psp, # 4
  str     r1, [ psp ]			\ push c-addr2
  mov     tos, r0			\ u2
  next,
END-CODE

CODE S=		\ c-addr1 c-addr2 u -- flag
\ *G Compare two same-length strings/memory blocks, returning TRUE if
\ ** they are identical.
  ldmia   psp ! { r0, r1 }		\ r0=adr2, r1=adr1
  mov .s  tos, tos
  b .eq   S=X                           \ TRUE as len=0
L: S=1
  ldrb    r2, [ r0 ]
  ldrb    r3, [ r1 ]
  add .s  r0, r0, # 1
  add .s  r1, r1, # 1
  cmp     r2, r3
  b .ne   S=X                           \ FALSE as mismatched chars
  sub .s  tos, tos, # 1
  b .ne   S=1                           \ if count=0 then TRUE
L: S=X
  eq, if,
    mov .s  tos, # 1
    neg .s  tos, tos			\ TRUE
  else,
    mov .s  tos, # 0			\ FALSE
  then,
  next,
END-CODE

: compare       \ c-addr1 u1 c-addr2 u2 -- n                    17.6.1.0935
\ *G Compare two strings. The return result is 0 for a match or can be
\ ** -ve/+ve indicating string differences.
\ ** If the two strings are identical, n is zero. If the two strings
\ ** are identical up to the length of the shorter string, n is
\ ** minus-one (-1) if u1 is less than u2 and one (1) otherwise.
\ ** If the two strings are not identical up to the length of the
\ ** shorter string, n is minus-one (-1) if the first non-matching
\ ** character in the string specified by c-addr1 u1 has a lesser
\ ** numeric value than the corresponding character in the string
\ ** specified by c-addr2 u2 and one (1) otherwise.
  rot swap                      \ c-addr1 c-addr2 u1 u2
  2dup - >r min                 \ c-addr1 c-addr2 minlen -- R: lendiff? --

  begin
    dup
  while
    -rot over c@ over c@ -	\ length c-addr1 c-addr2 (char1-char2)
    dup if			\ If chars are different
      r> drop >r		\  replace lendiff result with error code
      drop 0			\  and put 0 on TOS (make len==0 at BEGIN)
    else                        \ otherwise
      drop			\  discard difference
      1+ swap 1+ swap  rot 1-	\  increment addresses and decrement length
    then
  repeat
  drop 2drop                    \ remove addresses and null count from stack
                                \ -- ; R: result --
  r> dup if 0< 1 or then        \ make nice flag, 0 becomes 0, -ve becomes -1
                                \            and  +ve becomes 1
;

internal
: (search)	{ c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag }
  c-addr1 u1 u2 - 1+ bounds     	\ Search for first char of $2 in $1.  Note that
  ?do                           	\ we only check LEN($1)-LEN($2)+1 chars of $1.
    i c@  c-addr2 c@  = if        	\ Compare next char of $1 with first char of $2.
      i c-addr2 u2 s= if           	\ Is the whole of $2 present?
        i u1 i c-addr1 - - true 	\ Yes - build return stack parameters...
        unloop exit             	\ ...and bail out.
      then
    then
  loop
  c-addr1 u1 false            		\ End of loop and first char not found - no match.
;
external

: SEARCH	( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag  )
\ *G Search the string c-addr1/u1 for the string c-addr2/u2. If a match
\ ** is found return c-addr3/u3, the address of the start of the match
\ ** and the number of characters remaining in c-addr1/u1, plus flag f
\ ** set to true. If no match was found return c-addr1/u1 and f=0.
  2 pick over <         		\ Is $1 shorter than $2?
  if  2drop false exit  then   		\ Yes - $2 *can't* be in $1.
  dup 0 <=              		\ Is $2 zero length?
  if  2drop true exit  then    		\ Yes - we have found it at the start of $1.
  2 pick 0 <=           		\ Is $1 zero length?
  if  2drop false exit  then   		\ Yes - string not found.
  (search)
;

code cmove	\ asrc adest len --
\ *G Copy *\i{len} bytes of memory forwards from *\i{asrc} to *\i{adest}.
  mov .s  r3, tos                	\ r3=len
  ldmia   psp ! { r0, r1, tos }		\ r0=addr2, r1=addr1, r3=len, tos restored
  eq, if,
    bx      link			\ return if len=0, nothing to do
  endif,
  mov .s  r2, r0
  orr .s  r2, r2, r1			\ check alignment
  orr .s  r2, r2, r3			\ R2 := adr1|addr2|len
  mov .s  r4, # 3
  and .s  r4, r4, r2,			\ will be zero if aligned
  b .ne   cmovx3			\ if not aligned
L: CMOVX1
  ldr     r2, [ r1 ]
  str     r2, [ r0 ]
  add .s  r1, r1, # 4
  add .s  r0, r0, # 4
  sub .s  r3, r3, # 4
  b .ne   CMOVX1
  next,
L: CMOVX3
  ldrb    r2, [ r1 ]
  strb    r2, [ r0 ]
  add .s  r1, r1, # 1
  add .s  r0, r0, # 1
  sub .s  r3, r3, # 1
  b .ne   CMOVX3
  next,
END-CODE

CODE CMOVE>	\ c-addr1 c-addr2 u --
\ *G As *\fo{CMOVE} but working in the opposite direction,
\ ** copying the last character in the string first.
  mov .s  r3, tos                	\ r3=len
  ldmia   psp ! { r0, r1, tos }		\ r0=addr2, r1=addr1, r3=len, tos restored
  eq, if,
    bx      link			\ return if len=0, nothing to do
  endif,
  add .s  r0, r0, r3			\ addr2+len
  add .s  r1, r1, r3			\ addr1+len
  mov .s  r2, r0
  orr .s  r2, r2, r1			\ check alignment
  orr .s  r2, r2, r3			\ R2 := adr1|addr2|len
  mov .s  r4, # 3
  and .s  r4, r4, r2,			\ will be zero if aligned
  b .ne   CMV>3				\ byte by byte if not aligned
L: CMV>1	\ copy by 4 byte units
  sub .s  r1, r1, # 4
  sub .s  r0, r0, # 4
  ldr     r2, [ r1 ]
  str     r2, [ r0 ]
  sub .s  r3, r3, # 4
  b .ne   CMV>1
  next,
L: CMV>3	\ copy byte by byte
  sub .s  r1, r1, # 1
  sub .s  r0, r0, # 1
  ldrb    r2, [ r1 ]
  strb    r2, [ r0 ]
  sub .s  r3, r3, # 1
  b .ne   CMV>3
  next,
END-CODE

: ON		\ a-addr --
\ *G Given the address of a CELL this will set its contents to TRUE (-1).
  on  ;

: OFF	 	\ a-addr --
\ *G Given the address of a CELL this will set its contents to FALSE (0).
  off  ;

CODE C+!	\ b c-addr --
\ *G Add N to the character (byte) at memory address ADDR.
  ldmia   psp ! { r0 }
  ldrb    r1, [ tos ]
  add .s  r1, r1, r0
  strb    r1, [ tos ]
  ldmia   psp ! { tos }
  next,
END-CODE

CODE 2@		\ a-addr -- x1 x2
\ *G Fetch and return the two CELLS from memory ADDR and ADDR+sizeof(CELL).
\ ** The cell at the lower address is on the top of the stack.
  ldmia   tos ! { r0 r1 }		\ r0 from low address
  sub .s  psp, psp, # 4
  mov     tos, r0
  str     r1, [ psp ]
  next,
END-CODE

CODE 2! 	\ x1 x2 a-addr --
\ *G Store the two CELLS x1 and x2 at memory ADDR.
\ ** X2 is stored at ADDR and X1 is stored at ADDR+CELL.
  mov .s  r2, tos
  ldmia   psp ! { r0, r1, tos }		\ r0=x2, r1=x1
  stmia  r2 ! { r0 r1 }
  next,
END-CODE

CODE FILL	\ c-addr u char --
\ *G Fill LEN bytes of memory starting at ADDR with the byte information
\ ** specified as CHAR.
  mov .s  r2, tos
  ldmia   psp ! { r0, r1, tos }		\ r2=char, r0=count, r1=addr
  cmp     r0, # 0
  b .eq   FILX
L: FIL1
  strb    r2, [ r1 ]
  add .s  r1, r1, # 1
  sub .s  r0, r0, # 1
  b .ne FIL1
L: FILX
  next,
END-CODE

: +!		\ n|u a-addr --
\ *G Add N to the CELL at memory address ADDR.
  +!  ;

: INCR	 	\ a-addr --
\ *G Increment the data cell at a-addr by one.
  incr  ;

: DECR		\ a-addr --
\ *G Decrement the data cell at a-addr by one.
  decr  ;

: @		\ a-addr -- x
\ *G Fetch and return the CELL at memory ADDR.
  @  ;

: W@	 	\ a-addr -- w
\ *G Fetch and 0 extend the word (16 bit) at memory ADDR.
  w@  ;

: C@	 	\ c-addr -- char
\ *G Fetch and 0 extend the character at memory ADDR and return.
  c@  ;

: ! 		\ x a-addr --
\ *G Store the CELL quantity X at memory A-ADDR.
  !  ;

: W!	 	\ w a-addr --
\ *G Store the word (16 bit) quantity w at memory ADDR.
  w!  ;

: C!	 	\ char c-addr --
\ *G Store the character CHAR at memory C-ADDR.
  c!  ;

CODE UPPER 	\ c-addr u --
\ *G Convert the ASCII string described to upper-case. This operation
\ ** happens in place.
  ldmia   psp ! { r0 }			\ r0=addr, tos=len
  cmp     tos, # 0			\ check len=0
  b .eq   UPPER3
  mov .s  r2, # $DF			\ upper case mask
L: UPPER1
  ldrb    r1, [ r0 ]			\ fetch char and incr addr
  cmp     r1, # $60			\ check lower bound
  b .ls   UPPER2			\ skip if lower
  cmp     r1, # $7A			\ check upper bound
  ls, if,
    and .s  r1, r1, r2			\ if OK then mask
    strb    r1, [ r0 ]			\ if OK then store char
  endif,
l: UPPER2
  add .s  r0, r0, # 1
  sub .s  tos, tos, # 1			\ count
  b .ne   UPPER1			\ len=0 so exit
L: UPPER3
  ldmia psp ! { tos }
  next,
END-CODE

: TEST-BIT	\ mask c-addr -- flag
\ *G AND the mask with the contents of addr and return the result.
  c@ and  ;

: SET-BIT	\ mask c-addr --
\ *G Apply the mask ORred with the contents of c-addr.
\ ** Byte operation.
  bor!  ;

: RESET-BIT	\ mask c-addr --
\ *G Apply the mask inverted and ANDed with the contents of c-addr.
\ ** Byte operation.
  bbic!  ;

CODE TOGGLE-BIT	\ u c-addr --
\ *G Invert the bits at c-addr specified by the mask. Byte operation.
  ldmia   psp ! { r0 }			\ get mask
  ldrb    r1, [ tos ]			\ get byte from addr
  eor .s  r1, r1, r0			\ apply mask
  strb    r1, [ tos ]			\ store byte
  ldmia  psp ! { tos }			\ get new tos
  next,
END-CODE


\ **********************
\ *S Miscellaneous words
\ **********************

: NAME>	\ nfa -- cfa
\ *G Move a pointer from an NFA to the XT..
  dup c@ $1F and + 4 + -4 and  ;		\ 1 for count byte, 3 for aligning

\ On Cortex, name field is a multiple of 4 bytes so we only need to
\ check top bit of every fourth byte when working back from the cfa
\ to find the nfa
: >NAME	\ cfa -- nfa
\ *G Move a pointer from an XT back to the NFA or name-pointer.
\ ** If the original pointer was not an XT or if the definition
\ ** in question has no name header in the dictionary the
\ ** returned pointer will be useless. Care should be taken when
\ ** manipulating or scanning the Forth dictionary in this way.
  begin  4 - dup @ $80 and  until	\ nfa in low byte
;

internal
: (SEARCH-WORDLIST)	\ c-addr u ^^nfa -- 0 | xt 1 | xt -1
  begin
    @ dup
  while
    dup c@ $20 and 0= if		\ not hidden
      dup c@ $1F and            	\ -- c-addr u ^nfa nfalen ;
      2 pick = if			\ are the names the same *length*?
        dup 1+ 3 pick 3 pick s= if	\ are they the same *name*?
          nip nip
          dup name> swap
          c@ $40 and 0= if -1 else 1 then
          exit
        then
      then
    then
    n>link
  repeat
  drop 2drop 0
;
external

: SEARCH-WORDLIST	\ c-addr u wid -- 0|xt 1|xt -1
\ *G Search the given wordlist for a definition. If the definition is
\ ** not found then 0 is returned, otherwise the XT of the definition
\ ** is returned along with a non-zero code. A -ve code indicates a
\ ** "normal" definition and a +ve code indicates an *\fo{IMMEDIATE} word.
  dup 0= if
    nip nip
  else
    over 3 pick c@ + over @ 1- and      \ -- c-addr u wid thread# ;
    1+ cells +                          \ -- c-addr u ^nfa ;
    (search-wordlist)
  then
;

CODE DIGIT 	\ char n -- 0|n true
\ *G If the ASCII value *\i{CHAR} can be treated as a digit for a number
\ ** within the radix *\i{N} then return the digit and a TRUE flag, otherwise
\ ** return FALSE.
  ldmia   psp ! { r0 }			\ base in tos, char in r0
  sub .s  r0, r0, # $30                 \ '0'
  b .mi   DIG2				\ fail if char < '0'
  cmp     r0, # $0A
  b .lt   DIG1				\ true, '0' <= char <= '9@
  sub .s  r0, r0, # 7			\ convert 'A'..'F'
  cmp     r0, # $0A
  b .lt   DIG2				\ fail if result < 'A'
L: DIG1
  cmp     r0, tos			\ in range of base?
  lt, if,
    mov .s  tos, # 1			\ yes, tos=true
    neg .s  tos, tos
    sub .s  psp, psp, # 4
    str     r0, [ psp ]			\ push n
    next,
  endif,				\ exit
L: DIG2
  mov .s  tos, # 0                      \ tos=false
  next,                                 \ exit
END-CODE


\ **********************
\ *S Portability helpers
\ **********************
\ *P Using these words will make code easier to port between
\ ** 16, 32 and 64 bit targets.

CODE CELL+ 	\ a-addr1 -- a-addr2
\ *G Add the size of a CELL to the top-of stack.
  add .s  tos, tos, # 4
  next,
END-CODE

CODE CELLS 	\ n1 -- n2
\ *G Return the size in address units of N1 cells in memory.
  mov .s  tos, tos .lsl # 2
  next,
END-CODE

CODE CELL- 	\ a-addr1 -- a-addr2
\ *G Decrement an address by the size of a cell.
  sub .s  tos, tos, # 4
  next,
END-CODE

CODE CELL 	\ -- n
\ *G Return the size in address units of one CELL.
  sub .s  psp, psp, # 4
  str     tos, [ psp ]		 	\ save TOS
  mov .s  tos, # 4	             	\ return cell size
  next,
END-CODE

CODE CHAR+ 	\ c-addr1 -- c-addr2
\ *G Increment an address by the size of a character.
  add .s  tos, tos, # 1
  next,
END-CODE

: CHARS 	\ n1 -- n2
\ *G Return size in address units of N1 characters.
; immediate


\ ********************
\ *S Runtime for VALUE
\ ********************

internal

CODE VAL!	\ n -- ; store value address in-line
\ *G Store n at the inline address following this word.
\ ** INTERNAL.
\ N.B. The call to VAL! must be four-byte aligned
  mov     r1, link
  sub .s  r1, r1, # 1		\ inline address
  ldr     r0, [ r1 ]		\ get data address (in-line)
  add .s  r1, r1, # 5		\ step over, restore T bit
  mov     link, r1
  str     tos, [ r0 ]		\ and write data from tos
  ldmia   psp ! { tos }		\ restore TOS
  next,
END-CODE

CODE VAL@	\ -- n ; read value data address in-line
\ *G Read n from the inline address following this word.
\ ** INTERNAL.
  sub .s  psp, psp, # 4
  str     tos, [ psp ]
  mov     r1, link
  sub .s  r1, r1, # 1		\ inline address
  ldr     r0, [ r1 ]		\ get data address (in-line)
  add .s  r1, r1, # 5		\ step over, restore T bit
  mov     link, r1
  ldr     tos, [ r0 ]		\ read data into tos
  next,
END-CODE

\ VAL generates a literal, and so LIT is used instead.

external


\ **************************************
\ *S Supporting complation on the target
\ **************************************
\ *P Compilation on the target is supported for compilation into
\ ** RAM. The target's compiler is simplistic and gives neither
\ ** the code size nor the performance of cross-compiled code.
\ ** The support words are compiled without heads.

\ *P Direct compilation into Flash requires additional code.
\ ** If you need it and want support, please contact MPE.

interpreter also asm-access		\ black magic here!
  get-rsp  get-psp  get-tos
previous target
equ tos-reg  equ psp-reg  equ rsp-reg

internal
: opc32,	\ opc32 -- ; compile 32 bit opcode
  dup #16 rshift w, w,  ;

: opc32!	\ opc32 addr -- ; store a 32 bit opcode
  over #16 rshift over w!  2 + w!  ;

: opc32@	\ addr -- opc32 ; fetch a 32 bit opcode
  dup w@ #16 lshift  swap 2 + w@ or  ;

((
\ Short form for calls
  bl      <dest>			\ $+00
  ...					\ $+04, returns here
\ Long form for calls, start is NOT aligned
  ldr    r0, $ 6 +			\ $+00
  blx    r0				\ $+02
  b      $ 6 +				\ $+04  skip address
  addr					\ $+06  location must be aligned
))

: inRange24?	\ dest -- flag ; true if +/-24 bit
  here 4 + - $FF00:0000 $00FF:FFFE within?  ;

: GenJ		\ offset mask -- Jx ; J := (notI) xor S
  over and 0=  swap 0< xor  ;

: CalcTrel24	\ dest orig -- field24
\ calculate field for 24 bit Thumb branch
  4 + -  1 arshift			\ half word align
  dup $7FF and 				\ offset imm11 ;    bits 10..0  -> 10..0
  over #11 rshift $3FF and #16 lshift or \ merge imm10 ;     bits 20..11 -> 25..16
  over $0020:0000 GenJ $0000:2000 and or \ merge J1/bit13 ; I1=bit21    -> 13
  over $0040:0000 GenJ $0000:0800 and or \ merge J2/bit11 ; I2=bit22    -> 11
  swap 0< $0400:0000 and or		\ merge S to bit26
;

: bl24		\ dest orig -- opc
  calcTrel24 $F000:D000 or  ;

: pushLR.n,	\ -- ; lay 16 bit push of LR to return stack
  $B500 w,  ;				\ push { lr }

: nop.n,	\ -- ; lay 16 bit NOP
  $BF00 w, ;				\ nop .n

: !scall	\ dest addr --
\ *G Patch a BL DEST opcode at addr.
  tuck  bl24  swap opc32!  ;

: !lcall	\ dest addr --
\ *G Patch n MVL32 R0, # DEST+1 opcode at addr.
  swap 1 or swap			\ force T bit in dest
  6 + aligned !
;

$1E00 4 6 lshift or psp-reg 3 lshift or psp-reg or equ subPspIns
\ Instruction opcode for SUB  psp, psp, # 4
$6000 psp-reg 3 lshift or tos-reg or equ strTosIns
\ Instruction opcode for STR  tos, [ psp # 0 ]

: saveTos,	\ -- ; compiles push of tos to data stack
  subPspIns w,				\ sub  psp, psp, # 4
  strTosIns w,				\ template for str rt, [ rn, # 0 ]
;

: dataPtr@,	\ -- ; compiles fetch to TOS through LR
  $4670 w,				\ mov     r0, link
  $1E40 w,				\ sub .s  r0, r0, # 1
  $6800 tos-reg or w,			\ ldr     tos, [ r0, # 0 ]
;

: scall,	\ addr --
\ +G Compile a machine code BL to addr. No range checking is performed.
  here bl24 opc32,  ;

LongCalls? [if]
: hasBLins?	\ addr -- flag ; true if address contains BL
  opc32@ $F000:D000 and $F000:D000 =  ;

: lcall,	\ addr --
\ +G Compile a machine code long call to addr.
\ +* The address after the call is always aligned.
  here 3 and				\ if not aligned step 2
  if  nop.n,  endif
  $4800 w,				\ LDR r0, $ 4 +  (pc relative)
  $E001 w,				\ B   $+6 (skip over address)
  1 or ,				\ address
  nop.n,
  $4780 w,				\ BLX r0
;
((
: lcall,	\ addr --
\ +G Compile a machine code long call to addr.
\ +* The address after the call is always aligned.
  here 3 and 0=				\ if align step 2
  if  nop.n,  endif
  $4801 w,				\ LDR r0, $ 6 +  (pc relative)
  $4780 w,				\ BLX r0
  $E001 w,				\ B   $+6 (skip over address)
  1 or ,				\ address
;
))

: compileAligned,	\ xt --
\ +G Compile the word specified by xt into the current definition
\ +* so that the end of the call is on a cell boundary. Used for
\ +* words followed by an inline literal.
  dup 2+ inRange24? if
    here 2 and
    if  nop.n,  endif
    scall,  exit
  endif
  lcall,				\ ten bytes, tail aligned
;
[else]
: compileAligned,	\ xt --
  here 2 and
  if  nop.n,  endif
  scall,
;
[then]

: DOCOLON, 	\ --
\ +G Compile the runtime entry code required by colon definitions.
\ +* INTERNAL.
  pushLR.n,  ;				\ push { r14 }

external


\ *************************************
\ *S Defining words and runtime support
\ *************************************

ASMCODE
L: DOCREATE	\ -- addr
\ *G The run time action of *\fo{CREATE}. The call must be on a
\ ** four byte boundary. INTERNAL.
  sub .s  psp, psp, # 4			\ save TOS
  str     tos, [ psp, # 0 ]
  mov     r0, link			\ LINK is high register
  sub .s  r0, r0, # 1			\ remove T bit
  ldr     tos, [ r0, # 0 ]		\ get data address (in-line)
  pop     { pc }
end-code

internal

CODE LIT 	\ -- x
\ *G Code which when CALLED at runtime will return an inline cell
\ ** value. The call must be at a four byte boundary. INTERNAL.
  sub .s  psp, psp, # 4			\ save TOS
  str     tos, [ psp, # 0 ]
  mov     r0, link			\ LINK is high register
  sub .s  r0, r0, # 1			\ remove T bit
  ldr     tos, [ r0, # 0 ]		\ get data (in-line)
  add .s  r0, r0, # 5			\ restore T bit, step over data
  bx      r0
END-CODE

CODE (") 	\ -- a-addr ; return address of string, skip over it
\ *G Return the address of a counted string that is inline after the
\ ** CALLING word, and adjust the CALLING word's return address to
\ ** step over the inline string. The adjusted return address will
\ ** be at a four byte boundary.
\ ** See the definition of *\fo{(.")} for an example.
  sub .s  psp, psp, # 4			\ save TOS
  str     tos, [ psp, # 0 ]
  ldr     tos, [ rsp, # 0 ]		\ get return address of CALLER
  sub .s  tos, tos, # 1			\ remove Thumb bit
  ldrb    r1, [ tos, # 0 ]		\ length byte
  add .s  r1, r1, tos			\ r1=address+length
  add .s  r1, r1, # 4			\ +1 for count byte, +3 to align
  mov .s  r2, # 3			\ -4 AND -> 3 BIC
  bic .s  r1, r1, r2
  add .s  r1, r1, # 1			\ restore Thumb bit
  str     r1, [ rsp, # 0 ]		\ update return address
  next,
END-CODE

external

aligning? [if]
: aligned       \ addr -- addr'
\ *G Given an address pointer this word will return the next ALIGNED
\ ** address subject to system wide alignment restrictions.
  3 + -4 and
;
compiler
: aligned       \ addr -- addr'
  3 + -4 and
;
target
[then]

LongCalls? [if]
: compile,	\ xt --
\ *G Compile the word specified by xt into the current definition.
  dup inRange24?
  if  scall,  exit  endif
  lcall,
;

: >BODY		\ xt -- a-addr
\ *G Move a pointer from a CFA or "XT" to the definition BODY. This
\ ** should only be used with children of *\fo{CREATE}. E.g. if
\ ** *\fo{FOOBAR} is defined by *\fo{CREATE foobar}, then the
\ ** phrase *\fo{' foobar >body} would yield the same result as
\ ** executing *\fo{foobar}.
  dup 4 + hasBLins?			\ is BL instruction?
  if  8 + @  else  #12 + @  endif
;
[else]
: compile,	\ xt --
  scall,  ;
: >BODY		\ xt -- a-addr
  8 + @  ;
[then]

internal
LongCalls? [if]
: DOCREATE, 	\ --
\ *G Compile the run time action of *\fo{CREATE}.
\ ** INTERNAL.
  DOCREATE inRange24? if
    nop.n,  pushLR.n,  DOCREATE scall,
  else
    pushLR.n,  DOCREATE lcall,
  endif
  HERE 4+ ,
;

: (;CODE) 	\ -- ; R: a-addr --
\ *G Performed at compile time by *\fo{;CODE} and *\fo{DOES>}.
\ ** Patch the last word defined (by *\fo{CREATE}) to have the
\ ** run time actions that follow immediately after *\fo{(;CODE)}.
\ ** INTERNAL.
  r> -2 and  latest name>		\ -- runtime childxt ; SFP004
  dup 4 + hasBLins?			\ instruction after STR
  if  4 + !scall  else  2 + !lcall  endif
;
[else]
: DOCREATE, 	\ --
  nop.n,  pushLR.n,  DOCREATE scall,  HERE 4+ ,  ;

: (;CODE) 	\ -- ; R: a-addr --
\ *G Performed at compile time by *\fo{;CODE} and *\fo{DOES>}.
\ ** Patch the last word defined (by *\fo{CREATE}) to have the
\ ** run time actions that follow immediately after *\fo{(;CODE)}.
\ ** INTERNAL.
  r> -2 and  latest name> 4 + !scall  ;	\ SFP004
[then]
external

: CONSTANT 	\ x "<spaces>name" -- ; Exec: -- x
\ *G Create a new *\fo{CONSTANT} called *\fo{name} which has the
\ ** value *\i{x}. When *\fo{NAME} is executed *\i{x} is returned.
  CREATE -4 allot , ;CODE
  sub .s  psp, psp, # 4			\ save TOS
  str     tos, [ psp, # 0 ]
  mov     r0, link			\ LINK is high register
  sub .s  r0, r0, # 1			\ remove T bit
  ldr     tos, [ r0, # 0 ]		\ get data address (in-line)
  pop     { pc }
END-CODE

UseDoubles [if] 
: 2CONSTANT	\ Comp: x1 x2 "<spaces>name" -- ; Run: -- x1 x2
\ *G A two-cell equivalent of *\fo{CONSTANT}.
  create  cell negate allot , ,  ;code
  sub .s  psp, psp, # 8			\ save TOS
  str     tos, [ psp, # 4 ]
  mov     r0, link			\ LINK is high register
  sub .s  r0, r0, # 1			\ remove T bit
  ldr     tos, [ r0, # 0 ]		\ get data high
  ldr     r1, [ r0, # 4 ]		\ get data low
  str     r1, [ psp, # 0 ]
  pop     { pc }			\ exit
END-CODE
[then]

: VARIABLE 	\ "<spaces>name" -- ; Exec: -- a-addr
\ *G Create a new variable called *\fo{name}. When *\fo{name} is
\ ** executed the address of the data-cell is returned for use
\ ** with *\fo{@} and *\fo{!} operators.
  CREATE  0 ,  ;CODE
  sub .s  psp, psp, # 4			\ save TOS
  str     tos, [ psp, # 0 ]
  mov     r0, link			\ LINK is high register
  sub .s  r0, r0, # 1			\ remove T bit
  ldr     tos, [ r0, # 0 ]		\ get address
  pop     { pc }
END-CODE

UseDoubles [if] 
: 2VARIABLE	\ Comp: "<spaces>name" -- ; Run: -- a-addr
\ *G A two-cell equivalent of *\fo{VARIABLE}.
  create  0 , 0 ,  ;code
  sub .s  psp, psp, # 4			\ save TOS
  str     tos, [ psp, # 0 ]
  mov     r0, link			\ LINK is high register
  sub .s  r0, r0, # 1			\ remove T bit
  ldr     tos, [ r0, # 0 ]		\ get address
  pop     { pc }
END-CODE
[then]

: USER 		\ u "<spaces>name" -- ; Exec: -- addr ; SFP009
\ *G Create a new *\fo{USER} variable called *\fo{name}. The *\i{u}
\ ** parameter specifies the index into the user-area table at which
\ ** to place the* data. *\fo{USER} variables are located in a
\ ** separate area of memory for each task or interrupt. Use in
\ ** the form:
\ *C   $400 USER TaskData
  CREATE  -4 allot ,  ;CODE		\ replce data address by offset
  sub .s  psp, psp, # 4			\ save TOS
  str     tos, [ psp, # 0 ]
  mov     r0, link			\ LINK is high register
  sub .s  r0, r0, # 1			\ remove T bit
  ldr     tos, [ r0, # 0 ]		\ get offset
  add     tos, tos, up			\ add user pointer to user offset
  pop     { pc }
end-code

interpreter
: u#		\ "<name>"-- u
\ *G An *\fo{INTERPRETER} word that returns the index of the
\ ** *\fo{USER} variable whose name follows, e.g.
\ *C   u# S0
  ' >body
;
target

target-only
: :		\ C: "<spaces>name" -- colon-sys ; Exec: i*x -- j*x ; R: -- nest-sys
\ *G Begin a new definition called *\fo{name}.
  ?exec  !csp
  bl word count makeheader smudge
  ] docolon,
;
host&target

target-only
: :NONAME	\ C: -- colon-sys ; Exec: i*x -- i*x  ; R: -- nest-sys
\ *G Begin a new code definition which does not have a name. After the
\ ** definition is complete the semi-colon operator returns the XT of
\ ** newly compiled code on the stack.
  align here cell+                      \ return XT
  !csp                                  \ compiler stack security
  here last !  0 ,			\ lay null header to fool SMUDGE
  ] docolon,				\ compiler on, lay entry code
;
host&target

: DOES>		\ C: colon-sys1 -- colon-sys2 ; Run: -- ; R:  nest-sys --
\ *G Begin definition of the runtime-action of a child of a defining word.
\ ** See the section about defining words in *\i{Programming Forth}.
\ ** You should not use *\fo{RECURSE} after *\fo{DOES>}.
  postpone (;code)  saveTos, dataPtr@,  ;
IMMEDIATE

internal
: CRASH 	\ -- ; used as action of DEFER
\ *G The default action of a *\fo{DEFER}ed word, which is to
\ ** perform *\fo{#12 THROW}. INTERNAL.
  $0C throw  ;
external

: DEFER 	\ Comp: "<spaces>name" -- ; Run:  i*x -- j*x
\ *G Creates a new *\fo{DEFER}ed word. A default action,
\ ** *\fo{CRASH}, is assigned.
  CREATE  ['] CRASH ,  ;CODE
  mov     r0, link			\ LINK is high register
  sub .s  r0, r0, # 1			\ remove T bit
  ldr     r0, [ r0, # 0 ]		\ get data address
  ldr     r1, [ r0, # 0 ]		\ get xt
  mov .s  r2, # 1
  orr .s  r1, r1, r2			\ force T bit
  pop     { r3 }			\ get return address
  mov     link, r3
  bx      r1
END-CODE

: FIELD		\ size n "<spaces>name" -- size+n ; Exec: addr -- addr+n
\ *G Create a new field of *\i{n} bytes within a structure so far
\ ** of *\i{size} bytes.
  CREATE  -4 allot over , +  ;CODE
  mov     r0, link			\ LINK is high register
  sub .s  r0, r0, # 1			\ remove T bit
  ldr     r1 [ r0, # 0 ]		\ get offset
  add .s  tos, tos, r1			\ add to base addr
  pop     { pc }
END-CODE


\ ************************
\ *S Structure compilation
\ ************************
\ *P These words define high level branches. They are used by the structure
\ ** words such as *\fo{IF} and *\fo{AGAIN}.

internal

: >mark         \ -- addr
\ *G Mark the start of a forward branch. HIGH LEVEL CONSTRUCTS ONLY.
\ ** INTERNAL.
  here  0 ,  ;

: >resolve      \ addr --
\ *G Resolve absolute target of forward branch. HIGH LEVEL CONSTRUCTS ONLY.
\ ** INTERNAL.
  here 1 or swap !  ;			\ absolute dest+bit0

: <mark         \ -- addr
\ *G Mark the start (destination) of a backward branch.
\ ** HIGH LEVEL CONSTRUCTS ONLY.
\ ** INTERNAL.
  here  ;

: <resolve      \ addr --
\ *G Resolve a backward branch to addr.
\ ** HIGH LEVEL CONSTRUCTS ONLY.
\ ** INTERNAL.
  1 or ,  ;	               		\ absolute + bit0 for target

synonym >c_res_branch >resolve	\ addr -- ; fix up forward referenced branch
\ *G See *\fo{>RESOLVE}.
\ ** INTERNAL.
synonym c_mrk_branch< <mark	\ -- addr ; mark destination of backward branch
\ *G See *\fo{<MARK}.
\ ** INTERNAL.


\ **********************
\ *S Branch constructors
\ **********************
\ *P Used when compiling code on the target.

: c_branch<     \ addr --
\ *G Lay the code for an unconditional backward branch.
\ ** INTERNAL.
  ['] branch compileAligned, <resolve  ;

: c_?branch<    \ addr --
\ *G Lay the code for a conditional backward branch.
  ['] ?branch compileAligned, <resolve  ;

: c_branch>     \ -- addr
\ *G Lay the code for a forward referenced unconditional branch.
\ ** INTERNAL.
  ['] branch compileAligned,  >mark  ;

: c_?branch>    \ -- addr
\ *G Lay the code for a forward referenced conditional branch.
\ ** INTERNAL.
  ['] ?branch compileAligned,  >mark  ;


\ *****************
\ *S Main compilers
\ *****************

: c_lit		\ lit --
\ *G Compile the code for a literal of value *\i{lit}.
\ ** INTERNAL.
  ['] lit compileAligned,  ,  ;

: c_drop        \ --
\ *G Compile the code for *\fo{DROP}.
\ ** INTERNAL.
  postpone drop  ;

: c_exit        \ --
\ *G Compile the code for *\fo{EXIT}.
\ ** INTERNAL.
  $BD00 w,  ;				\ pop { pc }

: c_do          \ C: -- do-sys ; Run: n1|u1 n2|u2 -- ; R: -- loop-sys
\ *G Compile the code for *\fo{DO}.
\ ** INTERNAL.
  ['] (do) compileAligned, >mark <mark  ;

: c_?DO         \ C: -- do-sys ; Run: n1|u1 n2|u2 -- ; R: -- | loop-sys
\ *G Compile the code for *\fo{?DO}.
\ ** INTERNAL.
  ['] (?do) compileAligned, >mark <mark  ;

: c_LOOP        \ C: do-sys -- ; Run: -- ; R: loop-sys1 -- | loop-sys2
\ *G Compile the code for *\fo{LOOP}.
\ ** INTERNAL.
  ['] (loop) compileAligned, <resolve >resolve  ;

: c_+LOOP       \ C: do-sys -- ; Run: -- ; R: loop-sys1 -- | loop-sys2
\ *G Compile the code for *\fo{+LOOP}.
\ ** INTERNAL.
  ['] (+loop) compileAligned, <resolve >resolve  ;

variable NextCaseTarg	\ -- addr
\ *G Holds the entry point of the current *\fo{CASE} structure.
\ ** INTERNAL.

: c_case        \ -- addr
\ *G Compile the code for *\fo{CASE}.
\ ** INTERNAL.
  NextCaseTarg @  <mark NextCaseTarg !  ;

: c_OF          \ C: -- of-sys ; Run: x1 x2 -- | x1
\ *G Compile the code for *\fo{OF}.
\ ** INTERNAL.
  ['] (of) compileAligned, >mark  ;

: c_ENDOF	\ C: case-sys1 of-sys -- case-sys2 ; Run: --
\ *G Compile the code for *\fo{ENDOF}.
\ ** INTERNAL.
  c_branch> swap >c_res_branch  ;

: FIX-EXITS     \ n1..nn --
\ *G Compile the code to resolve the forward branches at the end
\ ** of a *\fo{CASE} structure.
\ ** INTERNAL.
  begin
    sp@ csp @ <>
  while
    >c_res_branch
  repeat
;

: c_ENDCASE     \ C: case-sys -- ; Run: x --
\ *G Compile the code for *\fo{ENDCASE}.
\ ** INTERNAL.
  c_drop  fix-exits  NextCaseTarg !  ;

FullCase? [if]
: c_END-CASE    \ C: case-sys -- ; Run: x --
\ *G Compile the code for *\fo{END-CASE}.
\ ** INTERNAL. Only compiled if the equate *\fo{FullCase?} is non-zero.
  fix-exits  NextCaseTarg !  ;

: c_NEXTCASE    \ C: case-sys -- ; Run: x --
\ *G Compile the code for *\fo{NEXTCASE}.
\ ** INTERNAL. Only compiled if the equate *\fo{FullCase?} is non-zero.
  c_drop  NextCaseTarg @ c_branch<  fix-exits  NextCaseTarg !  ;

: c_?OF         \ C: -- of-sys ; Run: flag --
\ *G Compile the code for *\fo{?OF}.
\ ** INTERNAL. Only compiled if the equate *\fo{FullCase?} is non-zero.
  c_?branch>  ;
[then]

external


\ ****************
\ *S Miscellaneous
\ ****************

code di		\ --
\ *G Disable interrupts.
  cps     .id .i
  next,
end-code

code ei		\ --
\ *G Enable interrupts.
  cps     .ie .i
  next,
end-code

code [I		\ R: -- x1 x2
\ *G Preserve interrupt/exception status on the return stack,
\ ** and disable interrupts/exceptions except reset, NMI and
\ ** HardFault. The state is restored by *\fo{I]}.
  mrs     r0, PRIMASK			\ get status
  cps     .id .i
  push .n { r0 }
  next,
end-code

code I]		\ R: x1 x2 --
\ *G Restore interrupt status saved by *\fo{[I} from the return
\ ** stack.
  pop .n  { r0 }
  msr     PRIMASK r0
  next,
end-code

: setMask	\ value mask addr -- ; cell operation
\ *G Clear the *\i{mask} bits at *\i{addr} and set (or) the
\ ** bits defined by *\i{value}.
  tuck @				\ -- value addr mask x
  swap invert and			\ -- value addr x'
  rot or				\ -- addr x''
  swap !
;

: init-io	\ addr --
\ *G Copy the contents of the I/O set up table to an I/O device.
\ ** Each element of the table is of the form addr (cell) followed
\ ** by data (cell). The table is terminated by an address of 0.
  begin
    dup @
   while
    dup 2@ !  2 cells +
  repeat
  drop
;


\ ******
\ *> ###
\ ******

decimal
