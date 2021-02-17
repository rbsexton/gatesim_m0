\ Operator methods used by VALUE and LOCAL variables

((
Released for use with the MPE Forth Cross Compiler by:

Copyright (c) 1998-1999
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
20070517 MPE002 Made $LOCAL$ string upper case to avoid in-place
		writes to Flash.
20040211 MPE001 Revamped for HEADER and MAKEHEADER change in v6.2
))


\ ===========
\ *! methods
\ *T Target VALUE and local variables
\ ===========
\ *P The file *\i{COMMON\METHODS.FTH} implements the compilation
\ ** of *\fo{VALUE}s and the ANS Forth *\fo{LOCALS|} syntax for
\ ** compilation on the target. Compilation of this file requires
\ ** CPU dependent support, usually  called *\i{LOCAL.FTH} in the
\ ** *\i{%CpuDir%} directory, and MPE standard control files will
\ ** compile these files if the equate *\fo{TARGET-LOCALS?} is
\ ** set non-zero in the control file.

\ *P Note that this file is only provided for full ANS compliance.
\ ** The MPE extended local variable syntax is provided by the
\ ** cross compiler, and is much more powerful and more readable.

\ *P Note also that compilation of %CpuDir%\LOCAL.FTH may be required if
\ ** you cross compile words with more than four input arguments.


\ ***************
\ Basic operators
\ ***************

(( \ allows new types to be defined on the target
variable OPERATORTYPE		\ used at compile time for prefix operator

: OPERATOR      \ n -- ; define an operator
  create immediate
    ,
  does>
    @c operatortype !
;
))

internal
variable OPERATORTYPE		\ used at compile time for prefix operator
external

interpreter
: OPERATOR      \ n -- ; define an operator in the cross compiler
\ *G An interpreter definition that build new operators such as "to" and "addr".
  create immediate
    ,
  does>
    @c operatortype !
;
target

decimal

32bit? [if]
 0 dup equ DEFAULTOP operator DEFAULT
 1 dup equ STOREOP operator ->                 \ store operator
 1                 operator TO                 \   "
 2 dup equ ADDROP operator ADDR                \ address operator
\ 3 dup equ INCOP operator INC
\ 4 dup equ DECOP operator DEC
\ 5 dup equ ADDOP operator ADD
\ 6 dup equ ZEROOP operator ZERO
\ 7 dup equ SUBOP operator SUB
\ 8 dup equ SIZEOFOP operator SIZEOF
\ 9 dup equ SETOP operator SET
\ 10 dup equ OFFSETOFOP operator OFFSETOF
[else]
 0     equ DEFAULTOP
 1 dup equ STOREOP operator to                 \ store operator
 1                 operator ->                 \   "
 2 dup equ ADDROP operator ADDR                \ address operator
[then]

internal
32bit? [if]
: BAD-METHOD    \ -- ;
  cr ." Invalid operator for this type"
  #-13 throw
;
[else]
: BAD-METHOD    \ -- ;
  #-13 throw
;
[then]
external


\ *******************
\ VALUE defining word
\ *******************

internal
: VAL-COMPILE/EXECUTE   \ value xt[c] xt[e] --
  state @ if
    drop  compile, ,
  else
    nip  execute
  endif
;
external

: VALUE         \ n -- ; --  n ; n VALUE <name>
\ *G Creates a variable of initial value n that returns its contents
\ ** when referenced. To store to a child of VALUE use "n to <child>".
[ harvard? ] [if]
  there constant
    immediate  ,(r)
[else]
  create
    immediate  ,
[then]
  does>
    case  operatortype @  operatortype off
      defaultop of  ['] val@  ['] @     val-compile/execute  endof
      storeop   of  ['] val!  ['] !     val-compile/execute  endof
      addrop    of  state @ if  c_lit  endif                 endof
                      bad-method
    endcase
;


\ ******************************
\ high-level ANS local variables
\ ******************************

target-locals? [if]

[defined] environment [if]                                                                            \ MSD002
  also environment definitions

  #16  constant #LOCALS		\ -- true ; max number of locals allowed
  TRUE constant LOCALS          \ -- true ; extension present
  TRUE constant LOCALS-EXT      \ -- true ; extension present

  previous definitions
[then]


\ ----------------------------------
\ local variable compiler for target
\ ----------------------------------

vocabulary LOCALVARS			\ in temp area for local vars

internal

variable TDP                            \ temp dp for local use
variable OLDDP                          \ saved current dp
variable OLDLAST                        \ saved last nfa
variable OLDCURRENT                     \ saved current
variable LVCOUNT                        \ current local variable
variable lvcountmax                     \ total offset required
variable LOCAL-OFFSET                   \ offset used for local dictionary
$400 local-offset !                     \ declare how far above dp to lay temp words

create LOCAL-MARKER  ", $LOCAL$"	\ MUST be upper case ; MPE002

: >TEMP-DICT	\ -- ; switch in temp dictionary
  dp @ olddp !                          \ first save current dp
  tdp @ dp !                            \ use temp dp
  last @ oldlast !                      \ save last
  current @ oldcurrent !                \ save vocabs
  ['] localvars >body @ current !
;

: >REAL-DICT	\ -- ; switch out temp dictionary
  dp @ tdp !                            \ save temp dp
  olddp @ dp !                          \ use real current dp
  oldlast @ last !                      \ restore last
  oldcurrent @ current !                \ restore vocabs
;

: START-LOCALS	\ --
  dp @ local-offset @ + tdp !		\ create new dp
  also localvars
  >temp-dict
[defined] $create [if]
  local-marker $create
[else]
  local-marker count makeheader docreate,
[then]
  >real-dict
;

: FORGET-LOCALS	\ --
  >temp-dict
  local-marker $forget
  >real-dict
  tdp off
  lvcount off
  previous
;

[defined] MAKEHEADER [if]
: LVAR		\ c-addr u -- ; build temp local var
  makeheader docreate,
  immediate                             \ and mark as immediate
  lvcount @ ,                           \ lay offset
Does>
  @
  OperatorType @off                     \ any prefixed operator ???
  case
    DEFAULTOP of  ['] lv@ compile, ,  endof	\ lay function and offset
    STOREOP   of  ['] lv! compile, ,  endof	\ lay function and offset
    ADDROP    of  ['] lv  compile, ,  endof	\ lay function and offset
                   bad-method
  endcase
;
[else]
: LVAR		\ c-addr u -- ; build temp local var
  align                                 \ force DP to even address
  2dup upper                            \ convert to upper case
  2dup current @ search-wordlist        \ see if name exists    \ MSD001
  if
    ." Local variable "
    >name CR .name
    ." is redefined "                   \ yes, say so
  then
  here cell+ place                      \ copy string to HERE+cell
  header docreate,
  immediate                             \ and mark as immediate
  lvcount @ ,                           \ lay offset
Does>
  @c
  OperatorType @off                     \ any prefixed operator?
  case
    DEFAULTOP of  ['] lv@ compile, ,  endof	\ lay function and offset
    STOREOP   of  ['] lv! compile, ,  endof	\ lay function and offset
    ADDROP    of  ['] lv  compile, ,  endof	\ lay function and offset
                   bad-method
  endcase
;
[then]

: MAKELVAR	\ c-addr u -- ; build temp local var
  >temp-dict
  lvar
  >real-dict
;

: DROP-LOCALS	\ -- ; cleanup code for local stack vars
  lvcount @                             \ only compile if we had lvars
  if  compile rellvs  endif		\ number of vars to lose
;

: CLEANUP-LOCALS	\ -- ; cleanup code for semicolon
  lvcount @                             \ only compile if we had lvars
  if  forget-locals  endif		\ now lose local definitions
;

external

: (LOCAL)	\ Comp: c-addr u -- ; Exec: -- x ; define local var
\ *G When executed during compilation, defines a local variable
\ ** whose name is given by c-addr/u. If u is zero, c-addr is ignored
\ ** and compilation of local variables is assumed to finish.
\ ** When the word containing the local variable executes, the local
\ ** variable is initialised from the stack. When the
\ ** local variable executes, its value is returned. The local variable
\ ** may be written to by preceding its name with TO. This word is
\ ** provided for the construction of user-defined local variable notations.
\ ** This word is only provided for ANS compatibility, and locals created
\ ** by it cannot be optimised by the VFX code generator.
  lvcount @ 0= if
    start-locals                        \ initialise locals
    [ 2 cells ] literal lvcount !	\ 2 cells used for frame management
    compile makelvs
    here -rot  0 ,                      \ count will be fixed up later
  endif
  ?dup if                               \ OK not last
    makelvar                            \ build temp local variable
    cell lvcount +!                     \ bump for next
  else                                  \ is last local
    drop
    lvcount @ 2 cells -                 \ discount frame management in patchup of MAKELVS
[ 32bit? ] [if]
    u4/ over w!                         \ fix up two *words* after MAKELVS
    0 swap 2+ w!                        \ #args #locals
[then]
[ 16bit? ] [if]
    u2/ over c!                         \ fix up two *bytes* after MAKELVS
    0 swap 1+ c!                        \ #args #locals
[then]
  endif
;

: LOCALS|	\ "name...name |" --
\ *G Create named local variables <name1> to <namen>. At run time
\ ** the stack effect is ( xn..x1 -- ), such that <name1> is
\ ** initialised with x1 and <namen> is initialised with xn.
\ ** Note that this means that the order of declaration is the reverse
\ ** of the order used in stack comments!
\ ** When referenced, a local variable returns its value. To write
\ ** to a local, precede its name with TO.
\ *P In the example below, a and b are named inputs.
\ *E : foo      \ a b --
\ **   locals| b a |
\ **   a b +  cr .
\ **   a b *  cr .
\ ** ;
  lvcount @ abort" previous LOCALS| not finished"
  >in @
  0
  begin
    bl word
    dup     c@ 1 =
    swap 1+ c@ [char] | =
    and not
   while
    cell +
  repeat
  lvcountmax !
  >in !
  begin
    bl parse
    over c@ [char] | -
    over 1 - or
  while
    (local)
  repeat
  2drop
  0 0 (local)
;
IMMEDIATE

[then]


\ ======
\ *> ###
\ ======

