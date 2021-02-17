\ MPE High level ANS kernel for embedded systems

((
Released for use with the MPE Forth Cross Compiler by:
and copyright (c) 1998-2004, 2006, 2008, 2010, 2012

MicroProcessor Engineering
133 Hill Lane
Southampton SO15 5AF
England

tel: +44 (0)23 8063 1441
fax: +44 (0)23 8033 9691
net: mpe@mpeforth.com
     tech-support@mpeforth.com
web: www.mpeforth.com


To do
=====
Factor COLD into initialisation and Forth entry.

Remove in-place usage of UPPER because it can cause writes to Flash.
UCMOVE may be useful.

Change NEXT-CASE to be compatible with VFX Forth.

Remove uses of PAD where possible.

Change error codes to ANS codes.


Change history
==============
20130128 SFP015 Used more efficient version of UPC.
20120824 SFP014 Added BigKernel? option
20101215 SFP013 Made >NUMBER case-insensitive.
20100623 MPE012 Changed POSTPONE to be smaller and less sensitive
		to target architecture.
20090704 MPE011 Improved overlap detection in MOVE so that we can
		make better use of optimised CMOVEs, e.g. ARM.
20081218 SFP010 Removed [IODEV and IODEV].
		Added [IO and IO].
20060307 MPE009 Added compiler macro for >THREADS.
20040401 MPE008 Added NEXTCASE.
20040225 MPE007 Bulletproofed PLACE.
20040218 MPE006 Added SETCONSOLE for 32 bit systems.
20030320 SFP005 Added FLUSHKEYS for 32 bit tagets.
                Modified ACCEPT and HALT? to ignore LF characters.
20020910 SFP004 Removed ERROR and ?ERROR
20020704 SFP003 Corrected REPEAT - how did this survive so long?
20011112 SFP002 Unmarked: start of DOCGEN documentation - an ongoing
                process.
20000614 SFP001 Unmarked reordering to reduce forward refs and size.
))


\ =========
\ *! kernel
\ *T High level kernel kernel62.fth.
\ =========

only forth definitions
decimal

+short-branches


\ ************************
\ *S Configuration options
\ ************************
\ *P The following option equates  are set if not already before
\ ** *\i{kernel62.fth) is compiled.

[undefined] BigKernel? [if]
1 equ BigKernel?	\ --
\ *G Set this false to reduce the kernel size and lose some
\ ** words.
[then]


\ *****************************************************
\ Compatibility definitions for Harvard/Non-Harvard use
\ *****************************************************

harvard? 0=  [undefined] @c  and [if]
compiler
: c@c		c@  ;
: @c		@  ;
: 2@c		2@  ;
: !c		!  ;
: cmovec	cmove  ;
: countc	count  ;
target
[then]


\ ------------------------------------------------------------------------------
\       character equates
\ ------------------------------------------------------------------------------

$07 equ ABELL                           \ sound
$08 equ BSIN                            \ back space from key
$7F equ DELIN                           \ delete from key
$08 equ BSOUT                           \ back space for emit
$09 equ ATAB                            \ tab
$0D equ ACR                             \ carriage return
$0A equ ALF                             \ line feed
$0C equ FFEED                           \ form feed
$20 equ ABL                             \ space
$2E equ ADOT                            \ .


\ -----------------
\ *S User variables
\ -----------------

variable next-user	\ -- addr
\ *G Next valid offset for a *\fo{USER} variable created by *\fo{+USER}.
  next-user off

interpreter
: +user-offset	\ -- offset ; user offset of next +USER allocation
  next-user @
;

: +user		\ size --
\ *G Creates a *\fo{USER} variable size bytes long at the offset given
\ ** by *\fo{NEXT-USER} and updates it.
  next-user @ user
  next-user +!
;
target

tasking? [if]
tcb-size +user SELF		\ task identifier and TCB
\ *G When multitasking is enabled by setting the equate *\fo{TASKING?}
\ ** the task control block for a task occupies *\fo{TCB-SIZE} bytes at
\ ** the start of the user area. Thus the user area pointer
\ ** also acts as a pointer to the task control block.
[endif]
+user-offset equ s0-offset
cell +user S0		\ base of data stack
\ *G Holds the initial setting of the data stack pointer.
\ ** N.B. *\fo{S0}, *\fo{R0}, *\fo{#TIB} and *\fo{'TIB} must be
\ ** defined in that order.
+user-offset equ r0-offset
cell +user R0           \ base of return stack
\ *G Holds the initial setting of the return stack pointer.
cell +user #TIB		\ number of chars currently in TIB
\ *G Holds the number of characters currently in *\fo{TIB}.
cell +user 'TIB		\ address of TIB
\ *G Holds the address of *\fo{TIB}, the terminal input buffer.
cell +user >IN		\ offset into TIB
\ *G Holds the current character position being processed in the
\ ** input stream.
cell +user XON/XOFF	\ true if XON/XOFF protocol in use
\ *G True when console is using XON/XOFF protocol.
cell +user ECHOING	\ true if echoing
\ *G True when console is echoing input characters.
cell +user OUT		\ number of chars displayed on current line
\ *G Holds the number of chars displayed on current output line.
\ ** Reset by CR.
cell +user BASE		\ current numeric conversion base
\ *G Holds the current numeric conversion base
cell +user HLD		\ used during number formatting
\ *G Holds data used during number formatting
cell +user #L		\ number of cells converted by NUMBER?
\ *G Holds the number of cells converted by *\fo{NUMBER?}
cell +user #D		\ number of digits converted by NUMBER?
\ *G Holds the number of digits converted by *\fo{NUMBER?}
cell +user DPL		\ position of double number character id
\ *G Holds the number of characters after the double number indicator
\ ** character. *\fo{DPL} is initialised to -1, which indicates a
\ ** single number, and is incremented for each character after
\ ** the separator.
cell +user HANDLER	\ used in catch and throw
\ *G Holds the address of the previous exception frame.
cell +user OPVEC	\ output vector
\ *G Holds the address of the I/O vector for the current output device.
cell +user IPVEC	\ input vector
\ *G Holds the address of the I/O vector for the current input device.
cell +user 'AbortText   \ Address of text from ABORT"
\ *G Set by the run-time action of *\fo{ABORT"} to hold the address
\ ** of the counted string used by *\fo{ABORT" <text>"}.

32bit? [if]		\ size of hold buffer
  #68 			\ depends on stack width in bits
[else]
  #34
[then]
  dup next-user +!  equ PICNUMSIZE
#64 chars dup +user PAD
\ *G A temporary string scratch buffer.
  equ PADSIZE

cr ." Kernel USER area size is " next-user @ .


\ -------------------
\ *S System Constants
\ -------------------
\ *P Various constants for the internal system.

\ *D FALSE       The well formed flag version for a logical negative
\ *D TRUE        The well formed flag version for a logical positive
\ *D BL          An internal constant for blank space
\ *D C/L         Max chars/line for internal displays under C/LINE

\ *D #VOCS       Maximum number of Vocabularies in search order
\ *D VSIZE       Size of *\fo{CONTEXT} area for search order
\ *D XON         XON character for serial line flow control
\ *D XOFF        XOFF character for serial line flow control

 0 constant FALSE
-1 constant TRUE
$20 constant BL                         \ space character
$40 constant C/L                        \ characters / line for display functions

32bit? [if]     \ choose visibility in 32 bit, choose size in 16 bit
$11 constant XON                        \ XON for flow control
$13 constant XOFF                       \ XOFF for flow control

8 constant #VOCS                        \ maximum number of vocabularies in search order
#VOCS cell * constant VSIZE             \ maximum size of search order area CONTEXT
[else]
$11 equ XON                             \ XON for flow control
$13 equ XOFF                            \ XOFF for flow control

8 equ #VOCS                             \ maximum number of vocabularies in search order
#VOCS cells equ VSIZE                   \ maximum size of search order area CONTEXT
[then]


\ -------------------------------
\ *S System VARIABLEs and Buffers
\ -------------------------------
\ *N Variables
\ *P Note that *\fo{FENCE}, *\fo{DP} and *\fo{VOC-LINK} must be
\ ** declared in that order.
\ *D WIDTH       maximum target name size
\ *D FENCE       protected dictionary
\ *D DP          dictionary pointer
\ *D VOC-LINK    links vocabularies
\ *D RP          Harvard targets only. The equivalent of DP for DATA space.
\ *D SCR         If *\fo{BLOCKS?} true; for mass storage
\ *D BLK         If *\fo{BLOCKS?} true; user input dev: 0 for keyboard, >0 for block
\ *D CURRENT     Vocabulary/wordlist in which to put new definitions
\ *D STATE       Interpreting=0 or compiling=-1
\ *D CSP         Preserved stack pointer for compile time error checking
\ *D CONTEXT     Search order array
\ *D LAST        Points to name field of last definition
\ *D #THREADS    Default number of threads in new wordlists

variable WIDTH      $1F width !         \ max name width
variable FENCE                          \ protected dictionary
variable DP                             \ dictionary pointer
variable VOC-LINK                       \ links vocabs.
harvard? [if]
variable RP				\ RAM dictionary pointer
[then]
blocks? [if]
variable SCR                            \ for mass storage
variable BLK                            \ user input dev: 0 for keyboard, >0 for block
[then]
variable CURRENT                        \ vocab in which to put new definitions.
variable STATE                          \ interpreting or comp.
variable CSP                            \ stack pointer
variable CONTEXT vsize cell - allot-ram \ search order array
variable LAST                           \ points to last NFA
variable #THREADS   $10 #threads !      \ threads in vocab


\ =================
\ *S Deferred words
\ =================

defer NUMBER?	\ addr -- d/n/- 2/1/0
\ *G Attempt to convert the counted string at 'addr' to an integer.
\ ** The return result is either 0 for failed, 1 for a single-cell
\ ** return result (followed by that cell) or 2 for a double-cell return.
\ ** The ASCII number string supplied can also contain implicit radix
\ ** over-rides. A leading $ enforces hexadecimal, a leading # enforces
\ ** decimal and a leading % enforces binary. Hexadecimal numbers can
\ ** can also be specified by a leading '0x' or trailing 'h'.
\ ** When one of the floating point packs is compiled,
\ ** the action of *\fo{NUMBER?} is changed.

((
defer ERROR	\ n -- ; error handler
\ *G The standard error handler reports error n. If the system is
\ ** loading, the offending line will be displayed. Now implemented
\ ** by default as a synonym for *\fo{THROW}.
\ ** Removed from v6.2 onwards. Use *\fo{THROW} instead.
))


\ --------------------------
\ *S Predefined Vocabularies
\ --------------------------

( *D FORTH       Is the standard general purpose vocabulary            )
( *D ROOT        This vocabulary holds the bare minimum functions     )

vocabulary FORTH
vocabulary ROOT


\ ========================
\ *S Vectored I/O handling
\ ========================
\ *N Introduction
\ *P The standard console Forth I/O words (*\fo{KEY?}, *\fo{KEY},
\ ** *\fo{EMIT}, *\fo{TYPE} and *\fo{CR}) can be used with any I/O
\ ** device by placing the address of a table of xts in the
\ ** *\fo{USER} variables *\fo{IPVEC} and *\fo{OPVEC}. *\fo{IPVEC}
\ ** (input vector) controls the actions of *\fo{KEY?} and *\fo{KEY},
\ ** and *\fo{OPVEC}(output vector) controls the actions of *\fo{EMIT},
\ ** *\fo{TYPE} and *\fo{CR}.
\ ** Adding a new device is matter of writing the five primitives,
\ ** building the table, and storing the address of the table in
\ ** the pointers *\fo{IPVEC} and *\fo{OPVEC} to make the new
\ ** device active. Any initialisation must be performed before
\ ** the device is made active.

\ *P Note that for the output words (*\fo{EMIT}, *\fo{TYPE} and
\ ** *\fo{CR}) the *\fo{USER} variable *\fo{OUT} is handled in the
\ ** kernel before the funtion in the table is called.

\ *N Building a vector table
\ *P The example below is taken from an ARM implementation.
\ *E create Console1      \ -- addr
\ **   ' serkey1i ,             \ -- char
\ **   ' serkey?1i ,            \ -- flag
\ **   ' seremit1 ,             \ char --
\ **   ' sertype1 ,             \ c-addr len --
\ **   ' serCR1 ,               \ --
\ **
\ ** Console1 opvec !  Console1 ipvec !

\ *N Generic I/O words

((	\ easier to expand
: ipfunc	\ n -- ; i*x -- j*x
  create
    cells ,
  does>
    @c ipvec @ + @c execute
;

: opfunc	\ n -- ; i*x -- j*x
  create
    cells ,
  does>
    @c opvec @ + @c execute
;

0 ipfunc KEY	\ -- char ; receive char
1 ipfunc KEY?	\ -- flag ; check receive char
2 opfunc EMIT	\ -- char ; display char
3 opfunc TYPE	\ caddr len -- ; display string
4 opfunc CR	\ -- ; display new line
5 opfunc TYPEC	\ caddr len -- ; display string in code space (Harvard only)
))

\ slightly shorter in most systems.
harvard? [if]
: KEY?		\ -- flag ; check receive char
\ *G Return true if a character is available at the current input
\ ** device.
  ipvec @ cell+  @C execute  ;
: KEY	        \ -- char ; receive char
\ *G Wait until the current input device receives a character and
\ ** return it.
  ipvec @ ( 0 cells + ) @C execute  ;
: EMIT		\ -- char ; display char
\ *G Display char on the current I/O device. *\fo{OUT} is incremented
\ ** before executing the vector function.
  1 out +!
  opvec @ [ 2 cells ] literal + @C execute  ;
: TYPE		\ caddr len -- ; display string
\ *G Display/write the string on the current output device.
\ ** Len is added to *\fo{OUT} before executing the vector function.
  dup out +!
  opvec @ [ 3 cells ] literal + @C execute  ;
: CR		\ -- ; display new line
\ *G Perform the equivalent of a CR/LF pair on the current output
\ ** device. *\fo{OUT} is zeroed. before executing the vector
\ ** function.
  out off
  opvec @ [ 4 cells ] literal + @C execute  ;
: TYPEC		\ caddr len -- ; display string
\ *G Display/write the string from CODE space on the current
\ ** output device.
\ ** Len is added to *\fo{OUT} before executing the vector function.
\ ** N.B. Harvard targets only. In non-Harvard targets, this is a
\ ** synonym for *\fo{TYPE}.
  dup out +!
  opvec @ [ 5 cells ] literal + @C execute  ;
[else]
: KEY?		\ -- flag ; check receive char
  ipvec @ cell+  @ execute  ;
: key	        \ -- char ; receive char
  ipvec @ ( 0 cells + ) @ execute  ;
: EMIT		\ -- char ; display char
  1 out +!
  opvec @ [ 2 cells ] literal + @ execute  ;
: TYPE		\ caddr len -- ; display string
  dup out +!
  opvec @ [ 3 cells ] literal + @ execute  ;
: CR		\ -- ; display new line
  out off
  opvec @ [ 4 cells ] literal + @ execute  ;
Synonym typec type
[then]

: SPACE         \ --
\ *G Output a blank space (ASCII 32) character.
  bl emit
;

: SPACES        \ n --
\ *G Output 'n' spaces, where 'n' > 0.
\ ** If 'n' < 0, no action is taken.
  0 max 0 ?do  space  loop
;

32bit? [if]
: FlushKeys	\ --
\ *G Compiled for 32 bit systems to flush any pending input that
\ ** might be returned by *\fo{KEY}.
  begin  key? while  key drop  repeat
;

: SetConsole	\ device --
\ *G Sets *\fo{KEY} and *\fo{EMIT} and friends to use the given
\ ** device for terminal I/O. Compiled for 32 bit systems, but is
\ ** also part of *\i{LIBRARY.FTH}.
  dup ipvec !  opvec !
;

[undefined] rr> [if]	\ linear cell-sized return address
: [io		\ -- ; R: -- ipvec opvec
\ *G Save the current I/O devices on the return stack.
  r>  ipvec @ >r opvec @ >r  >r  ;
: io]		\ -- ; R: ipvec opvec --
\ *G Restore the I/O devices from the return stack.
  r>  r> opvec ! r> ipvec !  >r  ;
[else]
: [io		\ -- ; R: -- ipvec opvec
  rr>  ipvec @ >r opvec @ >r  >rr  ;
: io]		\ -- ; R: ipvec opvec --
  rr>  r> opvec ! r> ipvec !  >rr  ;
[then]

[then]


\ -----------------------------
\ *S String and memory operations
\ -----------------------------
\ *P Some of these words may be coded for performance.
\ ** If they are predefined, the high level versions
\ ** will not be compiled.

\ *P For byte-addressed CPUs (nearly all except DSPs) this kernel
\ ** assumes that a character is an 8 bit byte, i.e. that:
\ *E   char = byte = address-unit

[undefined] place [if]
: PLACE         \ c-addr1 u c-addr2 -- ; copies uncounted string to counted
\ *G Place the string c-addr1/u as a counted string at c-addr2.
  2dup 2>r  1+ swap move  2r> c!    	\ MPE007
;
[then]

[undefined] bounds [if]
: BOUNDS	\ addr len -- addr+len addr
\ *G Modify the address and length parameters to provide an end-address
\ ** and start-address pair suitable for a *\fo{DO ... LOOP} construct.
  over + swap
;
[then]

[undefined] upc [if]
: UPC           \ char -- char'
\ *G If char is in the range 'a' to 'z' convert it to upper case.
\ ** *\fo{UPC} is English language specific.
  dup [char] a >= if			\ SFP015
    dup [char] z <= if
      $DF and
    then
  then
;
[then]

[undefined] upper [if]
: UPPER 	\ c-addr u --
\ *G Convert the ASCII string described to upper-case. This operation
\ ** happens in place.
\ ** Note that this word is language specific and is written to
\ ** handle English only.
  bounds
  ?do  i c@ upc i c!  loop
;
[then]

: ERASE         \ a-addr u --
\ *G Erase U bytes of memory from A-ADDR with 0.
  0 fill
;

BigKernel? [if]
: BLANK         \ a-addr u --
\ *G Blank U bytes of memory from A-ADDR using ASCII 32 (space).
  bl fill
;
[then]


\ ------------------------
\ *S Dictionary management
\ ------------------------

: HERE          \ -- addr
\ *G Return the current dictionary pointer which is the first
\ ** address-unit of free space within the system.
  dp @
;
((
optimised? [if]
compiler
: HERE          \ -- addr
  dp @
;
target
[then]
))

: ALLOT         \ n --
\ *G Allocate N address-units of data space from the current value of
\ ** *\fo{HERE} and move the pointer.
  dp +!
;

aligning? [if]
[undefined] aligned [if]
: aligned       \ addr -- addr'
\ *G Given an address pointer this word will return the next
\ ** *\fo{ALIGNED} address subject to system wide alignment
\ ** restrictions.
  [ cell 1- ] literal +
  [ cell negate ] literal and
;
[then]

: ALIGN         \ --
\ *G *\fo{ALIGN} dictionary pointer using the same rules as
\ ** *\fo{ALIGNED}.
  here aligned dp !
;
[else]
: aligned       \ addr -- addr' ; NOOP on this system
; immediate

: align		\ -- ; force alignment if required - NOOP on this system
; immediate

compiler
: aligned  ;
: align  ;
target
[then]

: LATEST        \ -- c-addr
\ *G Return the address of the name field of the last definition.
  Last @
;
compiler
: latest	( -- caddr ) Last @  ;
target

: SMUDGE        \ --
\ *G Toggle the *\fo{SMUDGE} bit of the latest definition.
  $20 latest toggle-bit
;

: ,             \ x --
\ *G Place the CELL value X into the dictionary at *\fo{HERE} and
\ ** increment the pointer.
  here !  cell allot
;

32bit? [if]
: W,            \ w --
\ *G Place the WORD value X into the dictionary at *\fo{HERE} and
\ ** increment the pointer. This word is not present on 16 bit
\ ** implementations.
  here w!  2 allot
;
[then]

: C,            \ char --
\ *G Place the CHAR value into the dictionary at *\fo{HERE} and
\ ** increment the pointer.
  here c!  1 allot
;

harvard? [if]
: there		\ -- addr
\ *G Harvard targets only: Return the DATA space pointer.
  rp @  ;

: allot-ram	\ n --
\ *G Harvard targets only: *\fo{ALLOT} DATA space.
  rp +!  ;

: c,(r)		\ b --
\ *G Harvard targets only: The equivalent of *\fo{C,} for DATA space.
  there c!  1 allot-ram  ;

: ,(r)		\ n --
\ *G Harvard targets only: The equivalent of *\fo{,} for DATA space.
  there !  2 allot-ram  ;
[then]

compiler
: N>LINK        \ a-addr -- a-addr'
  cell-
;

: LINK>N        \ a-addr -- a-addr'
  cell+
;

: >#threads ;	\ -- ; compiler treats this as an immediate noop

: >THREADS      \ wid -- a-addr ; MPE009
  >#threads cell+
;

: >vocname
  cell-
;

: >VOC-LINK     \ wid -- a-addr
  2 cells -
;
target

BigKernel? [if]
: N>LINK        \ a-addr -- a-addr'
( *G Move a pointer from a NFA field to the Link Field.                 )
  cell-
;

: LINK>N        \ a-addr -- a-addr'
\ *G The inverse of *\fo{N>LINK}.
  cell+
;

: >LINK         \ a-addr -- a-addr'
( *G Move a pointer from an XT to the link field address.               )
  >name n>link
;

: LINK>         \ a-addr -- a-addr'
\ *G The inverse of *\fo{>LINK}.
  cell+ name>
;

internal

: >VOC-LINK     \ wid -- a-addr
\ *G Step from a wordlist identifier, wid, to the address of the
\ ** field containg the address of the previously defined wordlist.
  [ 2 cells ] literal -
;

: >#THREADS     \ wid -- a-addr ; for XC5 compatibility
\ *G Step from a wordlist identifier, wid, to the address of the
\ ** field containg the number of threads in the wordlist.
; immediate


: >THREADS      \ wid -- a-addr
\ *G Step from a wordlist identifier, wid, to the address of the
\ ** array containing the top NFA for each thread in the wordlist.
  >#threads cell+
;

: >VOCNAME      \ wid -- a-addr
\ *G Step from a wordlist identifier, wid, to the address of the
\ ** field pointing to the vocabulary name field.
  cell-
;

external
[then]		\ BigKernel?

-short-branches
: FIND          \ c-addr -- c-addr 0|xt 1|xt -1
\ *G Perform the *\fo{SEARCH-WORDLIST} operation on all wordlists
\ ** within the current search order. This definition takes a
\ ** counted string rather than a *\i{c-addr/u} pair. The counted
\ ** string is returned as well as the 0 on failure.
  dup count upper                       \ make sure search text is upper case
  dup c@ if                             \ if there is any search text
    context vsize bounds		\  for all word lists in search order:
    do
      dup count i @ search-wordlist     \    search word list for text
      ?dup if                           \    if text is found then bail out now
        rot drop 			\      discard text address
	unloop  exit
      then
    cell +loop
  then
  0
;
+short-branches

: .NAME         \ nfa --
\ *G The correct way to display a definition's name given an NFA.
  countc $1F and typec space
;

internal
: HASH-THREAD   \ wid c-addr -- a-addr
\ Given a wordlist identifier (wid) and the address of a counted
\ ** string for a word name, return the address of the dictionary
\ ** name thread that will contain the name.
  dup c@ swap 1+ c@ +			\ count & first char
  swap                                  \ get voc-cfa
  dup >#threads @ 1- rot and cells      \ mask for #threads
  swap >threads +                       \ form thread addr
;

: makeheader	\ c-addr len --
\ *G Given a word name as a string in *\i{addr/len} form, build a
\ ** dictionary header for the word.
  2dup upper				\ convert to upper case
\ check for redefinitions
  2dup current @ search-wordlist	\ see if name exists
  if  >name CR .NAME ." is redefined "  then
\ copy string into header and form link field
  align                                 \ force DP to even address
  here cell+ place			\ -- ; copy string to HERE+CELL
  current @ here cell+ hash-thread      \ find thread-addr
  dup @ ,                               \ lay old thread
  here swap !                           \ link this word
\ Set dictionary information
  here last !                           \ this is latest word
  here dup c@ width @ min 1+ allot	\ reserve space
  align
  $80 swap set-bit                      \ set high bit
;
external

((
: $CREATE       \ c-addr --
\ *G Perform the action of *\fo{CREATE} (below) but take the name from
\ ** a counted string. OBSOLETE: replace by:
\ *C  count makeheader docreate,
  count makeheader docreate,
;
))

: CREATE        \ --
\ *G Create a new definition in the dictionary. When the new definition
\ ** is executed it will return the address of the definition BODY.
  bl word count makeheader docreate,
;


\ ---------------------
\ *S String compilation
\ ---------------------

internal
harvard? [if]
: (C")          \ -- c-addr
  (")
;

: (S")          \ -- c-addr u
  (") countc
;

: (ABORT")      \ i*x x1 -- | i*x
  (") swap if
    'AbortText !  -2 throw
  else
    drop
  then
;

: (.")          \ --
  (") countc typec
;
[else]
: (C")          \ -- c-addr
\ *G The run-time action for *\fo{C"} which returns the address of and
\ ** steps over a counted string.
  (")
;

: (S")          \ -- c-addr u
\ *G The run-time action for *\fo{S"} which returns the address and
\ ** length of and steps over a string.
  (") count
;

: (ABORT")      \ i*x x1 -- | i*x
\ *G The run time action of *\fo{ABORT"}.
  (") swap if
    'AbortText !  -2 throw
  else
    drop
  then
;

: (.")          \ --
\ *G The run-time action of *\fo{."}.
  (") count type
;
[then]
external


\ -----------------------------
\ *S Pre-ANS Exception handlers
\ -----------------------------
\ *P Before the ANS Forth standard, these words were the primary
\ ** error handlers. They are provided for compatibility, but
\ ** wherever possible, the use of *\fo{CATCH} and *\fo{THROW} will
\ ** be found to be more flexible.

BigKernel? [if]
: ABORT         \ i*x -- ; R: j*x --
\ *G Performs *\fo{-1 THROW}. This is a compatibility word for earlier
\ ** versions of the kernel. Unfortunately, the earlier versions
\ ** gave problems when *\fo{ABORT} was used in interrupt service
\ ** routines or tasks. The new definition is brutal but consistent.
  -1 Throw
;

: ABORT"        \ Comp: "ccc<quote>" -- ; Run: i*x x1 -- | i*x ; R: j*x -- | j*x
\ *G If x1 is non-zero at run-time, store the address of the
\ ** following counted string in *\fo{USER} variable *\fo{'ABORTTEXT},
\ ** and perform *\fo{-2 THROW}. The text interpreter in *\fo{QUIT}
\ ** will (if reached) display the text.
  ?comp  compile (abort")  ",
; IMMEDIATE
[then]

((
: (Error)       \ n --
\ *G The default action of *\fo{ERROR}.
\ ** This definition has been removed from v6.2 onwards.
\ ** See the section about the changes from v6.1 to v6.2.
  cr ." Error # " .  space here $. cr   \ Issue error message
  -1 throw                              \ restart
;
assign (error)  to-do error

: ?ERROR        \ flag n --
\ *G If flag is true, perform "n ERROR", otherwise do nothing.
\ ** This definition has been removed from v6.2 onwards.
\ ** See the section about the changes from v6.1 to v6.2.
  swap if  error  else  drop  then
;
))


\ ============================
\ *S ANS words CATCH and THROW
\ ============================
\ *P *\fo{CATCH} and *\fo{THROW} form the basis of all Forth error
\ ** handling. The following description of *\fo{CATCH} and
\ ** *\fo{THROW} originates with Mitch Bradley and is taken from
\ ** an ANS Forth standard draft.

\ *P *\fo{CATCH} and *\fo{THROW} provide a reliable mechanism for
\ ** handling exceptions, without having to propagate exception
\ ** flags through multiple levels of word nesting. It is similar
\ ** in spirit to the "non-local return" mechanisms of many other
\ ** languages, such as C's *\b{setjmp()} and *\b{longjmp()},
\ ** and LISP's *\b{CATCH} and *\b{THROW}. In the Forth context,
\ ** *\fo{THROW} may be described as a "multi-level EXIT", with
\ ** *\fo{CATCH} marking a location to which a *\fo{THROW} may return.

\ *P Several similar Forth "multi-level EXIT" exception-handling
\ ** schemes have been described and used in past years. It is not
\ ** possible to implement such a scheme using only standard words
\ ** (other than *\fo{CATCH} and *\fo{THROW}), because there is no
\ ** portable way to "unwind" the return stack to a predetermined
\ ** place.

\ *P *\fo{THROW} also provides a convenient implementation technique
\ ** for the standard words *\fo{ABORT} and *\fo{ABORT"}, allowing
\ ** an application to define, through the use of *\fo{CATCH}, the
\ ** behavior in the event of a system *\fo{ABORT}.

\ =========================
\ *N Example implementation
\ =========================
\ *P This sample implementation of *\fo{CATCH} and *\fo{THROW}
\ ** uses the non-standard words described below. They or their
\ ** equivalents are available in many systems. Other
\ ** implementation strategies, including directly saving the
\ ** value of *\fo{DEPTH}, are possible if such words are not
\ ** available.
\ *D SP@  ( -- addr )  returns the address corresponding to the top of data stack.
\ *D SP!  ( addr -- )  sets the stack pointer to addr, thus restoring the stack
\ ** depth to the same depth that existed just before addr was acquired by executing SP@.
\ *D RP@  ( -- addr )  returns the address corresponding to the top of return stack.
\ *D RP!  ( addr -- )  sets the return stack pointer to addr, thus restoring the
\ ** return stack depth to the same depth that existed just before addr was
\ ** acquired by executing RP@.
\ *E nnn USER HANDLER   0 HANDLER !  \ last exception handler
\ ** : CATCH  ( xt -- exception# | 0 ) \ return addr on stack
\ ** 	SP@ >R	( xt )	\ save data stack pointer
\ ** 	HANDLER @ >R	( xt )	\ and previous handler
\ ** 	RP@ HANDLER !	( xt )	\ set current handler
\ ** 	EXECUTE	( )	\ execute returns if no THROW
\ ** 	R> HANDLER !	( )	\ restore previous handler
\ ** 	R> DROP	( )	\ discard saved stack ptr
\ ** 	0	( 0 )	\ normal completion
\ ** ;
\ ** : THROW  ( ??? exception# -- ??? exception# )
\ ** 	?DUP IF	( exc# )	\ 0 THROW is no-op
\ ** 		HANDLER @ RP!	( exc# )	\ restore prev return stack
\ ** 		R> HANDLER !	( exc# )	\ restore prev handler
\ ** 		R> SWAP >R	( saved-sp ) \ exc# on return stack
\ ** 		SP! DROP R>	( exc# )	\ restore stack
\ ** 		\	Return to the caller of CATCH because return
\ ** 		\	stack is restored to the state that existed
\ ** 		\	when CATCH began execution
\ ** 	THEN
\ ** ;
\ *P The ROM PowerForth implementation is similar to the one described above,
\ ** but not identical.

\ ==============
\ *N Example use
\ ==============
\ *P If *\fo{THROW} is executed with a non zero argument, the
\ ** effect is as if the corresponding *\fo{CATCH} had returned it.
\ ** In that case, the stack depth is the same as it was just
\ ** before *\fo{CATCH} began execution.
\ ** The values of the i*x stack arguments could have been modified
\ ** arbitrarily during the execution of xt.  In general, nothing useful
\ ** may be done with those stack items, but since their number is known
\ ** (because the stack depth is deterministic), the application may
\ ** *\fo{DROP} them to return to a predictable stack state.

\ *P Typical use:
\ *E : could-fail	\ -- char
\ **   KEY DUP [CHAR] Q =
\ **   IF  1 THROW  THEN
\ ** ;
\ **
\ ** : do-it		\ a b -- c
\ **   2DROP could-fail
\ ** ;
\ **
\ ** : try-it		\ --
\ **   1 2 ['] do-it CATCH IF
\ **     ( -- x1 x2 ) 2DROP ." There was an exception" CR
\ **   ELSE
\ **     ." The character was " EMIT CR
\ **   THEN
\ ** ;
\ **
\ ** : retry-it		\ --
\ **   BEGIN
\ **     1 2 ['] do-it CATCH
\ **    WHILE
\ **     ( -- x1 x2 ) 2DROP  ." Exception, keep trying" CR
\ **   REPEAT ( char )
\ **   ." The character was " EMIT CR
\ ** ;

\ ==========
\ *N Gotchas
\ ==========
\ *P If a *\fo{THROW} is performed without a *\fo{CATCH} in place,
\ ** the system will/may crash. As the current exception frame
\ ** is pointed to by the *\fo{USER} variable *\fo{HANDLER},
\ ** each task and interrupt handler will need a *\fo{CATCH} if
\ ** *\fo{THROW} is used inside it.

\ *P You can no longer use *\fo{ABORT} as a way of resetting the
\ ** data stack and calling *\fo{QUIT}. *\fo{ABORT} is now defined
\ ** as *\fo{-1 THROW}.

\ =============
\ *N User words
\ =============

[undefined] catch [if]
: CATCH         \ i*x xt -- j*x 0|i*x n
\ *G Execute the code at XT with an exception frame protecting it.
\ ** *\fo{CATCH} returns a 0 if no error has occurred, otherwise it
\ ** returns the throw-code passed to the last *\fo{THROW}.
  sp@ >r                                \ save DSP
  lp@ >r				\ save LP
  handler @ >r                          \ and last handler
  rp@ handler !                         \ set new handler
  execute                               \ this gets back if there's no THROW
  r> handler !                          \ restore previous handler
  r> drop				\ discard saved LP
  r> drop                               \ discard saved DSP
  0                                     \ good exit = 0
;
[then]

[undefined] throw [if]
: THROW         \ k*x n -- k*x|i*x n
\ *G Throw a non-zero exception code n back to the last *\fo{CATCH}
\ ** call. If n is 0, no action is taken except to *\fo{DROP} n.
  ?dup 0= if  exit  then		\ zero = no throw
  handler @ rp!				\ restore last RSP
  r> handler !				\ restore prev handler
  r> lp!				\ restore LP
  r> swap >r				\ n > RS
  sp! drop r>				\ restore DS
					\ Returns to caller of CATCH
;
[then]

: ?throw        \ flag throw-code -- ; SFP017
\ *G Perform a *\fo{THROW} of value throw-code if flag is non-zero,
\ ** otherwise do nothing except discard flag and throw-code.
  swap
  if  throw  else  drop  endif
;


\ ********************************
\ *S Formatted and unformatted i/o
\ ********************************

\ =======================
\ *N Setting number bases
\ =======================

: HEX           \ --
\ *G Change current radix to base 16.
  #16 base !
;

: DECIMAL       \ --
\ *G Change current radix to base 10.
  #10 base !
;

32bit? [if]
: OCTAL         \ --
\ *G Change current radix to base 8. 32 bit targets only.
  #8 base !
;
[then]

BigKernel? [if]
: BINARY        \ --
\ *G Change current radix to base 2.
  2 base !
;
[then]
compiler
: binary	( -- )  2 base !  ;
target

\ =================
\ *N Numeric output
\ =================

: HOLD          \ char --
( *G Insert the ascii 'char' value into the pictured numeric output     )
( ** string currently being assembled.                                  )
  -1 hld +!  hld @ c!
;

: SIGN          \ n --
( *G Insert the ascii 'minus' symbol into the numeric output string if  )
( ** 'n' is negative.                                                   )
  0< if  [char] - hold  then
;

: #             \ ud1 -- ud2
( *G Given a double number on the stack this will add the next digit to )
( ** the pictured numeric output buffer and return the next double      )
( ** number to work with. PLEASE NOTE THAT THE NUMERIC OP STRING IS     )
\ ** BUILT FROM RIGHT (l.s. ddigit) to LEFT (m.s. digit).
  base @ mu/mod rot 9 over <
  if  7 +  then
  [char] 0 + hold
;

: #S            \ ud1 -- ud2
\ *G Keep performing *\fo{#} until all digits are generated.
  begin  #  2dup or 0=  until
;

: <#            \ --
( *G Begin definition of a new numeric output string buffer.            )
  pad hld !
;

: #>            \ xd -- c-addr u
\ *G Terminate defnition of a numeric output string. Return the
\ ** address and length of the ASCII string.
  2drop  hld @ pad over -  ;

: -TRAILING     \ c-addr u1 -- c-addr u2
\ *G Modify a string address/length pair to ignore any trailing spaces.
  dup 0 ?do
    2dup + 1- c@ bl <> ?leave
    1-
  loop
;

: D.R           \ d n --
\ *G Output the double number 'd' using current radix, right justified
\ ** to 'n' characters. Padding is inserted using spaces on the left
\ ** side.
  >r swap over dabs <# #s rot sign #>
  r> over - spaces type
;

: D.            \ d --
\ *G Output the double number 'd' without padding.
  0 d.r space
;

: .             \ n --
\ *G Output the cell signed value 'n' without justification.
  s>d d.
;

: U.            \ u --
\ *G As with . but treat as unsigned.
  0 d.
;

: U.R           \ u n --
\ *G As with D.R but uses a single-unsigned cell value.
  0 swap d.r
;

: .R            \ n1 n2 --
\ *G As with D.R but uses a single-signed cell value.
  >r s>d r> d.r
;


\ ================
\ *N Numeric input
\ ================

internal

: SKIP-SIGN     \ addr1 len1 -- addr2 len2 t/f ; true if sign=negative
\ *G Inspect the first character of the string, if it is a '+'
\ ** or '-' character, step over the string. Returning true if the
\ ** character was a '-', otherwise return false.
  over c@ dup [char] + =
  swap [char] - =  dup >r or
  if  1 /string  then
  r>
;

: +DIGIT        \ d1 n -- d2 ; accumulates digit into double accumulator
\ *G Multiply d1 by the current radix and add n to it.
  swap base @ um* drop                  \ accumulate into double
  rot base @ um*  d+
  1 #d +!  dpl @ 1+                     \ incr. #digits, double?
  if  1 dpl +!  then			\ yes, incr. after d.p.
;

: +CHAR         \ char -- flag ; true if ok
\ *G This routine handles non-numeric characters, returning
\ ** true for valid characters. By default, the only acceptable
\ ** non-numeric character is the double-number separator ','.
  [char] , = dup if			\ not digit, try ","
    drop  dpl @ 0< if			\ is "," see if allowed
      dpl off  2                        \   yes, set up and flag
    else
      0                                 \   illegal, flag it
    then
  then
  dup #l !
;

: +ASCII-DIGIT  \ d1 char -- d2 flag ; true=ok
\ *G Accumulate the double number d1 with the conversion of char,
\ ** returning true if the character is a valid digit or part of an
\ ** integer.
  dup base @ digit if			\ digit?
    nip +digit  #l @                    \   yes, accumulate into double
  else
    +char                               \   not digit, try ","
  then
;

-Short-Branches
: (INTEGER?)    \ c-addr u -- d/n/- 2/1/0
\ *G The guts of *\fo{INTEGER?} but without the base override handling.
\ ** See *\fo{INTEGER?}
  #d off  -1 dpl !  1 #l !  skip-sign over if
    >r  0 0 2swap  bounds
    do
      I c@ +ascii-digit 0= ?leave
    loop
    r> ?dnegate
    #d @ 0=  #l @ 0= or if
      #l off  2drop
    else
      #l @ 1 =  if  drop  then
    then
  else
    drop 2drop  #l off
  then
  #l @
;
+Short-Branches

: Check-Prefix  \ addr len -- addr' len'
\ *G If any *\fo{BASE} override prefices or suffices are used
\ ** in the input string, set *\fo{BASE} accordingly and return
\ ** the string without the override characters.
[ 32bit? ] [if]
  over dup c@ [char] 0 =                \ check for string starting
  swap 1+ c@ [char] X =  and            \ with '0x' and more than
  over 2 > and                          \ two characters long
  if  2 /string  hex  exit  endif
  2dup + 1- c@ [char] H =               \ ending in H
  over 1 > and                          \ and longer than 1 char
  if  1-  hex  exit  endif
[then]
  Case  over c@                         \ get possible prefix char
    [char] $ Of 1 /string  hex     Endof \  $ for HEX
    [char] # Of 1 /string  decimal Endof \  # for DECIMAL
[ 32bit? ] [if]
    [char] @ Of 1 /string  octal   Endof \  @ for OCTAL
[then]
    [char] % Of 1 /string  binary  Endof \  % for BINARY
  EndCase
;

external

: Integer?      \ $addr -- n 1 | d 2 | 0
\ *G Attempt to convert the counted string at 'addr' to an integer.
\ ** The return result is either 0 for failed, 1 for a single-cell
\ ** return result followed by that cellor 2 for a double return.
\ ** The ASCII number string supplied can also contain implicit radix
\ ** over-rides. A leading $ enforces hexadecimal, a leading # enforces
\ ** decimal and a leading % enforces binary.
\ ** The prefix '@' is supported for octal numbers in 32 bit systems,
\ ** for which hexadecimal numbers can also be specified by a
\ ** leading '0x' or a trailing 'h'.
  count dup if				\ if something to do
    2dup upper				\   convert to upper case
    base @ >r                           \   preserve current base
    check-prefix (integer?)             \   check prefix, convert to number
    r> base !                           \   restore base
  Else
    2drop  #l off  0
  Endif
;
assign integer? to-do number?

: >NUMBER       \ ud1 c-addr1 u1 -- ud2 c-addr2 u2 ; convert all until non-digits
( *G Accumulate digits from string c-addr1/u2 into double number ud1	)
( ** to produce ud2 until the first non-convertible character is found.	)
( ** c-addr2/u2 represents the remaining string with c-addr2 pointing	)
( ** the non-convertible character. The number base for conversion is	)
\ ** defined by the contents of *\fo{USER} variable *\fo{BASE}.
\ ** *\fo{>NUMBER} is now case insensitive.
  dup 0 ?do
    over c@ upc base @ digit 0= ?leave	\ convert char to digit, get out if failed
    -rot 2>r +digit 2r>			\ accumulate digit
    1 /string				\ bump address, decr char count
  loop
;


\ ==========================
\ *S String input and output
\ ==========================

internal

: BS            \ -- ; destructive backspace
\ *G Perform a destructive backspace by issuing ASCII characters
\ ** 8, 20h, 8. If *\fo{OUT} is non-zero at the start, it is decremented
\ ** by one regardless of the actions of the device driver.
  out @ ?dup if
    bsout emit  space  bsout emit
    1- out !
  then
;

: ?BS           \ pos -- pos' step ; perform BS if pos non-zero
\ *G If pos is non-zero and *\fo{ECHOING} is set, perform *\fo{BS}
\ ** and return the size of the step, 0 or -1.
  dup 0<>  echoing @ 0<>  and if        \ pos non-zero and echo
    bs 1- -1                            \   perform backspace
  else
    0                                   \   no
  then
;

: SAVE-CH       \ char addr -- ; save as required
\ *G Save char at addr, and output the character if
\ ** *\fo{ECHOING} is set.
  over swap c!				\ stash character
  echoing @                             \ echo it?
  if  emit  else  drop  then            \ process accordingly
;

external

: ."            \ "ccc<quote>" --
\ *G Output the text upto the closing double-quotes character.
\ ** Use *\fo{.( <text>)} when interpreting.
  compile (.")  ",
;
IMMEDIATE

: $.            \ c-addr -- ; display counted string
\ *G Output a counted-string to the output device.
\ ** Note that on Harvard targets (e.g. 8051) c-addr is in
\ ** DATA space.
  count type
;

: ACCEPT	\ c-addr +n1 -- +n2 ; read up to LEN chars into ADDR
( *G Read a string of maximum size n1 characters to the buffer at	)
( ** c-addr, returning n2 the number of characters actually read. Input	)
( ** may be terminated by CR. The action may be input device specific.	)
\ ** If *\fo{ECHOING} is non-zero, characters are echoed.
\ ** If *\fo{XON/XOFF} is non-zero, an XON character is sent at the
\ ** start and an XOFF character is sent at the the end.
  dup 0=				\ check for pathological case
  if  nip exit  then
  xon/xoff @ if  xon emit  then         \ enable server
  0 -rot bounds                         \ form address limits
  ?do
    case key                            \ get character
      bsin  of ?bs               endof  \ BS
      atab  of bl i save-ch 1+ 1 endof  \ TAB, convert to space
\      alf   of bl i save-ch 1+ 1 endof  \ LF, convert to space
      alf   of 0 endof			\ LF, ignore
      acr   of bl i save-ch  leave endof  \ CR
      ffeed of bl i save-ch  leave endof  \ FF, treat like CR
      delin of ?bs               endof  \ DEL
             i save-ch  1+ 1 0		\ stash char, echo it?
    endcase
  +loop                                 \ round again
  xon/xoff @ if  xoff emit  then        \ disable server
;


\ ***********************
\ *S Source input control
\ ***********************

0 value SOURCE-ID	\ -- n ; indicates input source
\ *G Returns an indicator of which device is generating
\ ** source input. See the ANS specification for more details.

BigKernel? [if]
: TIB           \ -- c-addr ; return address of terminal i/p buffer
\ *G Returns the address of the terminal input buffer. Note that
\ ** tasks requiring user input must initialise the *\fo{USER}
\ ** variable *\fo{'TIB}. New code should use *\fo{SOURCE} and
\ ** *\fo{TO-SOURCE} instead for ANS Forth compatibility.
  'tib @
;

: TO-SOURCE     \ c-addr u --
\ *G Set the address and length of the system terminal input buffer.
\ ** These are held in the user variables *\fo{'TIB} and *\fo{#TIB}.
  #tib !  'tib !
;
[then]
compiler
: TO-SOURCE	( c-addr u -- )  #tib !  'tib !  ;
target

: SOURCE        \ -- c-addr u
( *G Returns the address and length of the current terminal input       )
( ** buffer.                                                            )
  'tib @  #tib @
;

blocks? [if]
: SAVE-INPUT    \ -- xn..x1 n
  source  source-id  blk @  >in @  5
;

: RESTORE-INPUT \ xn..x1 n -- flag
  5 = if				\ parameters are likely to be valid?
    >in ! blk ! to source-id to-source  \   set input source
    false                               \   return false flag for okay!
  else
    true                                \   return true flag, don't touch stack
  then
;
[else]
: SAVE-INPUT    \ -- xn..x1 n
\ *G Save all the details of the input source onto the data stack.
\ ** will do the job. If you want to move the data to the return
\ ** stack, *\fo{N>R} and *\fo{NR>} are available in some 32 bit
\ ** implementations.
  source  source-id  >in @  4
;

: RESTORE-INPUT \ xn..x1 n -- flag
\ *G Attempt to restore input specification from the data stack. If
\ ** the stack picture between *\fo{SAVE-INPUT} and
\ ** *\fo{RESTORE-INPUT} is not balanced, a non-zero is returned
\ ** in place of N. On success a 0 is returned.
  4 = if				\ parameters are likely to be valid?
    >in !  to source-id  to-source	\   set input source
    false                               \   return false flag for okay!
  else
    true                                \   return true flag, don't touch stack
  then
;
[then]

: QUERY         \ -- ; fetch line into TIB
( *G Reset the input source specification to the console and accept a   )
( ** line of text into the input buffer.                                )
  0 to source-id                        \ mark source as keyboard
  'tib @ tib-len 1- accept #tib !	\ "tib-len 1-" was #80
  >in off
;

blocks? [if]
: REFILL        \ -- flag ; refill input source
  source-id 0= if                       \ source is user input device?
    blk @ ?dup if                       \   coming from a BLOCK?
      1+ blk !  >in off                 \     increment block number, parse from start
    else
      query                             \     reload from keyboard
    then
    true                                \   always succeeds from user input device
  else
    false                               \ can't refill from EVALUATE or FILE
  then
;
[else]
: REFILL        \ -- flag ; refill input source
( *G Attempt to refill the terminal input buffer from the current       )
( ** source. This may be a file or the console. An attempt to refill    )
( ** when the input source is a string will fail. The return result     )
( ** is a flag indicating success with TRUE and failure with FALSE.    )
( ** A failure to refill when the input source is a text file indicates)
( ** the end of file condition.                                         )
  source-id if                          \ source is not user input device?
    false
  else
    query  true                         \ reload from keyboard, always succeeds
  then
;
[then]


\ ================
\ *S Text scanning
\ ================

: PARSE         \ char "ccc<char>" -- c-addr u
\ *G Parse the next token from the terminal input buffer using
\ ** <char> as the delimiter. The next token is returned as a
\ ** *\fo{c-addr/u} string description. Note that *\fo{PARSE} does
\ ** not skip leading delimiters. If you need to skip leading
\ ** delimiters, use *\fo{PARSE-WORD} instead.
  >r
  source >in @ /string                  \ remainder of input is substring
  over swap r> scan                     \ look for char
  >r over -                             \ subtract addresses to get length
  dup r> 0<> -                          \ if string found then bump >IN past char
  >in +!                                \ update >IN to reflect parsed text
;

internal
: PARSE-WORD    \ char -- c-addr u ; find token, skip leading chars
\ *G An alternative to *\fo{WORD} below. The return is a
\ ** *\i{c-addr/u} pair rather than a counted string and no copy
\ ** has occured, i.e. the contents of *\fo{HERE} are unaffected.
\ ** Because no intermediate global buffers are used *\fo{PARSE-WORD}
\ ** is more reliable than *\fo{WORD} for text scanning in multi-threaded
\ ** applications.
  >r  source tuck  >in @ /string
  r@ skip  over swap r> scan
  >r over -  rot r>  dup 0<> + - >in !
;
external

: WORD          \ char "<chars>ccc<char>" -- c-addr
\ *G Similar behaviour to the ANS word *\fo{PARSE} but the returned
\ ** string is described as a counted string.
  parse-word  here place  here
;


\ ----------------
\ *S Miscellaneous
\ ----------------

: HALT?         \ -- flag
\ *G Used in listed displays. This word will check the keyboard for a
\ ** 'pause' key <space>, if the key is pressed it will then wait for
\ ** a continue key or an abort key. The return flag is TRUE if abort
\ ** is requested. Line Feed (LF, ASCII 10) characters are ignored.
  key? dup 0=				\ key not pressed?
  if  exit  endif
  key dup ALF =				\ ignore LFs
  if  2drop 0  exit  endif
  dup bl =  swap 19 =  or if		\ ctrl s or space?
    drop				\   chuck previous flag
    key					\   get second char
    dup bl <>  over ALF <> and		\   not LF, ctrl q, space?
    swap 17 <> and
  then
;

internal

origin [if]	\ only if ORIGIN is non-zero
: origin-	\ addr -- addr'
\ *G If addr is non-zero, subtract the start address of the first
\ ** defined CDATA section.
\ ** This word is only compiled if the start address of the first
\ ** defined CDATA section is non-zero.
  dup if  origin -  then                \ normalize if non-zero
;

: origin+	\ addr -- addr' ; denormalise NFA again
\ *G If addr is non-zero, add the start address of the first
\ ** defined CDATA section.
\ ** This word is only compiled if the start address of the first
\ ** defined CDATA section is non-zero.
  dup if  origin +  then		\ normalize if non-zero
;
[else]
compiler	\ treat these as immediate noops
: origin-  ;
: origin+  ;
target
[then]

: nfa-buff      \ -- addr+len addr ; make a buffer for holding NFAs
\ *G Form a temporary buffer for holding NFAs. A factor for
\ ** *\fo{WORDS}.
  pad                                   \ buffer
  context @ >#threads @ cells           \ length
  bounds
;

: MAX-NFA       \ -- addr c-addr ; returns addr and top nfa
\ *G Return the thread address and NFA of the highest word
\ ** in the NFA buffer. A factor for *\fo{WORDS}.
  0 0                                   \ clear result
  nfa-buff do
    i @  over u> if			\ if greater
      2drop  i  i @                     \ replace current set
    then
  cell +loop                            \ step on
  origin+				\ de-normalize
;

: COPY-THREADS  \ addr --
\ *G Copy the threads of the *\fo{CONTEXT} wordlist to a
\ ** temporary NFA buffer for manipulation. A factor for
\ ** *\fo{WORDS}.
  context @ >threads                    \ source
  nfa-buff do                           \ destination/len
    dup @ origin- i !
    cell+
  cell +loop                            \ copy and normalise
  drop                                  \ discard source addr
;

external

also root definitions
: WORDS         \ --
\ *G Display the names of all definitions in the wordlist at the
\ ** top of the search-order.
  cr                                    \ set up
[defined] FlushKeys [if] FlushKeys [then]
  copy-threads                          \ copy out & normalize
  begin
    max-nfa dup                         \ while still one left
    halt? 0= and                        \ and no stop command
   while
    out @ c/l > if  cr  then            \ new line if needed
    dup .name                           \ show name
    n>link @c origin- swap !		\ find next
  repeat
  2drop
;
previous definitions

: MOVE          \ src dest len -- ; intelligent move
\ *G An intelligent memory move, chooses between *\fo{CMOVE} and
\ ** *\fo{CMOVE>} at runtime to avoid memory overlap problems.
\ ** Note that as ROM PowerForth characters are 8 bit, there is
\ ** an implicit connection between a byte and a character.
  >r  2dup swap - r@ u< if		\ overlap if dest-src<len ; MPE011
    r> cmove>				\ move down
  else
    r> cmove				\ move up
  then
;

: DEPTH         \ -- +n
\ *G Return the number of items on the data stack, excluding the count.
  s0 @ sp@ - cell- cell /
;

romforth? [if]
: UNUSED        \ -- u ; free dictionary space
  here appl-rom appl-rom-end within? if	\ in application area?
    appl-rom-end                        \ yes, select end of that
  else
    unused-top                          \ no, select end of main RAM
  then
  here -
;
[else]
: UNUSED        \ -- u ; free dictionary space
\ *G Return the number of bytes free in the dictionary.
  unused-top here -
;
[then]

harvard? [if]
idata sec-end cdata equ dtop
: .FREE         \ --
  unused u. ." code bytes, "
  dtop there - u. ." data"
;
[else]
: .FREE         \ --
\ *G Return the free dictionary space.
  unused u. ." bytes free"
;
[then]


\ -------------------
\ *S Wordlist control
\ -------------------

: WORDLIST      \ -- wid
( *G Create a new wordlist and return a unique identifier for it.       )
  align here
  #threads @ dup ,                      \ lay number of threads
  0 ?do 0 , loop                        \ zero allot threads
;

: VOCABULARY    \ -- ; VOCABULARY <name>
\ *G Create a *\fo{VOCABULARY} which is implemented as a named wordlist.
  create
    here 3 cells + dup ,                \ lay pointer to #threads
    voc-link @ ,  voc-link !            \ update voc-link
    latest ,                            \ pointer to name
    wordlist drop
  does>
    @c context !
;


also root definitions

' forth equ OLDFORTH

\ Forth is redefined in ROOT so that it is always available, the vocabulary
\ 'Forth' is actually within itself.
: FORTH         \ --
( *G Install *\fo{FORTH} wordlist into search-order.                          )
  oldforth execute
;

: FORTH-WORDLIST        \ -- wid
( *G Return the unique WID for the main FORTH wordlist.                 )
  oldforth >body @
;

: GET-CURRENT   \ -- wid
( *G Return the WID for the Wordlist which holds any definitions made   )
( ** at this point.                                                     )
  current @
;

: SET-CURRENT   \ wid --
( *G Change the wordlist which will hold future definitions.            )
  current !
;

: GET-ORDER     \ -- widn...wid1 n
( *G Return the list of WIDs which make up the current search-order.    )
( ** The last value returned on top-of-stack is the number of WIDs      )
( ** returned.                                                          )
  0                                     \ voc counter
  context vsize cell- bounds swap do
    i @ ?dup                            \ not null
    if  swap 1+  then			\ push under count & bump count
  cell negate +loop
;

: SET-ORDER     \ widn...wid1 n -- ; unless n = -1
( *G Set the new search-order. N is the number of WIDs   )
( ** to place in the search-order. If N is -1 then the minimum search   )
( ** order is inserted.                                                 )
  context vsize erase                   \ wipe context area (empty order)
  dup 0< if                             \ -ve n for minimum order
    drop                                \ discard n
    root                                \ make ROOT the context
    context @
    [ context vsize + cell - ] literal ! \ and the last voc
  else
    ?dup if                             \ nothing to do if n=0
      cells vsize min                   \ ensure its not too big !!!
      context swap bounds
      do  i !  cell +loop               \ set wid into context
    then
  then
;

: ONLY          \ --
( *G Set the minimum search order as the current search-order.          )
  -1 set-order
;

: ALSO          \ --
( *G Duplicate the first WID in the search order.                       )
  get-order ?dup if
    over swap 1+ set-order
  then
;

: PREVIOUS      \ --
( *G Drop the current top of search-order.                              )
  get-order ?dup if
    nip 1- set-order
  then
;

: DEFINITIONS   \ --
( *G Set the current top WID of search-order as the current definitions )
( ** wordlist.                                                          )
  context @ current !
;

previous definitions


\ ---------------------
\ *S Control structures
\ ---------------------

internal
: ?PAIRS        \ x1 x2 --
\ *G If x1<>x2, issue and error.
\ ** Used for on-target compile-time error checking.
  <> #-22 ?throw
;

: !CSP          \ x --
\ *G Save the stack pointer in *\fo{CSP}.
\ ** Used for on-target compile-time error checking.
  sp@ csp !
;

: ?CSP          \ --
\ *G Issue an error if the stack pointer is not the
\ ** same as the value previously stored in CSP.
\ ** Used for on-target compile-time error checking.
  sp@ csp @ <> #-22 ?throw
;

: ?COMP         \ --
\ *G Error if not in compile state.
  state @ 0= #-14 ?throw
;

: ?EXEC         \ --
\ *G Error if not interpreting.
  state @ #-403 ?throw
;
external

: DO            \ C: -- do-sys ; Run: n1|u1 n2|u2 -- ; R: -- loop-sys
\ *G Begin a *\fo{DO ... LOOP} construct. Takes the end-value and
\ ** start-value from the data-stack.
  ?comp  c_do  3
;  IMMEDIATE

: ?DO           \ C: -- do-sys ; Run: n1|u1 n2|u2 -- ; R: -- | loop-sys
\ *G Compile a *\fo{DO} which will only begin loop execution if the
\ ** loop parameters are not the same. Thus *\fo{0 0 ?DO ... LOOP}
\ ** will not execute the contents of the loop.
  ?comp  c_?do  3
;  IMMEDIATE

: LOOP          \ C: do-sys -- ; Run: -- ; R: loop-sys1 -- | loop-sys2
\ *G The closing statement of a *\fo{DO..LOOP} construct.
\ ** Increments the index and terminates when the index crosses
\ ** the limit.
  ?comp  3 ?pairs  c_loop
;  IMMEDIATE

: +LOOP         \ C: do-sys -- ; Run: n -- ; R: loop-sys1 -- | loop-sys2
\ *G As with *\fo{LOOP} except that you specify the increment on
\ ** the data-stack.
  ?comp  3 ?pairs  c_+loop
;  IMMEDIATE

: BEGIN         \ C: -- dest ; Run: --
\ *G Mark the start of a structure of the form:
\ *C   BEGIN..[while]..UNTIL / AGAIN / [REPEAT]
  ?comp  c_mrk_branch<
;  IMMEDIATE

: AGAIN         \ C: dest -- ; Run: --
\ *G The end of a *\fo{BEGIN..AGAIN} construct which specifies
\ ** an infinite loop.                                                              )
  ?comp  c_branch<
;  IMMEDIATE

: UNTIL         \ C: dest -- ; Run: x --
\ *G Compile code into definition which will jump back to the
\ ** matching *\fo{BEGIN} if the supplied condition flag is
\ ** Zero/FALSE.
  ?comp  c_?branch<
;  IMMEDIATE

: WHILE         \ C: dest -- orig dest ; Run: x --
\ *G Separate the condition test from the loop code in a
\ ** *\fo{BEGIN..WHILE..REPEAT} block.
  ?comp  c_?branch> swap
;  IMMEDIATE

: REPEAT        \ C: orig dest -- ; Run: --
\ *G Loop back to the conditional dest code in a
\ ** *\fo{BEGIN..WHILE..REPEAT} construct.                                                         )
  ?comp  c_branch<   >c_res_branch	\ SFP003
;  IMMEDIATE

: IF            \ C: -- orig ; Run: x --
\ *G Mark the start of an *\fo{IF..[ELSE]..THEN} conditional
\ ** block.
  ?comp  c_?branch>
;  IMMEDIATE

: THEN          \ C: orig -- ; Run: --
\ *G Mark the end of an *\fo{IF..THEN} or *\fo{IF..ELSE..THEN}
\ ** conditional construct.
  ?comp  >c_res_branch
;  IMMEDIATE

: endif         \ C: orig -- ; Ru: -- ; synonym for THEN
\ *G An alias for *\fo{THEN}. Note that ANS Forth describes
\ ** *\fo{THEN} not *\fo{ENDIF}.
  [compile] then
;  immediate

: AHEAD         \ C: -- orig ; Run: --
\ *G Start an unconditional forward branch which will be resolved
\ ** later.
  ?comp  c_branch>
;  IMMEDIATE

: ELSE          \ C: orig1 -- orig2 ; Run: --
\ *G Begin the failure condition code for an *\fo{IF}.
  ?comp  c_branch>  swap >c_res_branch
;  IMMEDIATE

BigKernel? [if]
: CASE          \ C: -- case-sys ; Run: --
\ *G Begin a *\fo{CASE..ENDCASE} construct. Similar to C's
\ ** *\b{switch}.
  ?comp  csp @  c_case  !csp  4
;  IMMEDIATE

: OF            \ C: -- of-sys ; Run: x1 x2 -- | x1
\ *G Begin conditional block for *\fo{CASE}, executed when the
\ ** switch value is equal to the X2 value placed in TOS.
  4 ?pairs  c_of  5
;  IMMEDIATE

: ENDOF         \ C: case-sys1 of-sys -- case-sys2 ; Run: --
\ *G Mark the end of an OF conditional block within a *\fo{CASE}
\ ** construct. Compile a jump past the *\fo{ENDCASE} marker at
\ ** the end of the construct.
  5 ?pairs  c_endof  4
;  IMMEDIATE

: ENDCASE       \ C: case-sys -- ; Run: x --
\ *G Terminate a *\fo{CASE..ENDCASE} construct. *\fo{DROP}s the
\ ** switch value from the stack.
  4 ?pairs  c_endcase  csp !
;  IMMEDIATE

FullCase? [if]
: ?OF           \ C: -- of-sys ; Run: flag --
\ *G Begin conditional block for *\fo{CASE}, executed when the flag
\ ** is true.
  4 ?pairs  c_?of  5
;  IMMEDIATE

: END-CASE      \ C: case-sys -- ; Run: --
\ *G A Version of *\fo{ENDCASE} which does not drop the switch
\ ** value. Used when the switch value itself is consumed by a
\ ** DEFAULT condition.
  4 ?pairs  c_end-case  csp !
;  IMMEDIATE

: NEXTCASE      \ C: case-sys -- ; Run: x --
\ *G Terminate a *\fo{CASE..NEXTCASE} construct. *\fo{DROP}s the
\ ** switch value from the stack and compiles a branch back to the
\ ** top of the loop at *\fo{CASE}.
  4 ?pairs   c_nextcase  csp !
;  IMMEDIATE
[endif]		\ FullCase?
[then]		\ BigKernel?

: RECURSE       \ Comp: --
\ *G Compile a recursive call to the colon definition containing
\ ** *\fo{RECURSE} itself. Do not use *\fo{RECURSE} between
\ ** *\fo{DOES>} and *\fo{;}. Used in the form:
\ *C : foo  ... recurse ... ;
\ *P to compile a reference to *\fo{FOO} from inside *\fo{FOO}.
  latest name> compile,
;  IMMEDIATE


\ **********************************
\ *S Target interpreter and compiler
\ **********************************

internal
: ?STACK        \ --
\ *G Error if stack pointer out of range.
  sp@ s0 @ u> #-4 ?throw
;

: ?UNDEF        \ x --
\ *G Word not defined error if x=0.
  0= #-13 ?throw
;

external

: POSTPONE      \ Comp: "<spaces>name" --
\ *G Compile a reference to another word. *\fo{POSTPONE} can handle
\ ** compilation of *\fo{IMMEDIATE} words which would otherwise be
\ ** executed during compilation.
  bl word find dup ?undef		\ it is a name
  -1 = if				\ if non-immediate
    [compile] literal  ['] compile,	\ lay "lit xt  compile,"
  then
  compile,				\ immediate word will be compiled now
; IMMEDIATE

: S"            \ Comp: "ccc<quote>" -- ; Run: -- c-addr u
\ *G Describe a string. Text is taken upto the next double-quote
\ ** character. The address and length of the string are returned.
  state @ if
    compile (s") ",
  else
   [char] " parse  pad place  pad count
  then
; IMMEDIATE

: C"            \ Comp: "ccc<quote>" -- ; Run: -- c-addr
\ *G As *\fo{S"} except the address of a counted string is returned.
  state @ if
    compile (c") ",
  else
    [char] " parse  pad place  pad
  then
;
IMMEDIATE

internal
: #LITERAL      \ n1 .... nn n -- ; put in dictionary n1 first
\ *G Compile n1..nn as literals so that the same stack order
\ ** results when the code executes.
\ ** This word will be removed in a future release. INTERNAL.
  ?dup if
    0 swap 1-				\ n1..nn 0 n-1 --
    do  i roll c_lit  -1 +loop
  then
;
external

: LITERAL       \ Comp: x -- ; Run: -- x
\ *G Compile a literal into the current definition. Usually used
\ ** in the form *\fo{[ <expression ] LITERAL} inside a colon
\ ** definition. Note that *\fo{LITERAL} is *\fo{IMMEDIATE}.
  1 #literal
;
IMMEDIATE

: 2LITERAL      \ Comp: x1 x2 -- ; Run: -- x1 x2
\ *G A two cell version of *\fo{LITERAL}.
  2 #literal
;
IMMEDIATE

: CHAR          \ "<spaces>name" -- char
( *G Return the first character of the next token in the input stream.	)
( ** Usually used to avoid magic numbers in the source code.		)
  bl word 1+ c@
;

: [CHAR]        \ Comp: "<spaces>name" -- ; Run: -- char
( *G Compile the first character of the next token in the input stream	)
( ** as a literal. Usually used to avoid magic numbers in the source 	)
( ** code.								)
  ?comp  char [compile] literal
;
IMMEDIATE

: sliteral      \ c-addr u -- ; Run: -- c-addr2 u ; 17.6.1.2212
( *G Compile the string c-addr1/u into the dictionary so that at run time )
( ** the identical string c-addr2/u is returned. Note that because of	)
( ** the use of dynamic strings at compile time the address c-addr2 is	)
( ** unlikely to be the same as c-addr1.				)
  compile (s")				\ ANS permits the buffer
  here  over 1+ allot align  place	\ to be in transient space
; immediate

: [             \ --
\ *G Switch compiler into interpreter state.
  state off
;
IMMEDIATE

: ]             \ --
\ *G Switch compiler into compilation state.
  state on
;

: IMMEDIATE     \ --
\ *G Mark the last defined word as *\fo{IMMEDIATE}. Immediate
\ ** words will execute whenever encountered regardless of
\ ** *\fo{STATE}.
  $40 latest set-bit
;

: '             \ "<spaces>name" -- xt
( *G Find the xt of the next word in the input stream. An error occurs	)
( ** if the xt cannot be found.						)
  bl word find ?undef
;

: [']           \ Comp: "<spaces>name" -- ; Run: -- xt
( *G Find the xt of the next word in the input stream, and compile it	)
( ** as a literal. An error occurs if the xt cannot be found.		)
  ' [compile] literal
;
IMMEDIATE

: [COMPILE]     \ "<spaces>name" --
\ *G Compile the next word in the input stream. *\fo{[COMPILE]}
\ ** ignores the *\fo{IMMEDIATE} state of the word. Its operation
\ ** is mostly superceded by *\fo{POSTPONE}.
  ' compile,
;
IMMEDIATE

: (             \ "ccc<paren>" --
\ *G Begin an inline comment. All text upto the closing bracket is
\ ** ignored.
  [char] ) word drop
;
IMMEDIATE

blocks? [if]
: \             \ "ccc<eol>" --
  source-id 0=  blk @ 0<>  and if       \ source is from a block?
    >in @ c/l / 1+ c/l * >in !          \   use fixed length lines
  else
    0 word drop                         \   use variable length line
  then
;
IMMEDIATE
[else]
: \             \ "ccc<eol>" --
\ *G Begin a single-line comment. All text up to the end of the line is
\ ** ignored.
  0 word drop				\   use variable length line
;
IMMEDIATE
[then]

: ",            \ "ccc<quote>" --
\ *G Parse text up to the closing quote and compile into the
\ ** dictionary at *\fo{HERE} as a counted string. The end of the
\ ** string is aligned.
  [char] " word c@ 1+ allot  align
;

: .(            \ "cc<paren>" --
\ *G A documenting comment. Behaves in the same manner as *\fo{(}
\ ** except that the enclosed text is written to the console at
\ ** compile time.
  [char] ) word $.
;
IMMEDIATE

: ASSIGN        \ "<spaces>name" --
\ *G A state smart word to get the XT of a word. The source word
\ ** is parsed from the input stream. Used as part of an
\ ** *\fo{ASSIGN xxx TO-DO yyy} construct.
  state @ if
    [compile] [']
  else
    '
  then
;
IMMEDIATE

[undefined] (TO-DO) [if]
internal
: (TO-DO)       \ -- ; R: xt -- a-addr'
\ *G The run-time action of *\fo{TO-DO}. It is followed by the
\ ** data address of the *\fo{DEFER}red word at which the xt is
\ ** stored.
  r> dup cell+ aligned >r
  @c !
;
external
[then]

: TO-DO         \ "<spaces>name" --
\ *G The second part of the *\fo{ASSIGN xxx TO-DO yyy} construct.
\ ** This word will assign the given XT to be the action of a
\ ** *\fo{DEFER}ed word which is named in the input stream.
  ' >body  state @ if
    compile (to-do) ,
  else
    !
  then
;
IMMEDIATE

: exit		\ R: nest-sys -- ; exit current definition
( *G Compile code into the current definition to cause a definition to  )
( ** terminate. This is the Forth equivalent to inserting an RTS/RET    )
( ** instruction in the middle of an assembler subroutine.              )
  ?comp
[ target-locals? ] [if]
  drop-locals                           \ compiles in stack cleanup
[then]
  c_exit
; immediate

: ;             \ C: colon-sys -- ; Run: -- ; R: nest-sys --
\ *G Complete the definition of a new 'colon' word or *\fo{:NONAME}
\ ** code block.
  ?comp ?csp
[ target-locals? ] [if]
  drop-locals                           \ compiles in stack cleanup
  cleanup-locals                        \ cleans up local dictionary etc.
[then]
  c_exit
[defined] FlushCache [if]
  FlushCache
[then]
  smudge [compile] [
;
IMMEDIATE

-short-branches
: INTERPRET     \ --
\ *G Process the current input line as if it is text entered at
\ ** the keyboard.
[defined] FlushCache [if]
  FlushCache
[then]
  begin
    ?stack  bl word dup c@
  while
    find ?dup if
      0< state @ and
      if  compile,  else  execute  then
    else
      number? ?dup ?undef
      state @
      if  #literal  else  drop  then
    then
  repeat
  drop
;
+short-branches

BigKernel? 0= [if] internal [then]
[undefined] n>r [if]
: N>R           \ xn .. x1 N -- ; R: -- x1 .. xn n
\ *G Transfer N items and count to the return stack.
  dup                   \ xn .. x1 N N --
  begin
    dup
  while
    rot r> swap >r >r   \ xn .. N N -- ; R: .. x1 --
    1-                  \ xn .. N 'N -- ; R: .. x1 --
  repeat
  drop                  \ N -- ; R: x1 .. xn --
  r> swap >r >r
;
[then]

[undefined] nr> [if]
: NR>           \ -- xn .. x1 N ; R: x1 .. xn N --
\ *G Pull N items and count off the return stack.
  r> r> swap >r dup
  begin
    dup
  while
    r> r> swap >r -rot
    1-
  repeat
  drop
;
[then]
BigKernel? 0= [if] external [then]

: EVALUATE      \ i*x c-addr u -- j*x ; interpret the string
( *G Process the supplied string as though it had been entered via the  )
( ** interpreter.                                                       )
  save-input n>r
  to-source  -1 to source-id  >in off
[ blocks? ] [if]  blk off  [then]
  ['] interpret catch
  nr> restore-input drop
  throw
;

internal
: .throw	\ throw# --
\ *G Display the throw code. Values of 0 and -1 are ignored.
  case
     0 of  endof				\ quiet
    -1 of  endof				\ quiet
    -2 of  'AbortText @ countc typec  endof	\ ABORT" ..."
    -13 of  ." is undefined"  endof
       ." Throw code " .
  end-case
;
external

: QUIT		\ -- ; R: i*x --
\ *G Empty the return stack, store 0 in *\fo{SOURCE-ID}, and enter
\ ** interpretation state. *\fo{QUIT} repeatedly *\fo{ACCEPT}s a
\ ** line of input and *\fo{INTERPRET}s it, with a prompt if
\ ** interpreting and *\fo{ECHOING} is on. Note that any task that
\ ** uses *\fo{QUIT} must initialise *\fo{'TIB}, *\fo{BASE},
\ ** *\fo{IPVEC}, and *\fo{OPVEC}.
[ blocks? ] [if]
  blk off
[then]
  xon/xoff off  echoing on              \ No Xon/Xoff, do Echo
  0 to source-id  [compile] [		\ set up
  begin
    r0 @ rp!                            \   reset return stack
    echoing @ if  cr  then              \   if echoing enabled issue new line
    query				\   get user input
    ['] interpret catch ?dup 0= if	\   interpret line
      state @ 0=  echoing @ 0<>  and if	\   if interpreting & echoing
        ."  ok"  depth ?dup		\     prompt user
        if  ." -" .  then
      then
    else
      .throw  s0 @ sp!  [compile] [	\    display error, clean up
      cr source type			\    display input line
      cr >in @ 1- spaces ." ^"		\    display pointer to error
    then
  again                                 \   do next line
;


\ =========================
\ *N Compilation and Caches
\ =========================
\ *P Because some CPUs, e.g. XScale and ARM9s, have separate
\ ** instruction and data caches, self-modifying code can cause
\ ** problems when code is laid down (into the data cache) and
\ ** then an attempt is made to execute it (the instruction cache
\ ** will not necessarily contain the code). For this reason a
\ ** word is provided that will synchronise the caches. This
\ ** word is CPU specific and may reference code in a CPU and/or
\ ** hardware specific file.

\ *P Synchronisation will usually only be necessary when creating
\ ** words, constants, variables etc. interactively on the target
\ ** and then executing them before the code has reached the
\ ** instruction cache. Only executable code has to be
\ ** synchronised, not data.

\ *P If the word *\fo{FLUSHCACHE ( -- )} is provided before
\ ** *\i{kernel62.fth} is compiled, it will be executed by the text
\ ** interpreter before each line is processed. *\fo{FLUSHCACHE}
\ ** is also executed by *\fo{;}.


\ ***************
\ *S Startup code
\ ***************

\ *************
\ *N Cold chain
\ *************
\ *P If enabled by the non-zero equate *\fo{COLDCHAIN?} the cold
\ ** start code in *\fo{COLD} will walk a list and execute the xts
\ ** contained in it. The xts must have no stack effect *\fo{( -- )}
\ ** and are added to the list by the phrase:
\ *C   ' <wordname> AtCold
\ *P The list is executed in the order in which it was defined so
\ ** that the last word added is executed last. This was done
\ ** for compatibility with VFX Forth, which also contains a
\ ** shutdown chain, in which the last word added is executed
\ ** first.

\ *P If the equate *\fo{COLDCHAIN?} is not defined in the control
\ ** file, a default value of 0 will be defined.

[undefined] ColdChain? [if]
0 equ ColdChain?
[then]

ColdChain? [if]

align
l: ColdChainFirst	\ -- addr
\ *G Dummy first entry in ColdChain.
  0 ,  ' noop ,

variable ColdChain	\ -- addr
\ *G Holds the address of the last entry in the cold chain.
  ColdChainFirst ColdChain !

interpreter
: AtCold        \ xt(t) --
  align
  chere(t) dup ColdChain @c(t) !c(t)	\ last points to new
  ColdChain !c(t)			\ new becomes old
  0 ,c(t)  ,c(t)			\ lay link to next and xt
;
target

: AtCold        \ xt --
\ *G Specifiy a new XT to execute when *\fo{COLD} is run. Note
\ ** that the last word added is executed last. *\fo{ATCOLD} can
\ ** be executed interpretively during cross-compilation. The cold
\ ** chain is built in the current *\fo{CDATA} section.
  Align
  here dup  ColdChain @ !  ColdChain !	\ update cold chain
  0 ,  ,				\ lay link to next and xt
;

: WalkColdChain         \ --                                     MPE.0000
\ *G Execute all words added to the cold chain. Note that the first
\ ** word added is executed first.
  ColdChainFirst
  begin
    dup
   while
    dup cell + @ execute		\ execute XT
    @					\ get next entry
  repeat
  drop
;

[then]

\ ====================
\ *N The COLD sequence
\ ====================
\ *P At power up, the target executes *\fo{COLD} or the word specified
\ ** by *\fo{MAKE-TURNKEY <name>}.

internal

3 cells equ #idhead

: copyRAMtab	\ addr --
\ *G Copy the given RAM table from Flash into RAM. The table may
\ ** initialise multiple blocks of RAM.
  begin                                 \ len, addr, pageid, len data, ..., 0,
    dup @c
   while                                \ len<>0
    dup #idhead +  over 2@c             \ source, dest, len
    cmovec                              \ copy
    dup @c +  #idhead +                 \ step to next block
  repeat
  drop
;

: (INIT)        \ --
\ *G Performs the high level Forth startup. See the source code for
\ ** more details.
  init-ram copyRAMtab			\ copy IDATA sections to their destinations
  user-reset s0 4 cells cmovec          \ initialise user vars
  s0 @ sp!                              \ Reset data stack
  init-tib 'tib !			\ input buffer pointer
  handler off				\ no CATCH used yet
   	 fence 3 cells cmovec       \ fence, dp, voc-link
[ harvard? ] [if]
  init-rp @c rp !
[then]
[ blocks? ] [if]
  blk off
[then]
  0 to source-id  decimal
  only forth definitions                \ set up search order
;
external

: COLD          \ --
\ *G The first high level word executed by default. This word is
\ ** set to be the word executed at power up, but this may be
\ ** overridden by a later use of *\fo{MAKE-TURNKEY <name>}. See
\ ** the source code for more details of *\fo{COLD}.
  (init)                                \ start Forth
  init-ser                              \ initialise serial line
  console opvec !			\ default i/o channels
  console ipvec !
[ SIZEOFHEAP ] [if]
  init-heap				\ initialise memory heap manager
[then]
[ paged? ] [if]
  init-pages                            \ initialise paging
[then]
[ tasking? ] [if]
  init-multi				\ initialise multi-tasker
[then]
[ romforth? ] [if]
  relink				\ initialise ROM PowerForth
[then]
[defined] WalkColdChain [if]
  WalkColdChain				\ execute user specified initialisation
[then]

  CR .cpu				\ sign on
  cr .free                              \ display free space
  cr cr ."   ok"			\ display prompt
  s0 @ sp!                              \ reset data stack
\ [ 32bit? ] [if]  FlushKeys  [then]	\ flush UART input
  quit                                  \ start text interpreter
;
make-turnkey cold                       \ Default start-up word.


\ =====================
\ *S Kernel error codes
\ =====================

\ *D -1 ABORT
\ *D -2 ABORT"
\ *D -4 Stack underflow
\ *D -13  Undefined word.
\ *D -14  Attempt to interpret a compile only definition.
\ *D -22  Control structure mismatch - unbalanced control structure.
\ *D -121 Attempt to remove with MARKER or FORGET below FENCE in protected dictionary.
\ *D -403 Attempt to compile an interpret only definition.
\ *D -501 Error if not LOADing from a block.


\ ===============================================
\ *S Differences between the v6.1 and 6.2 kernels
\ ===============================================

\ =================
\ *N Error handling
\ =================
\ *P All error handling in the v6.2 kernel is defined in terms of
\ ** *\fo{CATCH} and *\fo{THROW}. The earlier words *\fo{ERROR} and
\ ** *\fo{?ERROR} have been removed. If you need them, define them
\ ** as synonyms for *\fo{THROW} and *\fo{?THROW}.

\ *P The definition of *\fo{ABORT} has changed significantly. The
\ ** old version was:
\ *E : ABORT         \ i*x -- ; R: j*x --
\ ** \ *G Empty the data stack and perform the action of QUIT, which includes
\ ** \ ** emptying the return stack, without displaying a message.
\ **   xon/xoff off  echoing on              \ No Xon/Xoff, do Echo
\ **   s0 @ sp!                              \ reset data stack
\ **   quit                                  \ start text interpreter
\ ** ;
\ *P The new version is:
\ *E : ABORT         \ i*x -- ; R: j*x --
\ ** \ *G Performs "-1 THROW". This is a compatibility word for earlier
\ ** \ ** versions of the kernel. Unfortunately, the earlier versions
\ ** \ ** gave problems when ABORT was used in interrupt service routines
\ ** \ ** or tasks. The new definition is brutal but consistent.
\ **   -1 Throw
\ ** ;
\ *P The old version worked 99% of the time, except that in tasks or
\ ** interrupt service routines, the result was unpredictable. Because
\ ** modern applications are larger and more complex, *\fo{ABORT}
\ ** has to be completely predictable. The line
\ *C   xon/xoff off  echoing on              \ No Xon/Xoff, do Echo
\ *P is now part of *\fo{QUIT}. The phrase *\fo{S0 @ SP!} must now
\ ** be provided by the *\fo{THROW} handler.

\ *P The previous definition of *\fo{THROW} checked for a
\ ** previously defined *\fo{CATCH} and performed the old
\ ** *\fo{ABORT} if no *\fo{CATCH} had been defined. The new
\ ** version assumes that a *\fo{CATCH} has been defined and
\ ** may/will crash if no *\fo{CATCH} has been performed. The
\ ** result is a faster and smaller definition of *\fo{CATCH}.
\ ** However, it is now the programmer's responsibility to provide
\ ** a *\fo{CATCH} handler for ALL ISRs and tasks that may generate
\ ** a *\fo{THROW}. This is actually very little different from
\ ** the previous situation, except that the system is less
\ ** forgiving if you forget to provide a handler.

\ *P Error codes have been made ANS compliant. It is MPE policy that
\ ** all error and ior (i/o result) codes shall be distinct from now
\ ** on.

\ ===================================
\ *N Terminal input buffer and ACCEPT
\ ===================================
\ *P The changes below simplifiy the source code, and permit multiple
\ ** tasks to use *\fo{EVALUATE} without interaction. Note that
\ ** compilation from multiple sources/tasks requires the
\ ** interpreter/compiler to be interlocked with a semaphore.

\ *P The *\fo{2VARIABLE SOURCE-STRING} has been removed, and
\ ** *\fo{TO-SOURCE} and *\fo{SOURCE} use *\fo{'TIB} and *\fo{#TIB}
\ ** instead.

\ *P The state variables *\fo{ECHOING} and *\fo{XON/XOFF} are now
\ ** *\fo{USER} variables. In most cases this will have no impact.
\ ** However, tasks may now control these variables independently.

\ *P *\fo{QUIT} always enforces *\fo{ECHOING} on and disables
\ ** XON/XOFF processing. *\fo{QUIT} does not select an I/O device.
\ ** This change was made to allow the interpreter to be used on
\ ** any channel in systems with several serial lines or with the
\ ** Telnet service of the PowerNet TCP/IP stack. Note that any
\ ** task that uses *\fo{QUIT} must initialise *\fo{IPVEC},
\ ** *\fo{OPVEC}, *\fo{ECHOING} and *\fo{XON/XOFF}.

\ *P Removed: *\fo{?EMIT} and *\fo{SOURCE-STRING}.


\ ******
\ *> ###
\ ******

