\ ARM Cortex assembler macros

((
Copyright (c) 1988-2001, 2009
MicroProcessor Engineering
133 Hill Lane
Southampton SO15 5AF
England

tel: +44 23 8063 1441
fax: +44 23 8033 9691
net: mpe@mpeforth.com
     tech-support@mpeforth.com
web: www.mpeforth.com
))

only forth  also c-c  also assembler
also asm-access definitions

: NEXT,		\ --
  Thumb? if
    bx  r14
  else
    mov pc, link
  endif
;

only forth definitions

