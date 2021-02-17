(( App Startup ))


\ -------------------------------------------
\ The word that sets everything up
\ -------------------------------------------
: SEV [asm sev asm] ;
: WFE [asm wfe asm] ;

: .PARAMS here u. unused-top u. dp @ u. ; 
: StartApp
	hex 
	.params
	." StartApp " 
	\ Initialize DP, because its not happening
	init-dp @ dp !
	.params cr
;


