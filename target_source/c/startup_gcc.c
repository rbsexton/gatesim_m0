//*****************************************************************************

#include <string.h>
#include <stdint.h>
#include <stdbool.h>

//*****************************************************************************
//
// Forward declaration of the default fault handlers.
//
//*****************************************************************************
extern void ResetISR(void);

//*****************************************************************************
//
// External declarations for the interrupt handlers used by the application.
//
//*****************************************************************************

//*****************************************************************************
//
// The entry point for the application.
//
//*****************************************************************************
extern int main(void);
void IntDefaultHandler(void);

extern void *_stack_top;

//*****************************************************************************
//
// The vector table.  Note that the proper constructs must be placed on this to
// ensure that it ends up at physical address 0x0000.0000.
//
//*****************************************************************************
const void *stack_top __attribute__ ((section(".boot_stack_pointer"))) = &_stack_top ; // The initial stack pointer

void (* const g_pfnVectors[])(void) __attribute__ ((section(".isr_vector"))) =
{
    /* 01 */ ResetISR,                               // The reset handler
    /* 02 */ IntDefaultHandler,                                  // The NMI handler
    /* 03 */ IntDefaultHandler,                       // The hard fault handler
    /* 04 */ IntDefaultHandler,                       // The MPU fault handler
    /* 05 */ IntDefaultHandler,                       // The bus fault handler
    /* 06 */ IntDefaultHandler,                       // The usage fault handler
    /* 07 */ (void (*const)(void)) 0,             // Reserved - Use for firmware flavor

    /*    */ // The next ones get filled in by the 'brand' program.
    /* 08 */ (void (*const)(void)) 0, // Reserved - Use for programming image size
    /* 09 */ (void (*const)(void)) 0, // Reserved - How much to skip before CRC'ing.
    /* 10 */ (void (*const)(void)) 0, // Reserved - Use for CRC
    /* 11 */ IntDefaultHandler,
    /* 12 */ IntDefaultHandler,                      // Debug monitor handler
    /* 13 */ 0,                                      // Reserved
    /* 14 */ IntDefaultHandler,                          // The PendSV handler
	/* 15 */ IntDefaultHandler,							// The SysTick handler


	/* 16 */ IntDefaultHandler,						// 16 GPIO Port A
	/* 17 */ IntDefaultHandler,						// GPIO Port B
    /* 18 */ IntDefaultHandler,                      // GPIO Port C
    /* 19 */ IntDefaultHandler,                      // GPIO Port D
    /* 20 */ IntDefaultHandler,                      // GPIO Port E
    /* 21 */ IntDefaultHandler,                      // UART0 Rx and Tx
    /* 22 */ IntDefaultHandler,                     // UART1 Rx and Tx
    /* 23 */ IntDefaultHandler,                      // SSI0 Rx and Tx

    /*    */ // The NVIC Supports 240 Interrupt Vectors. Not all of them work, sadly.
    /*    */ // Steal the PWM Interrupts.
    /* 24 */ IntDefaultHandler,                  // I2C0 Master and Slave
    /* 25 */ IntDefaultHandler,             		  		// SWI to request Erase or Read
    /* 26 */ IntDefaultHandler,                    // Callback into the lwIP layer for user feedback.
    /* 27 */ IntDefaultHandler,         // SWI to indicate pending write work.
    /* 28 */ IntDefaultHandler,                 // SWI to indicate work completion
    /* 29 */ IntDefaultHandler,                      // Quadrature Encoder 0
    /* 30 */ IntDefaultHandler,      		        // ADC Sequence 0
    /* 31 */ IntDefaultHandler,                      // ADC Sequence 1

    /* 32 */ IntDefaultHandler,                      // 32 ADC Sequence 2
    /* 33 */ IntDefaultHandler,                      // ADC Sequence 3
    /* 34 */ IntDefaultHandler,                       // Watchdog timer
    /* 35 */ IntDefaultHandler,                    // Timer 0 subtimer A
    /* 36 */ IntDefaultHandler,                      // Timer 0 subtimer B
    /* 37 */ IntDefaultHandler,                      // Timer 1 subtimer A
    /* 38 */ IntDefaultHandler,                      // Timer 1 subtimer B
    /* 39 */ IntDefaultHandler,                          // Timer 2 subtimer A

    /* 40 */ IntDefaultHandler,                      // Timer 2 subtimer B
    /* 41 */IntDefaultHandler,                      // Analog Comparator 0
    /* 42 */IntDefaultHandler,                      // Analog Comparator 1
    /* 43 */ IntDefaultHandler,                      // Analog Comparator 2
    /* 44 */IntDefaultHandler,                      // System Control (PLL, OSC, BO)
    /* 45 */IntDefaultHandler,                      // FLASH Control
    /* 46 */IntDefaultHandler,                      // GPIO Port F
    /* 47 */IntDefaultHandler,                      // GPIO Port G
};

//*****************************************************************************
//
// The following are constructs created by the linker, indicating where the
// the "data" and "bss" segments reside in memory.  The initializers for the
// for the "data" segment resides immediately following the "text" segment.
//
//*****************************************************************************
extern unsigned long _etext;
extern unsigned long _data;
extern unsigned long _edata;
extern unsigned long _bss;
extern unsigned long _ebss;


//*****************************************************************************
//
// This is the code that gets called when the processor first starts execution
// following a reset event.  Only the absolutely necessary set is performed,
// after which the application supplied entry() routine is called.  Any fancy
// actions (such as making decisions based on the reset cause register, and
// resetting the bits in that register) are left solely in the hands of the
// application.
//
//*****************************************************************************
void
ResetISR(void)
{
    unsigned long *pulSrc, *pulDest;

    //
    // Copy the data segment initializers from flash to SRAM.
    //
    pulSrc = &_etext;
    for(pulDest = &_data; pulDest < &_edata; )
    {
        *pulDest++ = *pulSrc++;
    }

    //
    // Zero fill the bss segment, the sharedbss segment, and the topmem
    //
    //__asm(
    //	  "    ldr     r0, =_bss\n"
     //     "    ldr     r1, =_ebss\n"
      //    "    mov     r2, #0\n"
       //   "    .thumb_func\n"
     //     "zero_loop:\n"
      //    "        cmp     r0, r1\n"
      //    "        it      lt\n"
       //   "        strlt   r2, [r0], #4\n"
      //    "        blt     zero_loop\n"
	  
//	  );

//
    // Call the application's entry point.
    //
    main();
}

//*****************************************************************************
//
// This is the code that gets called when the processor receives an unexpected
// interrupt.  This simply enters an infinite loop, preserving the system state
// for examination by a debugger.
//
//*****************************************************************************
void
IntDefaultHandler(void)
{
    //
    // Go into an infinite loop.
    //
    while(1)
    {
    }
}
