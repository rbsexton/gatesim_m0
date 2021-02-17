//*****************************************************************************
// The minimal program for the embedded M0
//*****************************************************************************
#include <stdbool.h> 
#include <stdint.h> 

#define FIFOLOOPBACK 0
#define SWLOOPBACK   1

#define FIFO 0x40003000

volatile uint32_t counter;

volatile uint32_t txcount;
volatile uint32_t rxcount;

typedef struct {
	uint32_t fifo;
	uint32_t to_dut_status;
	uint32_t to_host_status;
	} sFIFO;

void tx(int x) {
	volatile sFIFO *p = (volatile sFIFO *) FIFO;
	p->fifo = x;
	txcount++;
	}

int32_t rx() {
	volatile sFIFO *p = (volatile sFIFO *) FIFO;
	
	if ( p->to_dut_status & 1 ) {
		rxcount++;
		return( p->fifo );
		}
	else return(-1);
}

uint8_t rx_array[16]; 


const char message[] = "Hello!";

//
//
int main() {

	// Start by sending out a banner.
	const char *p = message;
	while (*p) { 
		tx(*p);
		p++;
		}


#if FIFOLOOPBACK
	int val = 0x20;	
	tx(val++); tx(val++); tx(val++); tx(val++);
	tx(val++); tx(val++); tx(val++); tx(val++);
	tx(0x42);

	int i = 0;

	rx_array[ i & 0xf] = rx(); i++;
	rx_array[ i & 0xf] = rx(); i++;
	rx_array[ i & 0xf] = rx(); i++;
	rx_array[ i & 0xf] = rx(); i++;
	
	rx_array[ i & 0xf] = rx(); i++;
	rx_array[ i & 0xf] = rx(); i++;
	rx_array[ i & 0xf] = rx(); i++;
	rx_array[ i & 0xf] = rx(); i++;
		
	rx_array[ i & 0xf] = rx(); i++;
#endif

	// Echo any IO.
	while (1) { 
		int val = rx();
		if ( val >= 0 ) tx(val);
	}

	}

