#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <libusb-1.0/libusb.h>


#define USE_LED 0

// BUILD: gcc sw.c -o sw -lusb-1.0

void show_error(const char* m) {
  fprintf( stderr, "sw error: %s\n", m );
}

void show_errorno( int x ) {
  if ( x ) {
    show_error( libusb_error_name( x ));
  }
}

void halt_error( int x ) {
  if ( x ) {
    show_errorno( x );
    exit( -1 );
  }
}

void dump(const char* t, const unsigned char* pb) {
  int i = 0;
  printf( "%s\n", t );
  while(i++ < 64) {
    printf( "%02x ", *(pb+i));
    if (0 == (i%16)) {
      printf("\n");
    }
  }
}

void hw_callback( struct libusb_transfer *transfer) {
  printf("+++ CALLBACK! +++\n");
}

void exec_cmd(libusb_device_handle *hw, unsigned char op,
	      unsigned char b1, unsigned char b2, unsigned char b3 )
{
  unsigned char buffer[64] = {0};
  int iError ;
  int iXfrCount;

  buffer[0] = op;
  buffer[1] = b1;
  buffer[2] = b2;
  buffer[3] = b3;

  //dump( "COMMAND:", &buffer[0] );

  iError = libusb_interrupt_transfer
    ( hw,
      1 | LIBUSB_ENDPOINT_OUT,
      &buffer[0],
      sizeof(buffer),
      &iXfrCount,
      500);

  //show_errorno( iError );

  //printf( "(%d) WR: Bytes transferred: %d of %d\n"
  //	  , iError
  //	  , iXfrCount
  //	  , sizeof(buffer) );

  iError = libusb_interrupt_transfer
    ( hw,
      1 | LIBUSB_ENDPOINT_IN,
      &buffer[0],
      sizeof(buffer),
      &iXfrCount,
      500);

  //show_errorno( iError );

  //printf( "(%d) RD: Bytes transferred: %d of %d\n"
  //	  , iError
  //	  , iXfrCount
  //	  , sizeof(buffer) );

  //dump( "RESPONSE:", &buffer[0] );
}




int main( int argc, char* argv[] )
{
  libusb_context       *ctx   = NULL;
  libusb_device        *hwdev = NULL;
  libusb_device_handle *hw    = NULL;

  struct libusb_transfer transfer   = {0};
  int                    iError     = 0;
  int                    iDirFlag   = 1;
  int                    iCount     = 1;
  int                    iPeriod    = 0;
  int                    bAttached  = 0;

  if (argc != 4) {
    show_error( "usage :- sw DIR COUNT SLEEP where DIR=f|b, COUNT=a number, SLEEP uS\n");
    exit(-1);
  }


  iDirFlag = ( 'f' == *argv[ 1 ]) ? 1 : 0;
  iCount   = atoi( argv[ 2 ]);
  iPeriod  = atoi( argv[ 3 ]);


  iError = libusb_init( &ctx );
  halt_error( iError );
  //libusb_set_debug( ctx, 3 );

  hw = libusb_open_device_with_vid_pid( ctx, 0x0b40, 0x0132 );

  if ( hw ) {
    if ( libusb_kernel_driver_active( hw, 0 )) {
      printf( "Detected kernel driver attached to device, removing it!\n" );
      iError = libusb_detach_kernel_driver( hw, 0 );
      if ( iError ) {
	fprintf( stderr, "Failed to detach kernel driver, bailed out! :(\n" );
	halt_error( iError );
      }
      else {
	bAttached = 1;
      }
    }
    else {
      printf( "No kernel driver attached to this device\n");
    }
    iError = libusb_claim_interface( hw, 0 );
    if (iError) {
      show_errorno( iError );
    }
    else {
      printf( "Success, the expandIO-USB is now under our control\n");

      if (USE_LED) {
          exec_cmd( hw, 0x9f, 0x01, 0x01, iDirFlag );
      }
      else {
          exec_cmd( hw, 0x9f, 0x02, 0x06, iDirFlag );
      }

      while ( iCount-- > 0 )
	{
	  if (USE_LED) { // use LED
	    exec_cmd( hw, 0x9f, 0x01, 0x00, 0x01 ); // pulse-LOW
	    usleep( iPeriod );
	    exec_cmd( hw, 0x9f, 0x01, 0x00, 0x00 ); // pulse-LOW
	    usleep( iPeriod );
	  }
	  else { // use PORTB
	    exec_cmd( hw, 0x9f, 0x02, 0x07, 0x01 ); // pulse-LOW
	    usleep( iPeriod );
	    exec_cmd( hw, 0x9f, 0x02, 0x07, 0x00 ); // pulse-LOW
	    usleep( iPeriod );
	  }
	}
      libusb_release_interface( hw, 0 );
    }

    if ( 1 == bAttached ) {
      libusb_attach_kernel_driver( hw, 0 );
      printf( "Re-attached kernel driver\n" );
    }

    libusb_close( hw );
    printf( "expandIO-USB handle closed\n" );
  }
  else {
    fprintf( stderr, "Failed to locate the expandIO-USB device\n" );
  }

  libusb_exit( ctx );
  printf( "USB connection released\n" );
}
