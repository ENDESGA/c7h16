// // // // // // //
// > c7h16 _
// -------
// explicit and minimal esoteric c abstraction
// requires: math.h, string.h, standard library
// @ENDESGA 2023
// // // // // // //

/*
//
//	functional verbs:
//	-------
//	make_H() -> construct H
//	new_H() -> out allocated H in memory
//	create_H() -> out constructed new_H()
//
*/

#pragma once
#ifndef C7H16_H
	#define C7H16_H

	#define OS_LINUX 0
	#define OS_WINDOWS 0
	#define OS_MACOS 0
	#define OS_OTHER 0

	#if defined( __WIN32__ ) || defined( WIN32 ) || defined( _WIN32 ) || defined( __CYGWIN__ ) || defined( __MINGW32__ ) || defined( __WINDOWS__ )
		#undef OS_WINDOWS
		#define OS_WINDOWS 1
		#define FARPROC WINDOWS_FARPROC
		#include <windows.h>
		#undef FARPROC
		#undef near
		#undef far
		#include <intrin.h>
	#elif defined( __LINUX__ ) || defined( linux ) || defined( __linux ) || defined( __linux__ )
		#undef OS_LINUX
		#define OS_LINUX 1
		#include <unistd.h>
		#include <sys/mman.h>
		#include <sys/stat.h>
		#include <sys/syscall.h>
		#include <fcntl.h>
		#include <immintrin.h>
		#include <pthread.h>
		#include <X11/Xlib.h>
		#include <X11/keysym.h>
	#elif defined( __MACOSX__ ) || defined( __APPLE__ )
		#undef OS_MACOS
		#define OS_MACOS 1
	#else
		#undef OS_OTHER
		#define OS_OTHER 1
	#endif

	#if defined( _MSC_VER )
		#define COMPILER_MSVC
	#elif defined( __GNUC__ )
		#define COMPILER_GCC
		#include <stdatomic.h>
	#else
		#define COMPILER_OTHER
	#endif

//

	#define CONCAT_LINE_SET( a, line ) __##a##_##line
	#define CONCAT_LINE( a, line ) CONCAT_LINE_SET( a, line )
	#define VAR_LINE( name ) CONCAT_LINE( name, __LINE__ )

	#define DEF_START \
		do              \
		{
	#define DEF_END \
		}             \
		spin( 0 )

//

/// constants

	#define null NULL
	#define yes ( 1 )
	#define no ( 0 )
	#define tau 6.283185307179586476925286766559
	#define tau_m2 12.566370614359172953850573533118
	#define tau_m3 18.849555921538759430775860299677
	#define tau_m5 31.415926535897932384626433832795
	#define tau_m7 43.982297150257105338477007365913
	#define tau_d2 3.1415926535897932384626433832795
	#define pi tau_d2
	#define tau_d3 2.0943951023931954923084289221863
	#define tau_d5 1.2566370614359172953850573533118
	#define tau_d7 0.897597901025655210989326680937
	#define tau_p2 39.478417604357434475337963999505
	#define tau_p3 248.05021344239856140381052053681
	#define tau_p5 9792.6299131290065044077219213899
	#define tau_p7 386597.53315542938464181843111322
	#define euler 2.7182818284590452353602874713527

	#define nano_per_micro 1000
	#define nano_per_milli 1000000
	#define nano_per_sec 1000000000
	#define nano_per_min 60000000000
	#define nano_per_hour 3600000000000
	#define micro_per_milli 1000
	#define micro_per_sec 1000000
	#define micro_per_min 60000000
	#define micro_per_hour 3600000000
	#define milli_per_sec 1000
	#define milli_per_min 60000
	#define milli_per_hour 3600000
	#define sec_per_min 60
	#define sec_per_hour 3600
	#define min_per_hour 60

//

	#define once static
	#define safe volatile
	#define global safe once

	#define to( TYPE, VAL ) ( TYPE )( VAL )

	#define ptr( _ ) _*
	#define val( _ ) *_
	#define ref( _ ) &_
	#define fn_ptr( _, OUTPUT, ... ) OUTPUT to( val( _ ), __VA_ARGS__ )
	#define struct( _ ) struct _
	#define enum( _ ) enum _
	#define union( _ ) union _

	#define make( _, ... ) ( ( _ ){ __VA_ARGS__ } )

	#define make_type( _ ) typedef _
	#define make_ptr( _ ) make_type( ptr( _ ) )
	#define make_fn_ptr( _, OUTPUT, ... ) make_type( fn_ptr( _, OUTPUT, __VA_ARGS__ ) )
	#define make_struct( _ ) struct( _ )
	#define make_enum( _ ) enum( _ )
	#define make_union( _ ) union( _ )

	#define fn once inline
	#define in const
	#define out return

//

	#undef abs
	#define abs( _ ) ( ( ( _ ) < 0 ) ? -( _ ) : ( _ ) )
	#undef min
	#define min( a, b ) ( ( ( a ) < ( b ) ) ? ( a ) : ( b ) )
	#undef max
	#define max( a, b ) ( ( ( a ) > ( b ) ) ? ( a ) : ( b ) )
	#undef sign
	#define sign( _ ) ( ( _ ) < 0 ? -1 : ( ( _ ) > 0 ? 1 : 0 ) )
	#undef avg
	#define avg( a, b ) ( ( ( a ) + ( b ) ) / 2. )

	#define size_( _ ) sizeof( _ )

//

	#define not !
	#define and &&
	#define or ||
	#define mod %

	#define ifn( _ ) if( !( _ ) )
	#define ifnull( _ ) if( ( _ ) == null )
	#define elif( _ ) else if( _ )

	#define with( _ ) switch( _ )
	#define is( _ ) case( _ ):
	#define skip break

	#define next continue

	#define spin( _ ) while( _ )
	#define loop spin( 1 )
	#define do_once              \
		once u8 VAR_LINE( o ) = 1; \
		if( ( VAR_LINE( o ) == 1 ? VAR_LINE( o )-- : 0 ) )
	#define iter( to_n, var )                 \
		register s32 VAR_LINE( to ) = ( to_n ); \
		if( VAR_LINE( to ) )                    \
			for( register s32 var = 0; var < VAR_LINE( to ); var++ )
	#define rep( to_n ) iter( to_n, VAR_LINE( r ) )

//

	#define cast( _, TYPE ) ( val( to( ptr( TYPE ), ref( _ ) ) ) )

	#define pure void
	//#define pure_ptr ptr(pure)
	#define size_pure size_( pure )
//#define size_pure_ptr size_( pure_ptr )

make_type( unsigned char ) flag;
	#define to_flag( _ ) ( !!( _ ) )
	#define size_flag ( size_( flag ) )
	#define print_flag( _ ) print( "%hhu", to_flag( _ ) )

make_type( unsigned char ) u8;
	#define to_u8( _ ) ( to( u8, _ ) )
	#define size_u8 ( size_( u8 ) )
	#define u8_min ( 0 )
	#define u8_max ( 0xFFU )
	#define print_u8( _ ) print( "%hhu", to_u8( _ ) )

make_type( unsigned short ) u16;
	#define to_u16( _ ) ( to( u16, _ ) )
	#define size_u16 ( size_( u16 ) )
	#define u16_min ( 0 )
	#define u16_max ( 0xFFFFU )
	#define print_u16( _ ) print( "%hu", to_u16( _ ) )

make_type( unsigned int ) u32;
	#define to_u32( _ ) ( to( u32, _ ) )
	#define size_u32 ( size_( u32 ) )
	#define u32_min ( 0 )
	#define u32_max ( 0xFFFFFFFFU )
	#define print_u32( _ ) print( "%u", to_u32( _ ) )

make_type( unsigned long long int ) u64;
	#define to_u64( _ ) ( to( u64, _ ) )
	#define size_u64 ( size_( u64 ) )
	#define u64_min ( 0 )
	#define u64_max ( 0xFFFFFFFFFFFFFFFFU )
	#define print_u64( _ ) print( "%llu", to_u64( _ ) )

make_type( char ) s8;
	#define to_s8( _ ) ( to( s8, _ ) )
	#define size_s8 ( size_( s8 ) )
	#define s8_min ( -128 )
	#define s8_max ( 127 )
	#define print_s8( _ ) print( "%hhd", to_s8( _ ) )

make_type( short ) s16;
	#define to_s16( _ ) ( to( s16, _ ) )
	#define size_s16 ( size_( s16 ) )
	#define s16_min ( -32768 )
	#define s16_max ( 32767 )
	#define print_s16( _ ) print( "%hd", to_s16( _ ) )

make_type( int ) s32;
	#define to_s32( _ ) ( to( s32, _ ) )
	#define size_s32 ( size_( s32 ) )
	#define s32_min ( -2147483648 )
	#define s32_max ( 2147483647 )
	#define print_s32( _ ) print( "%d", to_s32( _ ) )

make_type( long long int ) s64;
	#define to_s64( _ ) ( to( s64, _ ) )
	#define size_s64 ( size_( s64 ) )
	#define s64_min ( -9223372036854775808 )
	#define s64_max ( 9223372036854775807 )
	#define print_s64( _ ) print( "%lld", to_s64( _ ) )

make_type( float ) f32;
	#define to_f32( _ ) ( to( f32, _ ) )
	#define size_f32 ( size_( f32 ) )
	#define f32_min ( 1.175494351e-38F )
	#define f32_max ( 3.402823466e+38F )
	#define print_f32( _ ) print( "%f", to_f32( _ ) )

make_type( long double ) f64;
	#define to_f64( _ ) ( to( f64, _ ) )
	#define size_f64 ( size_( f64 ) )
	#define f64_min ( 2.2250738585072014e-308 )
	#define f64_max ( 1.7976931348623158e+308 )
	#define print_f64( _ ) print( "%lf", to_f64( _ ) )

//

/// time

fn u64 get_nano()
{
	#ifdef _WIN32
	LARGE_INTEGER frequency;
	QueryPerformanceFrequency( ref( frequency ) );
	LARGE_INTEGER counter;
	QueryPerformanceCounter( ref( counter ) );
	out( counter.QuadPart * nano_per_sec ) / frequency.QuadPart;
	#else
	struct timespec ts;
	clock_gettime( CLOCK_MONOTONIC, ref( ts ) );
	out ts.tv_sec* nano_per_sec + ts.tv_nsec;
	#endif
}

fn pure sleep_nano(u64 ns)
{
	#ifdef _WIN32
	HANDLE timer = CreateWaitableTimer(NULL, TRUE, NULL);
	
	LARGE_INTEGER li;
	li.QuadPart = -to_s64(ns / 100);
	if (!SetWaitableTimer(timer, &li, 0, NULL, NULL, FALSE)) {
		CloseHandle(timer);
		out;
	}
	
	WaitForSingleObject(timer, INFINITE);
	CloseHandle(timer);
	#else
	struct timespec ts;
	ts.tv_sec = ns / nano_per_sec;
	ts.tv_nsec = ns % nano_per_sec;
	nanosleep(&ts, NULL);
	#endif
}



//

/// maybe

make_struct( maybe )
{
	flag valid;
	ptr( pure ) value;
};
make_type( struct( maybe ) ) maybe;

fn maybe validate_maybe( ptr( pure ) in_val )
{
	out( maybe ){ .valid = ( ( in_val == null ) ? no : yes ), .value = ( in_val ) };
}

//

/// variable-arguments

	#ifndef va_start
		#ifndef __GNUC_VA_LIST
			#define __GNUC_VA_LIST
make_type( __builtin_va_list ) __gnuc_va_list;
		#endif

		#define va_start( v, l ) __builtin_va_start( v, l )
		#define va_end( v ) __builtin_va_end( v )
		#define va_arg( v, l ) __builtin_va_arg( v, l )
		#define va_copy( d, s ) __builtin_va_copy( d, s )
		#define __va_copy( d, s ) __builtin_va_copy( d, s )

make_type( __gnuc_va_list ) va_list;
	#endif

//

/// mem

	#if OS_WINDOWS
		#define allocate_mem( n, size_bytes ) VirtualAlloc( null, ( n ) * ( size_bytes ), MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE )
		#define free_mem( p ) VirtualFree( p, 0, MEM_RELEASE )
	#elif OS_LINUX
		#define allocate_mem( n, size_bytes ) mmap( null, ( n ) * ( size_bytes ), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0 )
		#define free_mem( p ) munmap( p, size_( p ) )
	#endif

	#define new_mem( type, n ) ( ( ptr( type ) )allocate_mem( n, size_( type ) ) )

fn ptr( pure ) copy_mem( ptr( pure ) dst, in ptr( pure ) src, u64 n )
{
	ptr( u64 ) dl = ( ptr( u64 ) )dst;
	ptr( u64 ) sl = ( ptr( u64 ) )src;
	spin( n >= size_u64 )
	{
		val( dl )++ = val( sl )++;
		n -= size_u64;
	}

	ptr( u8 ) d8 = ( ptr( u8 ) )dl;
	ptr( u8 ) s8 = ( ptr( u8 ) )sl;
	spin( n-- ) val( d8 )++ = val( s8 )++;

	out dst;
}

//

/// text

	#define upper_char( _ ) ( ( _ )&0xDF )
	#define lower_char( _ ) ( ( _ ) | 0x20 )
	#define upper_char_safe( _ ) ( ( ( _ ) >= 'a' and ( _ ) <= 'z' ) ? upper_char( _ ) : ( _ ) )
	#define lower_char_safe( _ ) ( ( ( _ ) >= 'A' and ( _ ) <= 'Z' ) ? upper_char( _ ) : ( _ ) )

u8 dec_to_u8[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0' };
u8 hex_to_u8[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', '0' };

make_type( ptr( s8 ) ) text;
	#define to_text( _ ) to( text, _ )
	#define size_text ( size_( text ) )

	#define new_text( SIZE ) new_mem( s8, SIZE )
	#define make_text( default_text, extra_char_mem ) copy_mem( new_text( text_length( default_text ) + ( extra_char_mem ) + 1 ), default_text, text_length( default_text ) )
	#define free_text( _ ) \
		if( _ != null ) free_mem( _ )
	#define print_text( _ ) print( "%s", text( _ ) )

fn u64 text_length( in text str )
{
	text s = str;

	spin( ( ( u64 )s mod size_u64 ) != 0 )
	{
		if( val( s ) == '\0' )
			out s - str;
		++s;
	}

	ptr( u64 ) long_ptr = ( ptr( u64 ) )s;
	u64
			magic_bits = 0x7efefeff,
			has_zero_byte = 0x80808080,
			longword = 0;

	loop
	{
		longword = val( long_ptr )++;

		if( ( ( longword + magic_bits ) ^ ~longword ) & has_zero_byte )
		{
			s = to_text( long_ptr - 1 );
			if( s[ 0 ] == 0 ) out s - str;
			elif( s[ 1 ] == 0 ) out s - str + 1;
			elif( s[ 2 ] == 0 ) out s - str + 2;
			elif( s[ 3 ] == 0 ) out s - str + 3;
			elif( s[ 4 ] == 0 ) out s - str + 4;
			elif( s[ 5 ] == 0 ) out s - str + 5;
			elif( s[ 6 ] == 0 ) out s - str + 6;
			elif( s[ 7 ] == 0 ) out s - str + 7;
		}
	}
}

fn text copy_text( in text in_dst, in text in_text )
{
	if( in_text == null ) out in_dst;
	ifn( val( in_text ) ) out in_dst;
	u32 n = text_length( in_text );
	ptr( u64 ) dl = ( ptr( u64 ) )in_dst;
	ptr( u64 ) sl = ( ptr( u64 ) )in_text;
	spin( n >= size_u64 )
	{
		val( dl )++ = val( sl )++;
		n -= size_u64;
	}

	ptr( u8 ) d8 = ( ptr( u8 ) )dl;
	ptr( u8 ) s8 = ( ptr( u8 ) )sl;
	spin( n-- ) val( d8 )++ = val( s8 )++;

	out in_dst;
}

fn text join_text( in text in_dst, in text in_text )
{
	copy_text( in_dst + text_length( in_dst ), in_text );
	out in_dst;
}

fn text end_text( in text str_base )
{
	out join_text( str_base, "\0" );
}

fn text flip_text( in text in_text, in u32 in_length )
{
	iter( in_length >> 1, i )
	{
		u32 j = in_length - i - 1;
		in_text[ i ] ^= in_text[ j ];
		in_text[ j ] ^= in_text[ i ];
		in_text[ i ] ^= in_text[ j ];
	}
	out in_text;
}

fn text int_to_text( s32 in_int )
{
	flag isNegative = no;

	// Check if the number is negative
	if( in_int < 0 )
	{
		isNegative = yes;
		in_int = -in_int;
	}

	int length = 0;

	// Calculate the length of the resulting string
	int temp = in_int;
	do
	{
		temp /= 10;
		length++;
	} while( temp != 0 );

	if( isNegative )
	{
		length++; // Increment length to accommodate the '-' sign
	}

	// Allocate memory for the string
	char* str = new_mem( char, length + 1 ); //new_text("",length);//(char*)malloc((length + 1) * sizeof(char));

	// Handle memory allocation failure
	if( str == NULL )
	{
		//perror("Memory allocation failed");
		//exit(EXIT_FAILURE);
	}

	// Convert the integer to string starting from the end
	int index = length - 1;
	do
	{
		str[ index-- ] = abs( in_int % 10 ) + '0';
		in_int /= 10;
	} while( in_int != 0 );

	// Add the negative sign if the number was negative
	if( isNegative )
	{
		str[ 0 ] = '-';
	}

	// Null-terminate the string
	str[ length ] = '\0';

	return str;
}

fn text format_text( in text in_formatted_text, va_list in_args )
{
	text temp_text = new_text( 512 );
	text text_ptr = temp_text;
	text formatted_ptr = in_formatted_text;

	spin( val( formatted_ptr ) != '\0' )
	{
		if( val( formatted_ptr ) != '%' )
		{
			val( text_ptr )++ = val( formatted_ptr );
		}
		else
		{
			formatted_ptr++;
			with( val( formatted_ptr ) )
			{
				is( 's' )
				{
					text arg_text = va_arg( in_args, text );
					copy_text( text_ptr, arg_text );
					text_ptr += text_length( arg_text );
					skip;
				}

				is( 'd' )
				{
					s32 arg_int = va_arg( in_args, s32 );
					text arg_text = int_to_text( arg_int );
					copy_text( text_ptr, arg_text );
					text_ptr += text_length( arg_text );
					skip;
				}
			}
		}
		formatted_ptr++;
	}

	out temp_text;
}

flag compare_text( text in_text1, text in_text2 )
{
	if( in_text1 == in_text2 ) out yes;
	if( val( in_text1 ) != val( in_text2 ) ) out no;

	spin(
			val( in_text1 ) and
			val( in_text2 ) and
			( val( in_text1 ) == val( in_text2 ) ) )
	{
		in_text1++;
		in_text2++;
	}
	out( ( val( in_text1 ) - val( in_text2 ) ) == 0 );
}

//

fn pure print( in text in_formatted_text, ... )
{
	va_list args;
	va_start( args, in_formatted_text );
	text temp_text = format_text( in_formatted_text, args );
	u64 temp_len = text_length( temp_text );
	va_end( args );

	if( temp_text > 0 )
	{
	#if OS_WINDOWS
		DWORD written;
		WriteConsoleA( GetStdHandle( STD_OUTPUT_HANDLE ), temp_text, temp_len, null, null );
	#elif OS_LINUX
		s32 result;
		asm volatile(
				"mov $1, %%rax\n"
				"mov $1, %%rdi\n"
				"mov %0, %%rsi\n"
				"mov %1, %%rdx\n"
				"syscall\n"
				:
				: "r"( temp_text ), "r"( temp_len )
				: "rax", "rdi", "rsi", "rdx" );
	#endif
	}

	free_text( temp_text );
}

text NL = "\n";
	#define print_ln print( NL )

	//

	// set
	#ifdef COMPILER_MSVC
		#define safe_s8_ptr_set( safe_ptr, set_value ) _InterlockedExchange8( to( safe ptr( s8 ), safe_ptr ), to( s8, set_value ) )
		#define safe_s16_ptr_set( safe_ptr, set_value ) _InterlockedExchange16( to( safe ptr( s16 ), safe_ptr ), to( s16, set_value ) )
		#define safe_s32_ptr_set( safe_ptr, set_value ) _InterlockedExchange( to( safe ptr( long ), safe_ptr ), to( long, set_value ) )
		#define safe_s64_ptr_set( safe_ptr, set_value ) _InterlockedExchange64( to( safe ptr( s64 ), safe_ptr ), to( s64, set_value ) )
	#elif defined( COMPILER_GCC )
		#define safe_s8_ptr_set( safe_ptr, set_value ) ( atomic_exchange( ( safe_ptr ), ( set_value ) ) )
		#define safe_s16_ptr_set( safe_ptr, set_value ) ( atomic_exchange( ( safe_ptr ), ( set_value ) ) )
		#define safe_s32_ptr_set( safe_ptr, set_value ) ( atomic_exchange( ( safe_ptr ), ( set_value ) ) )
		#define safe_s64_ptr_set( safe_ptr, set_value ) ( atomic_exchange( ( safe_ptr ), ( set_value ) ) )
	#endif

	// get
	#ifdef COMPILER_MSVC
		#define safe_s8_ptr_get( safe_ptr ) _InterlockedOr8( to( safe ptr( s8 ), safe_ptr ), 0 )
		#define safe_s16_ptr_get( safe_ptr ) _InterlockedOr16( to( safe ptr( s16 ), safe_ptr ), 0 )
		#define safe_s32_ptr_get( safe_ptr ) _InterlockedOr( to( safe ptr( long ), safe_ptr ), 0 )
		#define safe_s64_ptr_get( safe_ptr ) _InterlockedOr64( to( safe ptr( s64 ), safe_ptr ), 0 )
	#elif defined( COMPILER_GCC )
		#define safe_s8_ptr_get( safe_ptr ) ( atomic_load( ( safe_ptr ) ) )
		#define safe_s16_ptr_get( safe_ptr ) ( atomic_load( ( safe_ptr ) ) )
		#define safe_s32_ptr_get( safe_ptr ) ( atomic_load( ( safe_ptr ) ) )
		#define safe_s64_ptr_get( safe_ptr ) ( atomic_load( ( safe_ptr ) ) )
	#endif

//

	#define safe_s8_set( safe_var, set_value ) safe_s8_ptr_set( ref( safe_var ), set_value )
	#define safe_s16_set( safe_var, set_value ) safe_s16_ptr_set( ref( safe_var ), set_value )
	#define safe_s32_set( safe_var, set_value ) safe_s32_ptr_set( ref( safe_var ), set_value )
	#define safe_s64_set( safe_var, set_value ) safe_s64_ptr_set( ref( safe_var ), set_value )
	#define safe_s8_get( safe_var ) safe_s8_ptr_get( ref( safe_var ) )
	#define safe_s16_get( safe_var ) safe_s16_ptr_get( ref( safe_var ) )
	#define safe_s32_get( safe_var ) safe_s32_ptr_get( ref( safe_var ) )
	#define safe_s64_get( safe_var ) safe_s64_ptr_get( ref( safe_var ) )

//

make_union( safe_8 )
{
	s8 s;
	u8 u;
};

make_union( safe_16 )
{
	s16 s;
	u16 u;
};

make_union( safe_32 )
{
	s32 s;
	u32 u;
	f32 f;
};

make_union( safe_64 )
{
	s64 s;
	u64 u;
	f64 f;
	ptr( pure ) p;
};

	// Macros for unsigned integers
	#define safe_u8_ptr_set( safe_ptr, set_value ) safe_s8_ptr_set( safe_ptr, ( ( safe_8 ){ .u = set_value } ).s )
	#define safe_u16_ptr_set( safe_ptr, set_value ) safe_s16_ptr_set( safe_ptr, ( ( safe_16 ){ .u = set_value } ).s )
	#define safe_u32_ptr_set( safe_ptr, set_value ) safe_s32_ptr_set( safe_ptr, ( ( safe_32 ){ .u = set_value } ).s )
	#define safe_u64_ptr_set( safe_ptr, set_value ) safe_s64_ptr_set( safe_ptr, ( ( safe_64 ){ .u = set_value } ).s )

	#define safe_u8_ptr_get( safe_ptr ) make( safe_8, .s = safe_s8_ptr_get( safe_ptr ) ).u
	//#define safe_u8_ptr_get( safe_ptr ) ( ( safe_8 ){ .s = safe_s8_ptr_get( safe_ptr ) } ).u
	#define safe_u16_ptr_get( safe_ptr ) ( ( safe_16 ){ .s = safe_s16_ptr_get( safe_ptr ) } ).u
	#define safe_u32_ptr_get( safe_ptr ) ( ( safe_32 ){ .s = safe_s32_ptr_get( safe_ptr ) } ).u
	#define safe_u64_ptr_get( safe_ptr ) ( ( safe_64 ){ .s = safe_s64_ptr_get( safe_ptr ) } ).u

	#define safe_u8_set( safe_var, set_value ) safe_s8_set( safe_var, ( ( safe_8 ){ .u = set_value } ).s )
	#define safe_u16_set( safe_var, set_value ) safe_s16_set( safe_var, ( ( safe_16 ){ .u = set_value } ).s )
	#define safe_u32_set( safe_var, set_value ) safe_s32_set( safe_var, ( ( safe_32 ){ .u = set_value } ).s )
	#define safe_u64_set( safe_var, set_value ) safe_s64_set( safe_var, ( ( safe_64 ){ .u = set_value } ).s )

	#define safe_u8_get( safe_var ) ( ( safe_8 ){ .s = safe_s8_get( safe_var ) } ).u
	#define safe_u16_get( safe_var ) ( ( safe_16 ){ .s = safe_s16_get( safe_var ) } ).u
	#define safe_u32_get( safe_var ) ( ( safe_32 ){ .s = safe_s32_get( safe_var ) } ).u
	#define safe_u64_get( safe_var ) ( ( safe_64 ){ .s = safe_s64_get( safe_var ) } ).u

	// Macros for floating point numbers
	#define safe_f32_ptr_set( safe_ptr, set_value ) safe_s32_ptr_set( safe_ptr, ( ( safe_32 ){ .f = set_value } ).s )
	#define safe_f64_ptr_set( safe_ptr, set_value ) safe_s64_ptr_set( safe_ptr, ( ( safe_64 ){ .f = set_value } ).s )

	#define safe_f32_ptr_get( safe_ptr ) ( ( safe_32 ){ .s = safe_s32_ptr_get( safe_ptr ) } ).f
	#define safe_f64_ptr_get( safe_ptr ) ( ( safe_64 ){ .s = safe_s64_ptr_get( safe_ptr ) } ).f

	#define safe_f32_set( safe_var, set_value ) safe_s32_set( safe_var, ( ( safe_32 ){ .f = set_value } ).s )
	#define safe_f64_set( safe_var, set_value ) safe_s64_set( safe_var, ( ( safe_64 ){ .f = set_value } ).s )

	#define safe_f32_get( safe_var ) ( ( safe_32 ){ .s = safe_s32_get( safe_var ) } ).f
	#define safe_f64_get( safe_var ) ( ( safe_64 ){ .s = safe_s64_get( safe_var ) } ).f

	// Macros for pointers
	#define safe_ptr_set( safe_ptr, set_value ) safe_s64_set( safe_ptr, set_value )
	#define safe_ptr_get( safe_ptr ) safe_s64_get( safe_ptr )

// Spinlock type
make_type( safe s8 ) spinlock;

	// Spinlock macros
	#define engage_spinlock( lock )              \
		DEF_START                                  \
		spin( ( safe_s8_set( lock, 1 ) ) == 1 ) {} \
		DEF_END

	#define vacate_spinlock( lock ) safe_s8_set( lock, 0 )

//

make_struct( list )
{
	spinlock lock;
	s32 size, size_mem, size_type;
	ptr( u8 ) data;
};
make_ptr( struct( list ) ) list;

	#define iter_list( _, var ) iter( _->size, var )

fn list __new_list( in u32 in_size, in u32 in_size_mem, in u32 in_size_type, in ptr( pure ) in_data )
{
	list temp_list = new_mem( struct( list ), 1 );
	//
	temp_list->size = in_size;
	temp_list->size_mem = in_size_mem;
	temp_list->size_type = in_size_type;
	temp_list->data = ( ptr( u8 ) )in_data;
	//
	out temp_list;
}
	#define new_list_data( type, size, data ) __new_list( size, max( 1, size ), size_( type ), to( ptr( pure ), data ) )
	#define new_list( type ) new_list_data( type, 0, new_mem( u8, size_( type ) ) )

	#define lock_list( _ ) engage_spinlock( _->lock )
	#define unlock_list( _ ) vacate_spinlock( _->lock )

	#define list_alloc( _ )                                                     \
		DEF_START                                                                 \
		if( _->size == _->size_mem )                                              \
		{                                                                         \
			s32 temp_new_size_mem = to_s32( _->size_mem << 1 );                     \
			ptr( pure ) new_data = allocate_mem( temp_new_size_mem, _->size_type ); \
			copy_mem( new_data, _->data, _->size * _->size_type );                  \
			free_mem( _->data );                                                    \
			_->size_mem = temp_new_size_mem;                                        \
			_->data = new_data;                                                     \
		}                                                                         \
		DEF_END

	#define list_set( _, type, pos, val ) ( to( ptr( type ), _->data ) )[ ( pos ) ] = ( val )

	#define list_add( _, type, val )           \
		DEF_START                                \
		list_alloc( _ );                         \
		list_set( _, type, _->size++, ( val ) ); \
		DEF_END

	#define list_shift( _, n ) \
		copy_mem( to( ptr( pure ), _->data ), to( ptr( pure ), _->data + ( ( -( n ) ) * _->size_type ) ), ( ( _->size -= abs( n ) ) ) * _->size_type )

	#define list_move( _, start, length, n ) \
		copy_mem( to( ptr( pure ), _->data + ( ( ( start ) + ( n ) ) * _->size_type ) ), to( ptr( pure ), _->data + ( ( start )*_->size_type ) ), ( length )*_->size_type )

	#define list_insert( _, type, pos, val )               \
		DEF_START                                            \
		list_alloc( _ );                                     \
		list_move( _, ( pos ), _->size - ( pos ), 1 );       \
		( to( ptr( type ), _->data ) )[ ( pos ) ] = ( val ); \
		++( _ )->size;                                       \
		DEF_END

	#define list_delete( _, pos )                           \
		DEF_START                                             \
		list_move( _, ( pos ) + 1, _->size - ( pos )-1, -1 ); \
		--( _ )->size;                                        \
		DEF_END

	#define list_fill( _, val )                  \
		DEF_START                                  \
		iter( _->size, n ) _->data[ n ] = ( val ); \
		DEF_END

	#define list_free( _ )     \
		free_mem( ( _ )->data ); \
		free_mem( _ )

	#define list_get( _, type, pos ) ( to( ptr( type ), _->data ) )[ ( pos ) ]

	#define list_pop_front( _, type ) \
		list_get( _, type, 0 );         \
		list_shift( _, -1 )

	#define list_pop_back( _, type ) list_get( _, type, --( _->size ) )

//

make_struct( pile )
{
	spinlock lock;
	u32 size, prev_pos;
	list data, data_free;
}
struct_pile;
make_ptr( struct( pile ) ) pile;

	#define iter_pile( _, var ) iter( _->data->size, var )

fn pile __new_pile( in list in_list )
{
	pile temp_pile = new_mem( struct( pile ), 1 );
	//
	temp_pile->size = 0;
	temp_pile->prev_pos = 0;
	temp_pile->data = in_list;
	temp_pile->data_free = new_list( u32 );
	//
	out temp_pile;
}
	#define new_pile( type ) __new_pile( new_list( type ) )

	#define lock_pile( _ ) engage_spinlock( _->lock )
	#define unlock_pile( _ ) vacate_spinlock( _->lock )

	#define pile_add( _, type, val )                       \
		DEF_START                                            \
		_->size++;                                           \
		if( _->data_free->size )                             \
		{                                                    \
			_->prev_pos = list_pop_front( _->data_free, u32 ); \
			list_set( _->data, type, _->prev_pos, val );       \
		}                                                    \
		else                                                 \
		{                                                    \
			_->prev_pos = _->data->size;                       \
			list_add( _->data, type, val );                    \
		}                                                    \
		DEF_END

	#define pile_find( _, type, pos ) validate_maybe( list_get( _->data, type, pos ) )

	#define pile_delete( _, pos )                              \
		DEF_START                                                \
		_->size--;                                               \
		list_add( _->data_free, u32, pos );                      \
		list_set( _->data, u8, pos * _->data->size_type, 0x0u ); \
		DEF_END

//

/// rgba

make_struct( rgba )
{
	u8 r, g, b, a;
};
make_type( struct( rgba ) ) rgba;

	#define to_rgba( _ ) ( to( rgba, _ ) )
	#define size_rgba ( size_( rgba ) )
	#define make_rgba( r, g, b, a ) make( rgba, r, g, b, a )
	#define print_rgba( _ ) print( "%hhu,%hhu,%hhu,%hhu", rgba( _ ).r, rgba( _ ).g, rgba( _ ).b, rgba( _ ).a )

//

/// vectors

// float

make_struct( fvec2 )
{
	f32 x, y;
};
	#define to_struct_fvec2( _ ) ( to( struct( fvec2 ), _ ) )
	#define size_struct_fvec2 ( size_( struct( fvec2 ) ) )
	#define make_struct_fvec2( x, y ) make( struct( fvec2 ), x, y )

make_struct( fvec3 )
{
	f32 x, y, z;
};
	#define to_struct_fvec3( _ ) ( to( struct( fvec3 ), _ ) )
	#define size_struct_fvec3 ( size_( struct( fvec3 ) ) )
	#define make_struct_fvec3( x, y, z ) make( struct( fvec3 ), x, y, z )

make_struct( fvec4 )
{
	f32 x, y, z, w;
};
	#define to_struct_fvec4( _ ) ( to( struct( fvec4 ), _ ) )
	#define size_struct_fvec4 ( size_( struct( fvec4 ) ) )
	#define make_struct_fvec4( x, y, z, w ) make( struct( fvec4 ), x, y, z, w )

// signed int

make_struct( svec2 )
{
	s32 x, y;
};
	#define to_struct_svec2( _ ) ( to( struct( svec2 ), _ ) )
	#define size_struct_svec2 ( size_( struct( svec2 ) ) )
	#define make_struct_svec2( x, y ) make( struct( svec2 ), x, y )

make_struct( svec3 )
{
	s32 x, y, z;
};
	#define to_struct_svec3( _ ) ( to( struct( svec3 ), _ ) )
	#define size_struct_svec3 ( size_( struct( svec3 ) ) )
	#define make_struct_svec3( x, y, z ) make( struct( svec3 ), x, y, z )

make_struct( svec4 )
{
	s32 x, y, z, w;
};
	#define to_struct_svec4( _ ) ( to( struct( svec4 ), _ ) )
	#define size_struct_svec4 ( size_( struct( svec4 ) ) )
	#define make_struct_svec4( x, y, z, w ) make( struct( svec4 ), x, y, z, w )

// unsigned int

make_struct( uvec2 )
{
	u32 x, y;
};
	#define to_struct_uvec2( _ ) ( to( struct( uvec2 ), _ ) )
	#define size_struct_uvec2 ( size_( struct( uvec2 ) ) )
	#define make_struct_uvec2( x, y ) make( struct( uvec2 ), x, y )

make_struct( uvec3 )
{
	u32 x, y, z;
};
	#define to_struct_uvec3( _ ) ( to( struct( uvec3 ), _ ) )
	#define size_struct_uvec3 ( size_( struct( uvec3 ) ) )
	#define make_struct_uvec3( x, y, z ) make( struct( uvec3 ), x, y, z )

make_struct( uvec4 )
{
	u32 x, y, z, w;
};
	#define to_struct_uvec4( _ ) ( to( struct( uvec4 ), _ ) )
	#define size_struct_uvec4 ( size_( struct( uvec4 ) ) )
	#define make_struct_uvec4( x, y, z, w ) make( struct( uvec4 ), x, y, z, w )

//

#endif