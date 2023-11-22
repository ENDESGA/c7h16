// // // // // // //
// > c7h16 _
// -------
// explicit and minimal esoteric c abstraction
// requires: math.h, string.h, standard library
// @ENDESGA 2023
// // // // // // //

/*
	functional verbs:
	-------
	make_H() -> design H structure
	assign_H() -> out assign H in memory
	new_H() -> out constructed assign_H()
	create_H() -> construct in-scope value
*/

#pragma once
#ifndef c7h16_included
#define c7h16_included

#define OS_LINUX 0
#define OS_WINDOWS 0
#define OS_MACOS 0
#define OS_OTHER 0

#if defined( __WIN32__ ) || defined( WIN32 ) || defined( _WIN32 ) || defined( __CYGWIN__ ) || defined( __MINGW32__ ) || defined( __WINDOWS__ ) || defined( _WIN64 )
#undef OS_WINDOWS
#define OS_WINDOWS 1
#define _CRT_SECURE_NO_WARNINGS
#undef UNICODE
#undef _UNICODE
/*#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN
#define NOGDICAPMASKS*/
#include <windows.h>
#undef far
#undef near
#elif defined( __LINUX__ ) || defined( linux ) || defined( __linux ) || defined( __linux__ )
#undef OS_LINUX
#define OS_LINUX 1

#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <pthread.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/XKBlib.h>
#include <X11/Xutil.h>

#elif defined( __MACOSX__ ) || defined( __APPLE__ )
		#undef OS_MACOS
		#define OS_MACOS 1
#else
		#undef OS_OTHER
		#define OS_OTHER 1
#endif

#define COMPILER_MSVC 0
#define COMPILER_GCC 0
#define COMPILER_OTHER 0

#if defined( _MSC_VER )
		#undef COMPILER_MSVC
		#define COMPILER_MSVC 1
#elif defined( __GNUC__ )
#undef COMPILER_GCC
#define COMPILER_GCC 1

#include <stdatomic.h>

#else
		#undef COMPILER_OTHER
		#define COMPILER_OTHER 1
#endif

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

//

#define CONCAT_LINE_SET( a, line ) __##a##_##line
#define CONCAT_LINE( a, line ) CONCAT_LINE_SET( a, line )
#define VAR_LINE( name ) CONCAT_LINE( name, __LINE__ )

#define DEF_START \
		do              \
		{
#define DEF_END \
		}             \
		as( 0 )

//

/// constants

#define null NULL
#define yes ( 1 )
#define no ( 0 )

#define tau 6.283185307179586476925286766559

#define tau_m2 12.566370614359172953850573533118
#define tau_m3 18.849555921538759430775860299677
#define tau_m4 25.132741228718345907701147066236
#define tau_m5 31.415926535897932384626433832795
#define tau_m6 37.699111843077518861551720599354
#define tau_m7 43.982297150257105338477007365913

#define tau_d2 3.1415926535897932384626433832795
#define pi tau_d2
#define tau_d3 2.0943951023931954923084289221863
#define tau_d4 1.5707963267948966192313216916396
#define pi_d2 tau_d4
#define tau_d5 1.2566370614359172953850573533118
#define tau_d6 1.0471975511965977461542144610932
#define tau_d7 .897597901025655210989326680937
#define tau_d8 .785398163397448309615660845819
#define pi_d4 tau_d8

#define tau_p2 39.478417604357434475337963999505
#define tau_p3 248.05021344239856140381052053681
#define tau_p5 9792.6299131290065044077219213899
#define tau_p6 61528.908388819483969934044393755
#define tau_p7 386597.53315542938464181843111322

#define euler 2.7182818284590452353602874713527

#define _factorial_0 1
#define _factorial_1 1
#define _factorial_2 2
#define _factorial_3 6
#define _factorial_4 24
#define _factorial_5 120
#define _factorial_6 720
#define _factorial_7 5040
#define _factorial_8 40320
#define _factorial_9 362880
#define _factorial_10 3628800
#define _factorial_11 39916800
#define _factorial_12 479001600
#define _factorial_13 6227020800
#define _factorial_14 87178291200
#define _factorial_15 1307674368000
#define _factorial_16 20922789888000
#define _factorial_17 355687428096000
#define _factorial_18 6402373705728000
#define _factorial_19 121645100408832000
#define _factorial_20 2432902008176640000

#define FACTORIAL( _ ) _factorial_##_

//

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

#define constant const
#define once static
#define safe volatile
#define global safe once

#define to( TYPE, VAL ) ( TYPE )( VAL )

#define ptr( _ ) _*
#define val( _ ) *_
#define ref( _ ) &_
#define fn_ptr( OUTPUT, _, ... ) OUTPUT to( val( _ ), __VA_ARGS__ )
#define struct( _ ) struct _
#define enum( _ ) enum _
#define union( _ ) union _

#define create( _, ... ) ( ( _ ){ __VA_ARGS__ } )

#define make_type( _ ) typedef _
#define make_ptr( _ ) make_type( ptr( _ ) )
#define make_fn_ptr( OUTPUT, _, ... ) make_type( fn_ptr( OUTPUT, _, __VA_ARGS__ ) )
#define make_struct( _ ) make_type( struct _ ) _; struct( _ )
#define make_enum( _ ) enum( _ )
#define make_union( _ ) union( _ )

#define inl once inline
#define fn inl void

#define in constant
#define out return

//

#define size_( _ ) sizeof( _ )

#undef abs
#define abs( _ ) ( ( ( _ ) < 0 ) ? -( _ ) : ( _ ) )
#undef min
#define min( a, b ) ( ( ( a ) < ( b ) ) ? ( a ) : ( b ) )
#undef max
#define max( a, b ) ( ( ( a ) > ( b ) ) ? ( a ) : ( b ) )
#undef zsign
#define zsign( _ ) ( ( _ ) < 0 ? -1 : ( ( _ ) > 0 ? 1 : 0 ) )
#undef sign
#define sign( _ ) ( ( _ ) < 0 ? -1 : 1 )
#undef avg
#define avg( a, b ) ( ( ( a ) + ( b ) ) / 2. )
#undef lerp
#define lerp( a, b, t ) (((a) * (1.-(t))) + ((b) * (t)) )
#undef clamp
#define clamp( _, MIN, MAX ) ((_) < (MIN) ? (MIN) : ((_) > (MAX) ? (MAX) : (_)))

//

#define not !
#define and &&
#define or ||
#define mod %

#define is ==
#define isnt !=

#define ifn( _ ) if( !( _ ) )
#define ifnull( _ ) if( ( _ ) == null )
#define elif( _ ) else if( _ )

#define check( _ ) switch( _ )
#define with( _ ) case( _ ):
#define skip break
#define with_other default:

#define next continue

#define as( _ ) while( _ )
#define loop as( 1 )
#define do_once              \
		once u8 VAR_LINE( o ) = 1; \
		if( ( VAR_LINE( o ) == 1 ? VAR_LINE( o )-- : 0 ) )
#define iter( to_n, var )                 \
		register s32 VAR_LINE( to ) = ( to_n ); \
		if( VAR_LINE( to ) )                    \
			for( register s32 var = 0; var < VAR_LINE( to ); var++ )
#define rep( to_n ) iter( to_n, VAR_LINE( r ) )
#define range( from_n, to_n, var ) \
		for( register s32 var = from_n; var < to_n; var++ )

#define start_scope do
#define end_scope while( no )

//

#define cast( TYPE, _ ) ( val( to( ptr( TYPE ), ref( _ ) ) ) )

// Convert 1D index to 2D (x, y) position
#define index_to_2d( INDEX, WIDTH ) { ( INDEX ) % ( WIDTH ), ( INDEX ) / ( WIDTH ) }

// Convert 2D (x, y) position to 1D index
#define index_from_2d( X, Y, WIDTH ) ( ( Y )*WIDTH + ( X ) )

// Convert 1D index to 3D (x, y, z) position
#define index_to_3d( INDEX, WIDTH, HEIGHT ) ( ( s32[ 3 ] ){ ( INDEX ) % ( WIDTH ), ( ( INDEX ) / ( WIDTH ) ) % ( HEIGHT ), ( INDEX ) / ( ( WIDTH ) * ( HEIGHT ) ) } )

// Convert 3D (x, y, z) position to 1D index
#define index_from_3d( X, Y, Z, WIDTH, HEIGHT ) ( ( Z ) * ( WIDTH ) * ( HEIGHT ) + ( Y ) * ( WIDTH ) + ( X ) )

#define print( ... ) printf( __VA_ARGS__ )

#define pure void
#define size_pure size_( pure )

make_type(unsigned char) flag;
#define to_flag( _ ) ( !!( _ ) )
#define size_flag ( size_( flag ) )
#define print_flag( _ ) print( "%hhu", to_flag( _ ) )

make_type(unsigned char) u8;
#define to_u8( _ ) ( to( u8, _ ) )
#define size_u8 ( size_( u8 ) )
#define u8_min ( 0 )
#define u8_max ( 0xFFU )
#define print_u8( _ ) print( "%hhu", to_u8( _ ) )

make_type(unsigned short) u16;
#define to_u16( _ ) ( to( u16, _ ) )
#define size_u16 ( size_( u16 ) )
#define u16_min ( 0 )
#define u16_max ( 0xFFFFU )
#define print_u16( _ ) print( "%hu", to_u16( _ ) )

make_type(unsigned int) u32;
#define to_u32( _ ) ( to( u32, _ ) )
#define size_u32 ( size_( u32 ) )
#define u32_min ( 0 )
#define u32_max ( 0xFFFFFFFFU )
#define print_u32( _ ) print( "%u", to_u32( _ ) )

make_type(unsigned long long int) u64;
#define to_u64( _ ) ( to( u64, _ ) )
#define size_u64 ( size_( u64 ) )
#define u64_min ( 0 )
#define u64_max ( 0xFFFFFFFFFFFFFFFFU )
#define print_u64( _ ) print( "%llu", to_u64( _ ) )

make_type(char) s8;
#define to_s8( _ ) ( to( s8, _ ) )
#define size_s8 ( size_( s8 ) )
#define s8_min ( -128 )
#define s8_max ( 127 )
#define print_s8( _ ) print( "%hhd", to_s8( _ ) )

make_type(short) s16;
#define to_s16( _ ) ( to( s16, _ ) )
#define size_s16 ( size_( s16 ) )
#define s16_min ( -32768 )
#define s16_max ( 32767 )
#define print_s16( _ ) print( "%hd", to_s16( _ ) )

make_type(int) s32;
#define to_s32( _ ) ( to( s32, _ ) )
#define size_s32 ( size_( s32 ) )
#define s32_min ( -2147483648 )
#define s32_max ( 2147483647 )
#define print_s32( _ ) print( #_ ": %d\n", to_s32( _ ) )

make_type(long long int) s64;
#define to_s64( _ ) ( to( s64, _ ) )
#define size_s64 ( size_( s64 ) )
#define s64_min ( -9223372036854775808 )
#define s64_max ( 9223372036854775807 )
#define print_s64( _ ) print( "%lld", to_s64( _ ) )

make_type(float) f32;
#define to_f32( _ ) ( to( f32, _ ) )
#define size_f32 ( size_( f32 ) )
#define f32_min ( 1.175494351e-38F )
#define f32_max ( 3.402823466e+38F )
#define f32_epsilon ( 1.1920929e-7 )
#define f32_step ( 1.e-6 )
#define print_f32( _ ) print( #_ ": %f\n", to_f32( _ ) )

#define trunc_f32( f ) to_f32( to_s32( f ) )

inl f32 floor_f32(f32 f) {
    out (f < 0. and f != trunc_f32(f)) ? trunc_f32(f) - 1. : trunc_f32(f);
}

inl f32 round_f32(f32 f) {
    out (f >= 0.) ? trunc_f32(f + .5) : trunc_f32(f - .5);
}

inl f32 ceil_f32(f32 f) {
    out (f > 0. and f != trunc_f32(f)) ? trunc_f32(f) + 1. : trunc_f32(f);
}

inl f32 sqr_f32(f32 f) {
    out (f * f);
}

make_type(double) f64;
#define to_f64( _ ) ( to( f64, _ ) )
#define size_f64 ( size_( f64 ) )
#define f64_min ( 2.2250738585072014e-308 )
#define f64_max ( 1.7976931348623158e+308 )
#define f64_epsilon ( 2.220446e-16 )
#define f64_step ( 1.e-15 )
#define print_f64( _ ) print( "%lf", to_f64( _ ) )

#define trunc_f64( f ) to_f64( to_s64( f ) )

inl f64 floor_f64(f64 f) {
    out (f < 0. and f != trunc_f64(f)) ? trunc_f64(f) - 1. : trunc_f64(f);
}

inl f64 round_f64(f64 f) {
    out (f >= 0.) ? trunc_f64(f + .5) : trunc_f64(f - .5);
}

inl f64 ceil_f64(f64 f) {
    out (f > 0. and f != trunc_f64(f)) ? trunc_f64(f) + 1. : trunc_f64(f);
}

inl f64 sqr_f64(f64 f) {
    out (f * f);
}

//

/// nano

inl u64 get_ns() {
#ifdef _WIN32
    LARGE_INTEGER frequency;
    QueryPerformanceFrequency(ref(frequency));
    LARGE_INTEGER counter;
    QueryPerformanceCounter(ref(counter));
    out (counter.QuadPart * nano_per_sec) / frequency.QuadPart;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, ref(ts));
    out ts.tv_sec * nano_per_sec + ts.tv_nsec;
#endif
}

fn sleep_ns(u64 ns) {
#ifdef _WIN32
    LARGE_INTEGER start, end, freq;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&start);

    double elapsed_ns = 0;
    while (elapsed_ns < (double)ns) {
        QueryPerformanceCounter(&end);
        elapsed_ns = ((end.QuadPart - start.QuadPart) * 1e9) / freq.QuadPart;
    }
#else
    struct timespec ts;
    ts.tv_sec = ns / nano_per_sec;
    ts.tv_nsec = ns % nano_per_sec;
    nanosleep(&ts, NULL);
#endif
}

//

/// maybe
/*
make_struct( maybe )
{
	flag valid;
	u64 value;
};
make_type( struct( maybe ) ) maybe;

inl maybe validate_maybe( ptr( pure ) in_val )
{
	out( maybe ){ .valid = ( ( val( to( ptr( u64 ), in_val ) ) == 0 ) ? no : yes ), .value = val( to( ptr( u64 ), in_val ) ) };
}*/

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

make_type(__gnuc_va_list) va_list;
#endif

//

/// ptr

#define assign_ptr( bytes ) calloc( 1, bytes )
#define delete_ptr( p ) \
		if( p != null ) free( p )

#define new_ptr( type, n ) to( ptr( type ), calloc( n, size_( type ) ) )

#define copy_ptr( dst, src, n ) memcpy( dst, src, n )

/*inl ptr( pure ) copy_ptr( ptr( pure ) dst, in ptr( pure ) src, u64 n )
{
	ptr( u8 ) d8 = dst;
	ptr( u8 ) s8 = src;
	as( n-- ) val( d8 )++ = val( s8 )++;
	out dst;
}*/

//

/// text

#define upper_char( _ ) ( ( _ )&0xDF )
#define lower_char( _ ) ( ( _ ) | 0x20 )
#define upper_char_safe( _ ) ( ( ( _ ) >= 'a' and ( _ ) <= 'z' ) ? upper_char( _ ) : ( _ ) )
#define lower_char_safe( _ ) ( ( ( _ ) >= 'A' and ( _ ) <= 'Z' ) ? lower_char( _ ) : ( _ ) )

u8 dec_to_u8[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'};
u8 hex_to_u8[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', '0'};

make_type(ptr( s8 )) text;
#define to_text( _ ) to( text, _ )
#define size_text ( size_( text ) )

#define text_length( TEXT ) ( strlen( TEXT ) + 1 )
#define copy_text( TEXT_DST, TEXT_SRC ) strcpy( TEXT_DST, TEXT_SRC )
#define join_text( TEXT_DST, TEXT_SRC ) strcat( TEXT_DST, TEXT_SRC )
#define end_text( TEXT ) join_text( TEXT, "\0" )
#define compare_text( TEXT_A, TEXT_B ) ( strcmp( TEXT_A, TEXT_B ) == 0 )

#define assign_text( SIZE ) new_ptr( s8, SIZE )
#define new_text( DEFAULT_TEXT, EXTRA_CHARS ) copy_text( assign_text( text_length( DEFAULT_TEXT ) + ( EXTRA_CHARS ) ), DEFAULT_TEXT )
#define delete_text( _ ) \
		if( _ != null ) delete_ptr( _ )
#define print_text( _ ) print( "%s", _ )

inl text flip_text(in text in_text, in u32 in_length) {
    iter(in_length >> 1, i) {
        u32 j = in_length - i - 1;
        in_text[i] ^= in_text[j];
        in_text[j] ^= in_text[i];
        in_text[i] ^= in_text[j];
    }
    out in_text;
}

inl text format_text(in text in_formatted_text, ...) {
    va_list args;
    va_start(args, in_formatted_text);
    s32 len = vsnprintf(null, 0, in_formatted_text, args);
    va_end(args);
    if (len < 0)
        out null;
    text formatted_text = new_ptr(s8, len + 1);
    va_start(args, in_formatted_text);
    vsnprintf(formatted_text, len + 1, in_formatted_text, args);
    va_end(args);
    out formatted_text;
}

/*
make_type( ptr( s8 ) ) text;
	#define to_text( _ ) to( text, _ )
	#define size_text ( size_( text ) )

	#define assign_text( SIZE ) new_ptr( s8, SIZE + 2 )
	#define new_text( DEFAULT_TEXT, EXTRA_CHARS ) copy_ptr( assign_text( text_length( DEFAULT_TEXT ) + ( EXTRA_CHARS )), DEFAULT_TEXT, text_length( DEFAULT_TEXT ) )
	#define delete_text( _ ) \
		if( _ != null ) delete_ptr( _ )
	#define print_text( _ ) print( "%s", _ )

inl u64 text_length( in text str )
{
	text s = str;

	as( ( ( u64 )s mod size_u64 ) != 0 )
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

inl text copy_text( text in_dst, text in_text )
{
	if( !in_text or !val( in_text ) ) out in_dst;
	copy_ptr( in_dst, in_text, text_length( in_text ) );
	out in_dst;
}

inl text join_text( in text in_dst, in text in_text )
{
	copy_text( in_dst + text_length( in_dst ), in_text );
	out in_dst;
}

inl text end_text( in text str_base )
{
	out join_text( str_base, "\0" );
}

inl text flip_text( in text in_text, in u32 in_length )
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

inl text format_text( in text in_formatted_text, ... )
{
	va_list args;
	va_start( args, in_formatted_text );
	s32 len = vsnprintf( null, 0, in_formatted_text, args );
	va_end( args );
	if( len < 0 ) out null;
	text formatted_text = new_ptr( s8, len + 1 );
	va_start( args, in_formatted_text );
	vsnprintf( formatted_text, len + 1, in_formatted_text, args );
	va_end( args );
	out formatted_text;
}

flag compare_text( text in_text1, text in_text2 )
{
	if( in_text1 == in_text2 ) out yes;
	if( val( in_text1 ) != val( in_text2 ) ) out no;

	as(
		val( in_text1 ) and
		val( in_text2 ) and
		( val( in_text1 ) == val( in_text2 ) )
	)
	{
		in_text1++;
		in_text2++;
	}
	out( ( val( in_text1 ) - val( in_text2 ) ) == 0 );
}*/

//

global text NL = "\n";
#define print_nl print_text( NL )

#define print_debug( ... )        \
		DEF_START                       \
		print( "DEBUG: " __VA_ARGS__ ); \
		print_nl;                       \
		DEF_END

#define print_error( IF_YES, ... ) \
		DEF_START                        \
		if( IF_YES )                     \
		{                                \
			print( "FILE: %s, LINE: %d, ERROR: ", __FILE__, __LINE__ );            \
			print( __VA_ARGS__ );          \
			print_nl;                      \
			loop{};\
		}                                \
		DEF_END

#define print_trace( ... )        \
		DEF_START                       \
		print( "TRACE: " __VA_ARGS__ ); \
		print_nl;                       \
		DEF_END

//

// set
#if COMPILER_MSVC
		#define safe_s8_ptr_set( PTR, VALUE ) _InterlockedExchange8( to( safe ptr( s8 ), PTR ), to( s8, VALUE ) )
		#define safe_s16_ptr_set( PTR, VALUE ) _InterlockedExchange16( to( safe ptr( s16 ), PTR ), to( s16, VALUE ) )
		#define safe_s32_ptr_set( PTR, VALUE ) _InterlockedExchange( to( safe ptr( long ), PTR ), to( long, VALUE ) )
		#define safe_s64_ptr_set( PTR, VALUE ) _InterlockedExchange64( to( safe ptr( s64 ), PTR ), to( s64, VALUE ) )
#elif COMPILER_GCC
#define safe_s8_ptr_set( PTR, VALUE ) ( atomic_exchange( ( PTR ), ( VALUE ) ) )
#define safe_s16_ptr_set( PTR, VALUE ) ( atomic_exchange( ( PTR ), ( VALUE ) ) )
#define safe_s32_ptr_set( PTR, VALUE ) ( atomic_exchange( ( PTR ), ( VALUE ) ) )
#define safe_s64_ptr_set( PTR, VALUE ) ( atomic_exchange( ( PTR ), ( VALUE ) ) )
#endif

// get
#if COMPILER_MSVC
		#define safe_s8_ptr_get( PTR ) _InterlockedOr8( to( safe ptr( s8 ), PTR ), 0 )
		#define safe_s16_ptr_get( PTR ) _InterlockedOr16( to( safe ptr( s16 ), PTR ), 0 )
		#define safe_s32_ptr_get( PTR ) _InterlockedOr( to( safe ptr( long ), PTR ), 0 )
		#define safe_s64_ptr_get( PTR ) _InterlockedOr64( to( safe ptr( s64 ), PTR ), 0 )
#elif COMPILER_GCC
#define safe_s8_ptr_get( PTR ) ( atomic_load( ( PTR ) ) )
#define safe_s16_ptr_get( PTR ) ( atomic_load( ( PTR ) ) )
#define safe_s32_ptr_get( PTR ) ( atomic_load( ( PTR ) ) )
#define safe_s64_ptr_get( PTR ) ( atomic_load( ( PTR ) ) )
#endif

// increment
#if COMPILER_MSVC
		#define safe_s8_ptr_inc( PTR ) _InterlockedIncrement( to( safe ptr( s8 ), PTR ) )
		#define safe_s16_ptr_inc( PTR ) _InterlockedIncrement16( to( safe ptr( s16 ), PTR ) )
		#define safe_s32_ptr_inc( PTR ) _InterlockedIncrement( to( safe ptr( long ), PTR ) )
		#define safe_s64_ptr_inc( PTR ) _InterlockedIncrement64( to( safe ptr( s64 ), PTR ) )
#elif COMPILER_GCC
#define safe_s8_ptr_inc( PTR ) ( atomic_fetch_add( ( PTR ), 1 ) )
#define safe_s16_ptr_inc( PTR ) ( atomic_fetch_add( ( PTR ), 1 ) )
#define safe_s32_ptr_inc( PTR ) ( atomic_fetch_add( ( PTR ), 1 ) )
#define safe_s64_ptr_inc( PTR ) ( atomic_fetch_add( ( PTR ), 1 ) )
#endif

// decrement
#if COMPILER_MSVC
		#define safe_s8_ptr_dec( PTR ) _InterlockedDecrement( to( safe ptr( s8 ), PTR ) )
		#define safe_s16_ptr_dec( PTR ) _InterlockedDecrement16( to( safe ptr( s16 ), PTR ) )
		#define safe_s32_ptr_dec( PTR ) _InterlockedDecrement( to( safe ptr( long ), PTR ) )
		#define safe_s64_ptr_dec( PTR ) _InterlockedDecrement64( to( safe ptr( s64 ), PTR ) )
#elif COMPILER_GCC
#define safe_s8_ptr_dec( PTR ) ( atomic_fetch_sub( ( PTR ), 1 ) )
#define safe_s16_ptr_dec( PTR ) ( atomic_fetch_sub( ( PTR ), 1 ) )
#define safe_s32_ptr_dec( PTR ) ( atomic_fetch_sub( ( PTR ), 1 ) )
#define safe_s64_ptr_dec( PTR ) ( atomic_fetch_sub( ( PTR ), 1 ) )
#endif

//

#define safe_s8_set( VAR, VALUE ) safe_s8_ptr_set( ref( VAR ), VALUE )
#define safe_s16_set( VAR, VALUE ) safe_s16_ptr_set( ref( VAR ), VALUE )
#define safe_s32_set( VAR, VALUE ) safe_s32_ptr_set( ref( VAR ), VALUE )
#define safe_s64_set( VAR, VALUE ) safe_s64_ptr_set( ref( VAR ), VALUE )
#define safe_s8_get( VAR ) safe_s8_ptr_get( ref( VAR ) )
#define safe_s16_get( VAR ) safe_s16_ptr_get( ref( VAR ) )
#define safe_s32_get( VAR ) safe_s32_ptr_get( ref( VAR ) )
#define safe_s64_get( VAR ) safe_s64_ptr_get( ref( VAR ) )
#define safe_s8_inc( VAR ) safe_s8_ptr_inc( ref( VAR ) )
#define safe_s16_inc( VAR ) safe_s16_ptr_inc( ref( VAR ) )
#define safe_s32_inc( VAR ) safe_s32_ptr_inc( ref( VAR ) )
#define safe_s64_inc( VAR ) safe_s64_ptr_inc( ref( VAR ) )
#define safe_s8_dec( VAR ) safe_s8_ptr_dec( ref( VAR ) )
#define safe_s16_dec( VAR ) safe_s16_ptr_dec( ref( VAR ) )
#define safe_s32_dec( VAR ) safe_s32_ptr_dec( ref( VAR ) )
#define safe_s64_dec( VAR ) safe_s64_ptr_dec( ref( VAR ) )

//

make_union(safe_8) {
    safe s8 s;
    safe u8 u;
};

make_union(safe_16) {
    safe s16 s;
    safe u16 u;
};

make_union(safe_32) {
    safe s32 s;
    safe u32 u;
    safe f32 f;
};

make_union(safe_64) {
    safe s64 s;
    safe u64 u;
    safe f64 f;
    ptr(pure) p;
};

#define safe_u8_ptr_set( PTR, VALUE ) safe_s8_ptr_set( PTR, create( union( safe_8 ), .u = VALUE ).s )
#define safe_u16_ptr_set( PTR, VALUE ) safe_s16_ptr_set( PTR, create( union( safe_16 ), .u = VALUE ).s )
#define safe_u32_ptr_set( PTR, VALUE ) safe_s32_ptr_set( PTR, create( union( safe_32 ), .u = VALUE ).s )
#define safe_u64_ptr_set( PTR, VALUE ) safe_s64_ptr_set( PTR, create( union( safe_64 ), .u = VALUE ).s )
#define safe_u8_ptr_get( PTR ) create( union( safe_8 ), .s = safe_s8_ptr_get( PTR ) ).u
#define safe_u16_ptr_get( PTR ) create( union( safe_16 ), .s = safe_s16_ptr_get( PTR ) ).u
#define safe_u32_ptr_get( PTR ) create( union( safe_32 ), .s = safe_s32_ptr_get( PTR ) ).u
#define safe_u64_ptr_get( PTR ) create( union( safe_64 ), .s = safe_s64_ptr_get( PTR ) ).u

#define safe_u8_set( VAR, VALUE ) safe_s8_set( VAR, create( union( safe_8 ), .u = VALUE ).s )
#define safe_u16_set( VAR, VALUE ) safe_s16_set( VAR, create( union( safe_16 ), .u = VALUE ).s )
#define safe_u32_set( VAR, VALUE ) safe_s32_set( VAR, create( union( safe_32 ), .u = VALUE ).s )
#define safe_u64_set( VAR, VALUE ) safe_s64_set( VAR, create( union( safe_64 ), .u = VALUE ).s )
#define safe_u8_get( VAR ) create( union( safe_8 ), .s = safe_s8_get( VAR ) ).u
#define safe_u16_get( VAR ) create( union( safe_16 ), .s = safe_s16_get( VAR ) ).u
#define safe_u32_get( VAR ) create( union( safe_32 ), .s = safe_s32_get( VAR ) ).u
#define safe_u64_get( VAR ) create( union( safe_64 ), .s = safe_s64_get( VAR ) ).u

#define safe_u8_inc( VAR ) safe_s8_inc( VAR )
#define safe_u16_inc( VAR ) safe_s16_inc( VAR )
#define safe_u32_inc( VAR ) safe_s32_inc( VAR )
#define safe_u64_inc( VAR ) safe_s64_inc( VAR )
#define safe_u8_dec( VAR ) safe_s8_dec( VAR )
#define safe_u16_dec( VAR ) safe_s16_dec( VAR )
#define safe_u32_dec( VAR ) safe_s32_dec( VAR )
#define safe_u64_dec( VAR ) safe_s64_dec( VAR )

// macros for floating point numbers
#define safe_f32_ptr_set( PTR, VALUE ) safe_s32_ptr_set( PTR, create( union( safe_32 ), .f = VALUE ).s )
#define safe_f64_ptr_set( PTR, VALUE ) safe_s64_ptr_set( PTR, create( union( safe_64 ), .f = VALUE ).s )
#define safe_f32_ptr_get( PTR ) create( union( safe_32 ), .s = safe_s32_ptr_get( PTR ) ).f
#define safe_f64_ptr_get( PTR ) create( union( safe_64 ), .s = safe_s64_ptr_get( PTR ) ).f

#define safe_f32_set( VAR, VALUE ) safe_s32_set( VAR, create( union( safe_32 ), .f = VALUE ).s )
#define safe_f64_set( VAR, VALUE ) safe_s64_set( VAR, create( union( safe_64 ), .f = VALUE ).s )
#define safe_f32_get( VAR ) create( union( safe_32 ), .s = safe_s32_get( VAR ) ).f
#define safe_f64_get( VAR ) create( union( safe_64 ), .s = safe_s64_get( VAR ) ).f

// macros for pointers
#define safe_ptr_set( PTR, VALUE ) safe_s64_set( PTR, VALUE )
#define safe_ptr_get( PTR ) safe_s64_get( PTR )

#define safe_flag_get( VAR ) safe_u8_get( VAR )
#define safe_flag_set( VAR, VALUE ) safe_u8_set( VAR, VALUE )

// spinlock type
make_type(safe s8) spinlock;

// spinlock macros
#define engage_spinlock( LOCK )            \
		DEF_START                                \
		as( ( safe_s8_set( LOCK, 1 ) ) == 1 ) {} \
		DEF_END

#define vacate_spinlock( LOCK ) safe_s8_set( LOCK, 0 )

//

make_struct(list_struct) {
    spinlock lock;
    s32 size, size_ptr, size_type;
    ptr(u8) data;
};

make_ptr(list_struct) list;

#define iter_list( LIST, TYPE ) iter( LIST->size, _iter_##TYPE )

inl list assign_list(in s32 in_size, in s32 in_size_ptr, in s32 in_size_type, in ptr(pure) in_data) {
    list temp_list = new_ptr(list_struct, 1);
    //
    temp_list->size = in_size;
    temp_list->size_ptr = in_size_ptr;
    temp_list->size_type = in_size_type;
    temp_list->data = (ptr(u8))in_data;
    //
    out temp_list;
}

#define new_list_data( type, size, data ) assign_list( size, max( 1, size ), size_( type ), to( ptr( pure ), data ) )
#define new_list( type ) new_list_data( type, 0, new_ptr( u8, size_( type ) ) )

#define lock_list( LIST ) engage_spinlock( LIST->lock )
#define unlock_list( LIST ) vacate_spinlock( LIST->lock )

#define list_assign( LIST )                                                \
		DEF_START                                                                \
		as( LIST->size >= LIST->size_ptr )                                       \
		{                                                                        \
			LIST->size_ptr = to_s32( LIST->size_ptr << 1 );                        \
			ptr( pure ) new_data = assign_ptr( LIST->size_ptr * LIST->size_type ); \
			copy_ptr( new_data, LIST->data, LIST->size * LIST->size_type );        \
			delete_ptr( LIST->data );                                              \
			LIST->data = new_data;                                                 \
		}                                                                        \
		DEF_END

#define list_set( LIST, type, pos, val ) ( to( ptr( type ), LIST->data ) )[ ( pos ) ] = ( val )

#define list_add( LIST, type, val )              \
		DEF_START                                      \
		list_assign( LIST );                           \
		list_set( LIST, type, LIST->size++, ( val ) ); \
		DEF_END

#define list_safe_add( LIST, type, val ) \
		DEF_START                              \
		lock_list( LIST );                     \
		list_add( LIST, type, val );           \
		unlock_list( LIST );                   \
		DEF_END

#define list_shift( LIST, n ) \
		copy_ptr( to( ptr( pure ), LIST->data ), to( ptr( pure ), LIST->data + ( ( -( n ) ) * LIST->size_type ) ), ( ( LIST->size -= abs( n ) ) ) * LIST->size_type )

#define list_move( LIST, start, length, n ) \
		copy_ptr( to( ptr( pure ), LIST->data + ( ( ( start ) + ( n ) ) * LIST->size_type ) ), to( ptr( pure ), LIST->data + ( ( start )*LIST->size_type ) ), ( length )*LIST->size_type )

#define list_insert( LIST, type, pos, val )               \
		DEF_START                                               \
		list_assign( LIST );                                    \
		list_move( LIST, ( pos ), LIST->size - ( pos ), 1 );    \
		( to( ptr( type ), LIST->data ) )[ ( pos ) ] = ( val ); \
		++( LIST )->size;                                       \
		DEF_END

#define list_delete( LIST, pos )                              \
		DEF_START                                                   \
		list_move( LIST, ( pos ) + 1, LIST->size - ( pos )-1, -1 ); \
		--( LIST )->size;                                           \
		DEF_END

#define list_fill( LIST, val )                     \
		DEF_START                                        \
		iter( LIST->size, n ) LIST->data[ n ] = ( val ); \
		DEF_END

#define delete_list( LIST )          \
		DEF_START                          \
		if( LIST == null ) skip;           \
		if( ( LIST )->data == null ) skip; \
		delete_ptr( ( LIST )->data );      \
		delete_ptr( LIST );                \
		DEF_END

#define empty_list( LIST ) \
		LIST->size = 0;

#define list_get( LIST, type, pos ) ( to( ptr( type ), LIST->data ) )[ ( pos ) ]
#define list_safe_get( LIST, type, pos ) ( to( ptr( type ), LIST->data ) )[ ( pos ) ]

#define list_get_iter( LIST, TYPE ) \
TYPE this_##TYPE = list_get( LIST, TYPE, _iter_##TYPE ); \
if( this_##TYPE == null ) next;

	#define list_get_iter_ptr( LIST, TYPE ) \
ptr(TYPE) this_##TYPE = ref(list_get( LIST, TYPE, _iter_##TYPE )); \
if( cast(u64,this_##TYPE) == 0 ) next;

#define list_remove_front( LIST, type ) \
		list_get( LIST, type, 0 );            \
		list_shift( LIST, -1 )

#define list_remove_back( LIST, type ) list_get( LIST, type, --( LIST->size ) )

//

make_struct(pile_struct) {
    spinlock lock;
    u32 size, prev_pos;
    list data, data_free;
};

make_ptr(pile_struct) pile;

#define iter_pile( PILE, TYPE )       \
		TYPE this_##TYPE;             \
		u32 TYPE##_n = 0;                    \
		u32 PILE##_size = PILE->size; \
		iter( PILE->data->size, _iter_##TYPE )

#define iter_pile_ptr( PILE, TYPE )       \
ptr(TYPE) this_##TYPE;             \
u32 TYPE##_n = 0;                    \
u32 PILE##_size = PILE->size; \
iter( PILE->data->size, _iter_##TYPE )

inl pile __new_pile(in list in_list) {
    pile temp_pile = new_ptr(pile_struct, 1);
    //
    temp_pile->size = 0;
    temp_pile->prev_pos = 0;
    temp_pile->data = in_list;
    temp_pile->data_free = new_list(u32);
    //
    out temp_pile;
}

#define new_pile( type ) __new_pile( new_list( type ) )

#define lock_pile( _ ) engage_spinlock( _->lock )
#define unlock_pile( _ ) vacate_spinlock( _->lock )

#define pile_add( _, type, val )                         \
		DEF_START                                              \
		_->size++;                                             \
		if( _->data_free->size )                               \
		{                                                      \
			_->prev_pos = list_remove_back( _->data_free, u32 ); \
			list_set( _->data, type, _->prev_pos, val );         \
		}                                                      \
		else                                                   \
		{                                                      \
			_->prev_pos = _->data->size;                         \
			list_add( _->data, type, val );                      \
		}                                                      \
		DEF_END

#define pile_safe_add( _, type, val ) \
		DEF_START                           \
		lock_pile( _ );                     \
		pile_add( _, type, val );           \
		unlock_pile( _ );                   \
		DEF_END

#define pile_find( _, type, pos ) list_get( _->data, type, pos )

#define pile_find_iter( PILE, TYPE )                   \
		if( TYPE##_n >= PILE##_size ) skip;                  \
		this_##TYPE = pile_find( PILE, TYPE, _iter_##TYPE ); \
		if( cast(u64,this_##TYPE) == 0 ) next;                      \
		else                                                 \
			TYPE##_n++;

#define pile_find_iter_ptr( PILE, TYPE )                   \
if( TYPE##_n >= PILE##_size ) skip;                  \
this_##TYPE = ref(pile_find( PILE, TYPE, _iter_##TYPE )); \
if( val(cast(ptr(u64),this_##TYPE)) == 0 ) next;                      \
else                                                 \
TYPE##_n++;

#define pile_safe_find( _, type, pos ) list_safe_get( _->data, type, pos )

#define pile_delete( _, type, pos )              \
		DEF_START                                      \
		_->size--;                                     \
		list_add( _->data_free, u32, pos );            \
		list_set( _->data, type, pos, ( type ){ 0 } ); \
		DEF_END

#define pile_safe_delete( _, type, pos ) \
		DEF_START                              \
		lock_pile( _ );                        \
		pile_delete( _, type, pos );           \
		unlock_pile( _ );                      \
		DEF_END

#define delete_pile( _ )       \
		DEF_START                    \
		delete_list( _->data );      \
		delete_list( _->data_free ); \
		delete_ptr( _ );             \
		DEF_END

//

/// rgba

make_struct(rgba) {
    u8 r, g, b, a;
};

#define to_rgba( _ ) ( to( rgba, _ ) )
#define size_rgba ( size_( rgba ) )
#define create_rgba( r, g, b, a ) create( rgba, r, g, b, a )
#define print_rgba( _ ) print( "%hhu,%hhu,%hhu,%hhu", rgba( _ ).r, rgba( _ ).g, rgba( _ ).b, rgba( _ ).a )

//

/// random

#define _heptaplex_collapse( TYPE, SHIFT )                                      \
		TYPE random_##TYPE( TYPE x, TYPE y, TYPE z )                                  \
		{                                                                             \
			x = ~( ~x - y - z ) * ~( x - ~y - z ) * ~( x - y - ~z );                    \
			y = ~( ~x - y - z ) * ~( x - ~y - z ) * ~( x - y - ~z );                    \
			z = x ^ y ^ ( ~( ~x - y - z ) * ~( x - ~y - z ) * ~( x - y - ~z ) );        \
			out z ^ ~( ~z >> SHIFT );                                                   \
		}                                                                             \
                                                                                  \
		TYPE random_range_##TYPE( in TYPE in_min, in TYPE in_max )                    \
		{                                                                             \
			once TYPE x = 1, y = 2, z = 3;                                              \
			out in_min + ( random_##TYPE( x++, y++, z++ ) mod( in_max - in_min + 1 ) ); \
		}

_heptaplex_collapse(u8, 4);
_heptaplex_collapse(s8, 4);
_heptaplex_collapse(u16, 8);
_heptaplex_collapse(s16, 8);
_heptaplex_collapse(u32, 16);
_heptaplex_collapse(s32, 16);
_heptaplex_collapse(u64, 32);
_heptaplex_collapse(s64, 32);

#define random_s32( x, y, z ) to_s32( random_u32( x, y, z ) )

f32 noise_f32() {
    once u32 x = 1, y = 2, z = 3;
    out to_f32(random_u32( x++, y++, z++ )) / to_f32(0xffffffffu);
}

f64 noise_f64() {
    once s32 x = 1, y = 2, z = 3;
    out random_s64(x++, y++, z++) / to_f64(0x7fffffffffffffffu);
}

f32 random_range_f32(in f32 in_min, in f32 in_max) {
    out in_min + ((in_max - in_min) * noise_f32());
}

//

	#define choose( TYPE, ... ) ( (TYPE[]){ __VA_ARGS__ } )[random_range_u32(0,(sizeof((TYPE[]){__VA_ARGS__})/sizeof(TYPE))-1)]
	#define choose_ptr( ... ) choose( ptr(pure), __VA_ARGS__ )


/// vectors

#define _vec_functions_2( TYPE )

// float

make_struct(fvec2) {
    union {
        struct {
            f32 x, y;
        };

        struct {
            f32 w, h;
        };
    };
};

#define to_fvec2( _ ) ( to( fvec2, _ ) )
#define size_fvec2 ( size_( fvec2 ) )
#define create_fvec2( x, y ) create( fvec2, x, y )

inl fvec2 fvec2_neg(in fvec2 in_v) {
    out create_fvec2(-in_v.x, -in_v.y);
}

inl fvec2 fvec2_add(in fvec2 in_a, in fvec2 in_b) {
    out create_fvec2(in_a.x + in_b.x, in_a.y + in_b.y);
}

inl fvec2 fvec2_add_f32(in fvec2 in_a, in f32 in_b) {
    out create_fvec2(in_a.x + in_b, in_a.y + in_b);
}

inl fvec2 fvec2_sub(in fvec2 in_a, in fvec2 in_b) {
    out create_fvec2(in_a.x - in_b.x, in_a.y - in_b.y);
}

inl fvec2 fvec2_sub_f32(in fvec2 in_a, in f32 in_b) {
    out create_fvec2(in_a.x - in_b, in_a.y - in_b);
}

inl fvec2 fvec2_mul(in fvec2 in_a, in fvec2 in_b) {
    out create_fvec2(in_a.x * in_b.x, in_a.y * in_b.y);
}

inl fvec2 fvec2_mul_f32(in fvec2 in_a, in f32 in_b) {
    out create_fvec2(in_a.x * in_b, in_a.y * in_b);
}

inl fvec2 fvec2_div(in fvec2 in_a, in fvec2 in_b) {
    out create_fvec2(in_a.x / in_b.x, in_a.y / in_b.y);
}

inl fvec2 fvec2_div_f32(in fvec2 in_a, in f32 in_b) {
    out create_fvec2(in_a.x / in_b, in_a.y / in_b);
}

inl fvec2 fvec2_avg( in fvec2 in_a, in fvec2 in_b )
{
		out create_fvec2( avg( in_a.x, in_b.x ), avg( in_a.y, in_b.y ) );
}

inl fvec2 fvec2_avg_f32( in fvec2 in_a, in f32 in_b )
{
		out create_fvec2( avg( in_a.x, in_b ), avg( in_a.y, in_b ) );
}

inl fvec2 fvec2_sqr(in fvec2 in_v) {
    out create_fvec2(in_v.x * in_v.x, in_v.y * in_v.y);
}

inl fvec2 fvec2_sqrt(in fvec2 in_v) {
    out create_fvec2(sqrtf( in_v.x ), sqrtf( in_v.y ));
}

inl f32 fvec2_dot(in fvec2 a, in fvec2 b) {
    out (a.x * b.x) + (a.y * b.y);
}

inl f32 fvec2_dot2(in fvec2 v) {
    out fvec2_dot(v, v);
}

inl f32 fvec2_len(in fvec2 v) {
    out sqrtf(fvec2_dot2(v));
}

inl f32 fvec2_dis(in fvec2 a, in fvec2 b) {
    out fvec2_len(create_fvec2(a.x - b.x, a.y - b.y));
}

inl flag fvec2_dis_check( in fvec2 a, in fvec2 b, in f32 in_dis )
{
		out fvec2_dot2( create_fvec2( a.x - b.x, a.y - b.y ) ) < sqr_f32( in_dis );
}

inl f32 fvec2_rad(in fvec2 v) {
		out atan2f( v.y, v.x );
}

inl f32 fvec2_dir( in fvec2 a, in fvec2 b )
{
		out fvec2_rad( create_fvec2( b.x - a.x, b.y - a.y ) );
}

inl fvec2 fvec2_norm(in fvec2 v) {
    out fvec2_div_f32(v, fvec2_len(v));
}

inl fvec2 fvec2_lerp(in fvec2 in_a, in fvec2 in_b, in f32 m) {
    out create_fvec2(( in_a.x * ( 1. - m ) ) + ( in_b.x * m ), ( in_a.y * ( 1. - m ) ) + ( in_b.y * m ));
}

inl fvec2 fvec2_floor(in fvec2 v) {
		out create_fvec2(floor_f32( v.x ), floor_f32( v.y ));
}

//

make_struct(fvec3) {
    union {
        struct {
            f32 x, y, z;
        };

        struct {
            f32 w, h, d;
        };
    };
};

#define to_fvec3( _ ) ( to( fvec3, _ ) )
#define size_fvec3 ( size_( fvec3 ) )
#define create_fvec3( x, y, z ) create( fvec3, x, y, z )

inl fvec3 fvec3_neg(in fvec3 in_v) {
    out create_fvec3(-in_v.x, -in_v.y, -in_v.z);
}

inl fvec3 fvec3_add(in fvec3 in_a, in fvec3 in_b) {
    out create_fvec3(in_a.x + in_b.x, in_a.y + in_b.y, in_a.z + in_b.z);
}

inl fvec3 fvec3_add_f32(in fvec3 in_a, in f32 in_b) {
    out create_fvec3(in_a.x + in_b, in_a.y + in_b, in_a.z + in_b);
}

inl fvec3 fvec3_sub(in fvec3 in_a, in fvec3 in_b) {
    out create_fvec3(in_a.x - in_b.x, in_a.y - in_b.y, in_a.z - in_b.z);
}

inl fvec3 fvec3_sub_f32(in fvec3 in_a, in f32 in_b) {
    out create_fvec3(in_a.x - in_b, in_a.y - in_b, in_a.z - in_b);
}

inl fvec3 fvec3_mul(in fvec3 in_a, in fvec3 in_b) {
    out create_fvec3(in_a.x * in_b.x, in_a.y * in_b.y, in_a.z * in_b.z);
}

inl fvec3 fvec3_mul_f32(in fvec3 in_a, in f32 in_b) {
    out create_fvec3(in_a.x * in_b, in_a.y * in_b, in_a.z * in_b);
}

inl fvec3 fvec3_div(in fvec3 in_a, in fvec3 in_b) {
    out create_fvec3(in_a.x / in_b.x, in_a.y / in_b.y, in_a.z / in_b.z);
}

inl fvec3 fvec3_div_f32(in fvec3 in_a, in f32 in_b) {
    out create_fvec3(in_a.x / in_b, in_a.y / in_b, in_a.z / in_b);
}

inl f32 fvec3_dot(in fvec3 a, in fvec3 b) {
    out (a.x * b.x) + (a.y * b.y) + (a.z * b.z);
}

inl f32 fvec3_dot2(in fvec3 v) {
    out fvec3_dot(v, v);
}

inl fvec3 fvec3_cross(in fvec3 a, in fvec3 b) {
    out create_fvec3(
        ( a.y * b.z ) - ( a.z * b.y ),
        ( a.z * b.x ) - ( a.x * b.z ),
        ( a.x * b.y ) - ( a.y * b.x )
    );
}

inl f32 fvec3_len(in fvec3 v) {
    out sqrtf(fvec3_dot2(v));
}

inl fvec3 fvec3_norm(in fvec3 v) {
    out fvec3_div_f32(v, fvec3_len(v));
}

//

make_struct(fvec4) {
    f32 x, y, z, w;
};

#define to_fvec4( _ ) ( to( fvec4, _ ) )
#define size_fvec4 ( size_( fvec4 ) )
#define create_fvec4( x, y, z, w ) create( fvec4, x, y, z, w )

// signed int

make_struct(svec2) {
    union {
        struct {
            s32 x, y;
        };

        struct {
            s32 w, h;
        };
    };
};

#define to_svec2( _ ) ( to( svec2, _ ) )
#define size_svec2 ( size_( svec2 ) )
#define create_svec2( x, y ) create( svec2, x, y )

make_struct(svec3) {
    union {
        struct {
            s32 x, y, z;
        };

        struct {
            s32 w, h, d;
        };
    };
};

#define to_svec3( _ ) ( to( svec3, _ ) )
#define size_svec3 ( size_( svec3 ) )
#define create_svec3( x, y, z ) create( svec3, x, y, z )

inl svec3 svec3_neg(in svec3 in_v) {
    out create_svec3(-in_v.x, -in_v.y, -in_v.z);
}

inl svec3 svec3_add(in svec3 in_a, in svec3 in_b) {
    out create_svec3(in_a.x + in_b.x, in_a.y + in_b.y, in_a.z + in_b.z);
}

inl svec3 svec3_add_s32(in svec3 in_a, in s32 in_b) {
    out create_svec3(in_a.x + in_b, in_a.y + in_b, in_a.z + in_b);
}

inl svec3 svec3_sub(in svec3 in_a, in svec3 in_b) {
    out create_svec3(in_a.x - in_b.x, in_a.y - in_b.y, in_a.z - in_b.z);
}

inl svec3 svec3_sub_s32(in svec3 in_a, in s32 in_b) {
    out create_svec3(in_a.x - in_b, in_a.y - in_b, in_a.z - in_b);
}

inl svec3 svec3_mul(in svec3 in_a, in svec3 in_b) {
    out create_svec3(in_a.x * in_b.x, in_a.y * in_b.y, in_a.z * in_b.z);
}

inl svec3 svec3_mul_s32(in svec3 in_a, in s32 in_b) {
    out create_svec3(in_a.x * in_b, in_a.y * in_b, in_a.z * in_b);
}

inl svec3 svec3_div(in svec3 in_a, in svec3 in_b) {
    out create_svec3(in_a.x / in_b.x, in_a.y / in_b.y, in_a.z / in_b.z);
}

inl svec3 svec3_div_s32(in svec3 in_a, in s32 in_b) {
    out create_svec3(in_a.x / in_b, in_a.y / in_b, in_a.z / in_b);
}

make_struct(svec4) {
    s32 x, y, z, w;
};

#define to_svec4( _ ) ( to( svec4, _ ) )
#define size_svec4 ( size_( svec4 ) )
#define create_svec4( x, y, z, w ) create( svec4, x, y, z, w )

// unsigned int

make_struct(uvec2) {
    union {
        struct {
            u32 x, y;
        };

        struct {
            u32 w, h;
        };
    };
};

#define to_uvec2( _ ) ( to( uvec2, _ ) )
#define size_uvec2 ( size_( uvec2 ) )
#define create_uvec2( x, y ) create( uvec2, x, y )

make_struct(uvec3) {
    union {
        struct {
            u32 x, y, z;
        };

        struct {
            u32 w, h, d;
        };
    };
};

#define to_uvec3( _ ) ( to( uvec3, _ ) )
#define size_uvec3 ( size_( uvec3 ) )
#define create_uvec3( x, y, z ) create( uvec3, x, y, z )

make_struct(uvec4) {
    u32 x, y, z, w;
};

#define to_uvec4( _ ) ( to( uvec4, _ ) )
#define size_uvec4 ( size_( uvec4 ) )
#define create_uvec4( x, y, z, w ) create( uvec4, x, y, z, w )

//

make_struct(quat) {
    f32 a;
    fvec3 v;
};

#define create_quat( a, v ) create( quat, a, v )

inl quat new_quat(in f32 angle, in fvec3 axis) {
    out create_quat(cosf( angle / 2.f ), fvec3_mul_f32( fvec3_norm( axis ), sinf( angle / 2.f ) ));
}

inl quat quat_mul(in quat a, in quat b) {
    out create_quat(
        ( a.a * b.a ) - fvec3_dot( a.v, b.v ),
        fvec3_add(
            fvec3_add(
                fvec3_mul_f32( b.v, a.a ),
                fvec3_mul_f32( a.v, b.a )
            ),
            fvec3_cross( a.v, b.v )
        )
    );
}

inl quat quat_mul_f32(in quat a, in f32 b) {
    out create_quat(a.a * b, fvec3_mul_f32( a.v, b ));
}

inl quat new_quat_look(in f32 pitch_x, in f32 roll_y, in f32 yaw_z) {
    out quat_mul(new_quat(pitch_x, create_fvec3(1, 0, 0)),
                 quat_mul(new_quat(roll_y, create_fvec3(0, 1, 0)), new_quat(yaw_z, create_fvec3(0, 0, 1))));
}

inl quat quat_conj(in quat q) {
    out create_quat(q.a, fvec3_neg( q.v ));
}

inl fvec3 fvec3_rot(in fvec3 v, in quat q) {
    out quat_mul(quat_mul(q, create_quat(0, v)), quat_conj(q)).v;
}

inl fvec3 quat_get_forward(in quat q) {
    out fvec3_norm(fvec3_rot(create_fvec3(0, 0, -1), quat_conj(q)));
}

inl fvec3 quat_get_right(in quat q) {
    out fvec3_norm(fvec3_rot(create_fvec3(1, 0, 0), quat_conj(q)));
}

inl fvec3 quat_get_up(in quat q) {
    out fvec3_norm(fvec3_rot(create_fvec3(0, 1, 0), quat_conj(q)));
}

//

make_struct(dual_quat) {
    quat r;
    quat d;
};

#define create_dual_quat( r, d ) create( dual_quat, r, d )

inl dual_quat new_dual_quat(in quat dir, in fvec3 pos) {
    out create_dual_quat(dir, quat_mul_f32( quat_mul( create_quat( 0, pos ), dir ), .5 ));
}

//

make_struct(dual_quat_proj) {
    dual_quat dq;
    fvec4 p;
};

#define create_dual_quat_proj( dq, p ) create( dual_quat_proj, dq, p )

inl dual_quat_proj new_dual_quat_proj(in quat dir, in fvec3 pos, f32 fov, f32 aspect, f32 near, f32 far) {
    f32 f, d;
    f = 1.0f / tanf(fov * 0.5f);
    d = 1.0f / (near - far);

    out create_dual_quat_proj(
        new_dual_quat( dir, pos ),
        // create_fvec4( f / aspect, f, ( near + far ) * d, near )
        create_fvec4( f / aspect, f, ( near + far ) * d, 2.0f * near * far * d )
    );
}

/// noise

constant fvec3 _grad_dirs[16] = {
    {1, 1, 0},
    {-1, 1, 0},
    {1, -1, 0},
    {-1, -1, 0},
    {1, 0, 1},
    {-1, 0, 1},
    {1, 0, -1},
    {-1, 0, -1},
    {0, 1, 1},
    {0, -1, 1},
    {0, 1, -1},
    {0, -1, -1},
    {1, 1, 0},
    {-1, 1, 0},
    {1, -1, 0},
    {-1, -1, 0}
};

fvec3 grad_dir(u32 h) {
    return _grad_dirs[h & 15];
}

float mix(float a, float b, float t) {
    return a * (1.0f - t) + b * t;
}

float interp(float v1, float v2, float v3, float v4, float v5, float v6, float v7, float v8, fvec3 t) {
    return mix(
        mix(mix(v1, v2, t.x), mix(v3, v4, t.x), t.y),
        mix(mix(v5, v6, t.x), mix(v7, v8, t.x), t.y),
        t.z
    );
}

fvec3 fade(fvec3 t) {
    return ( fvec3){
        t.x * t.x * t.x * (t.x * (t.x * 6.0f - 15.0f) + 10.0f),
        t.y * t.y * t.y * (t.y * (t.y * 6.0f - 15.0f) + 10.0f),
        t.z * t.z * t.z * (t.z * (t.z * 6.0f - 15.0f) + 10.0f)
    };
}

float dot(fvec3 a, fvec3 b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

float noise(fvec3 p) {
    fvec3 fl_p = {floor(p.x), floor(p.y), floor(p.z)};
    fvec3 fr_p = {p.x - fl_p.x, p.y - fl_p.y, p.z - fl_p.z};
    uvec3 c = {(u32)fl_p.x, (u32)fl_p.y, (u32)fl_p.z};

    return interp(
        dot(grad_dir(random_u32(c.x, c.y, c.z)), fr_p),
        dot(grad_dir(random_u32(c.x + 1, c.y, c.z)), create_fvec3(fr_p.x - 1, fr_p.y, fr_p.z)),
        dot(grad_dir(random_u32(c.x, c.y + 1, c.z)), create_fvec3(fr_p.x, fr_p.y - 1, fr_p.z)),
        dot(grad_dir(random_u32(c.x + 1, c.y + 1, c.z)), create_fvec3(fr_p.x - 1, fr_p.y - 1, fr_p.z)),
        dot(grad_dir(random_u32(c.x, c.y, c.z + 1)), create_fvec3(fr_p.x, fr_p.y, fr_p.z - 1)),
        dot(grad_dir(random_u32(c.x + 1, c.y, c.z + 1)), create_fvec3(fr_p.x - 1, fr_p.y, fr_p.z - 1)),
        dot(grad_dir(random_u32(c.x, c.y + 1, c.z + 1)), create_fvec3(fr_p.x, fr_p.y - 1, fr_p.z - 1)),
        dot(grad_dir(random_u32(c.x + 1, c.y + 1, c.z + 1)), create_fvec3(fr_p.x - 1, fr_p.y - 1, fr_p.z - 1)),
        fade(fr_p)
    );
}

float perlin(fvec3 p, int freq, int octa, float pers, float lacu) {
    float v = 0.0f, a = 1.0f, c_f = (float)freq;
    for (int i = 0; i < octa; i++) {
        v += noise(p) * a;
        a *= pers;
        c_f *= lacu;
        p.x *= lacu;
        p.y *= lacu;
        p.z *= lacu;
    }
    return v;
}

//

#endif
