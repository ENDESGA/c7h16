// // // // // // //
// > c7h16 _
// -------
// explicit and minimal esoteric c abstraction
// requires: math.h, string.h, standard library
// @ENDESGA 2023
// // // // // // //

/*
#include <c7h16.h>
needs to be the final include
otherwise it will be less compatible
-
treat as the final lens
*/

#pragma once
#ifndef C7H16_H
#define C7H16_H

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define OS_LINUX 0
#define OS_WINDOWS 0
#define OS_MACOS 0
#define OS_OTHER 0

#if defined( __WIN32__ ) || defined( WIN32 ) || defined( _WIN32 ) || defined( __CYGWIN__ ) || defined( __MINGW32__ ) || defined( __WINDOWS__ )
#undef OS_WINDOWS
#define OS_WINDOWS 1
#include <windows.h>
#undef near
#undef far
#include <intrin.h>
#elif defined( __LINUX__ ) || defined( linux ) || defined( __linux ) || defined( __linux__ )
#undef OS_LINUX
#define OS_LINUX 1
#include <immintrin.h>
#elif defined( __MACOSX__ ) || defined( __APPLE__ )
#undef OS_MACOS
#define OS_MACOS 1
#else
#undef OS_OTHER
#define OS_OTHER 1
#endif

//

#define CAT_LINE_SET( a, line ) $##a##_##line
#define CAT_LINE( a, line ) CAT_LINE_SET( a, line )
#define VAR_LINE( name ) CAT_LINE( name, __LINE__ )

#define DEF_START \
	do {
#define DEF_END \
	}             \
	spin( 0 )

//

#define once static
#define safe volatile
#define global safe once

#define make_type( _ ) typedef safe _
#define make_ptr( _ ) make_type( ptr( _ ) )
#define make_struct make_type( struct )
#define make_enum make_type( enum )
#define make_union make_type( union )

#define ptr( _ ) _*
#define val( _ ) *_
#define ref( _ ) _&
#define adr( _ ) &_

//

#define cast( type, _ ) val( ( ( ptr( type ) )( adr( _ ) ) ) )

#define pure void
#define pure_( _ ) ( ( pure )( _ ) )
#define size_pure sizeof( pure )
#define pure_ptr ptr( pure )
#define pure_ptr_( _ ) ( ( pure_ptr )( _ ) )
#define size_pure_ptr sizeof( pure_ptr )

make_type( uint8_t ) u8;
#define u8_( _ ) ( ( u8 )( _ ) )
#define size_u8 ( sizeof( u8 ) )
#define u8_min u8_( 0 )
#define u8_max u8_( UINT8_MAX )

make_type( uint16_t ) u16;
#define u16_( _ ) ( ( u16 )( _ ) )
#define size_u16 ( sizeof( u16 ) )
#define u16_min u16_( 0 )
#define u16_max u16_( UINT16_MAX )

make_type( uint32_t ) u32;
#define u32_( _ ) ( ( u32 )( _ ) )
#define size_u32 ( sizeof( u32 ) )
#define u32_min u32_( 0 )
#define u32_max u32_( UINT32_MAX )

make_type( uint64_t ) u64;
#define u64_( _ ) ( ( u64 )( _ ) )
#define size_u64 ( sizeof( u64 ) )
#define u64_min u64_( 0 )
#define u64_max u64_( UINT64_MAX )

make_type( int8_t ) s8;
#define s8_( _ ) ( ( s8 )( _ ) )
#define size_s8 ( sizeof( s8 ) )
#define s8_min s8_( INT8_MIN )
#define s8_max s8_( INT8_MAX )

make_type( int16_t ) s16;
#define s16_( _ ) ( ( s16 )( _ ) )
#define size_s16 ( sizeof( s16 ) )
#define s16_min s16_( INT16_MIN )
#define s16_max s16_( INT16_MAX )

make_type( int32_t ) s32;
#define s32_( _ ) ( ( s32 )( _ ) )
#define size_s32 ( sizeof( s32 ) )
#define s32_min s32_( INT32_MIN )
#define s32_max s32_( INT32_MAX )

make_type( int64_t ) s64;
#define s64_( _ ) ( ( s64 )( _ ) )
#define size_s64 ( sizeof( s64 ) )
#define s64_min s64_( INT64_MIN )
#define s64_max s64_( INT64_MAX )

make_type( float ) f32;
#define f32_( _ ) ( ( f32 )( _ ) )
#define size_f32 ( sizeof( f32 ) )

make_type( long double ) f64;
#define f64_( _ ) ( ( f64 )( _ ) )
#define size_f64 ( sizeof( f64 ) )

make_type( const ptr( char ) ) str;
#define str_( _ ) ( ( str )( _ ) )
#define size_str ( sizeof( str ) )
#define new_str( default_str, extra_char_mem ) memcpy( new_mem( char, strlen( default_str ) + ( extra_char_mem ) + 1 ), default_str, strlen( default_str ) )
#define str_len( str_to_get_length ) strlen( str_to_get_length )
#define str_add( str_base, str_to_add ) strcat_s( str_base, str_size( str_base ) + str_size( str_to_add ) + 1, str_to_add )
#define str_end( str_base ) str_add( str_base, "\0" )
#define delete_str( str_to_delete ) \
	if( str_to_delete == null ) free( str_to_delete )

//

make_struct
{
	u8 r, g, b, a;
}
rgba;
#define rgba_( _ ) ( ( rgba )( _ ) )
#define size_rgba ( sizeof( rgba ) )
#define make_rgba( r, g, b, a ) ( ( rgba ){ r, g, b, a } )

//

#define fn const global inline
#define in const safe
#define out return

#define fn_ptr( _ ) pure( val( _ ) )()
#define fn_ptr_( _ ) ( ( fn_ptr() )( _ ) )
#define fn_ptr_param1( _, _1 ) pure( val( _ ) )( _1 )
#define fn_ptr_param2( _, _1, _2 ) pure( val( _ ) )( _1, _2 )
#define fn_ptr_param3( _, _1, _2, _3 ) pure( val( _ ) )( _1, _2, _3 )
#define fn_ptr_param4( _, _1, _2, _3, _4 ) pure( val( _ ) )( _1, _2, _3, _4 )

//

#define sign( _ ) ( ( _ ) < 0 ? -1 : ( ( _ ) > 0 ? 1 : 0 ) )
#define avg( a, b ) ( ( ( a ) + ( b ) ) / 2. )

#define print printf
#define println() print( "\n" )

#define new_mem( type, n ) ( ( ptr( type ) )calloc( n, sizeof( type ) ) )
#define free_mem( _ ) free( _ )

//

#define ifn( _ ) if( !( _ ) )
#define elif( _ ) else if( _ )

#define and &&
#define or ||

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

#define null NULL
#define pi 3.14159265358979
#define pi_mul2 6.28318530717959
#define pi_mul3 9.42477796076937
#define pi_mul4 12.56637061435917
#define pi_mul8 25.13274122871834
#define pi_div2 1.57079632679489
#define pi_div3 1.04719755119659
#define pi_div4 0.78539816339744
#define pi_div8 0.39269908169872
#define pi_pow2 9.86960440108935
#define pi_pow3 31.00627668029982
#define pi_pow4 97.40909103400243
#define pi_pow8 9488.53101607057401
#define euler 2.71828182845904
#define e_mul2 5.43656365691809

//

make_struct
{
	s32 size, size_mem, size_type;
	ptr( u8 ) data;
}
struct_list;
make_ptr( struct_list ) list;

fn list $new_list( u32 size, u32 size_mem, u32 size_type, ptr( u8 ) data )
{
	list l = new_mem( struct_list, 1 );
	//
	l->size = size;
	l->size_mem = size_mem;
	l->size_type = size_type;
	l->data = data;
	//
	out l;
}
#define new_list( type ) $new_list( 0, 1, sizeof( type ), new_mem( u8, sizeof( type ) ) )
#define new_list_size( type, size ) $new_list( size, size, sizeof( type ), new_mem_ptr( type, size, ptr( u8 ) ) )
#define new_list_data( type, size, data ) $new_list( size, size, sizeof( type ), data )

#define list_alloc( l )                                                    \
	DEF_START                                                                \
	if( l->size == l->size_mem )                                             \
		{                                                                      \
			s32 new_size_mem = ( s32 )( l->size_mem << 1 );                      \
			pure_ptr new_data = realloc( l->data, new_size_mem * l->size_type ); \
			if( new_data == null )                                               \
				{                                                                  \
					break;                                                           \
				}                                                                  \
			l->size_mem = new_size_mem;                                          \
			l->data = new_data;                                                  \
		}                                                                      \
	DEF_END

#define list_set( l, type, pos, val ) ( ( ptr( type ) )( l->data ) )[ pos ] = val

#define list_add( l, type, val )       \
	DEF_START                            \
	list_alloc( l );                     \
	list_set( l, type, l->size++, val ); \
	DEF_END

#define list_insert( l, pos, val )                       \
	DEF_START                                              \
	list_alloc( l );                                       \
	memmove( ( l )->data + ( ( pos + 1 ) * l->size_type ), \
					 ( l )->data + ( pos * l->size_type ),         \
					 ( ( l )->size - ( pos ) ) * l->size_type );   \
	( ( ptr( typeof( val ) ) )( l->data ) )[ pos ] = val;  \
	++( l )->size;                                         \
	DEF_END

#define list_pop_back( l ) ( ( l )->size > 0 ? ( l )->data[ --( l )->size ] : null )

#define list_fill( l, val )                    \
	DEF_START                                    \
	to( l->size, n ) { ( l )->data[ n ] = val; } \
	DEF_END

#define list_free( l ) free( ( l )->data )

#define list_get( l, type, pos ) ( ( ptr( type ) )( l->data ) )[ pos ]