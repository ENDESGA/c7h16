// // // // // // //
// > c7h16_heptane _
// -------
// explicit and minimal esoteric c abstraction
// requires: math.h, standard library
// @ENDESGA 2023
// // // // // // //

/*
	#include "c7h16.h"
	needs to be the final include
	otherwise it will be less compatible
*/

#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>

#ifndef c7h16_heptane
#define c7h16_heptane

#define OS_LINUX 0
#define OS_WINDOWS 0
#define OS_MACOS 0
#define OS_OTHER 0

#if defined( __WIN32__ ) || defined( WIN32 ) || defined( _WIN32 ) || defined( __CYGWIN__ ) || defined( __MINGW32__ ) || defined( __WINDOWS__ )
	#undef OS_WINDOWS
	#define OS_WINDOWS 1
    #include <Windows.h>
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

#define ptr( _ ) _*
#define val( _ ) *_
#define ref( _ ) _&
#define adr( _ ) &_

#define pure void
#define once static
#define safe volatile
#define global safe once
#define fn const global inline ///////////////////
#define in const global
#define out return
#define make_type( _ ) typedef safe _
#define make_struct make_type( struct )
#define make_enum make_type( enum )
#define make_union make_type( union )
#define make_ptr( _ ) make_type( ptr( _ ) )
#define print printf
#define println() print("\n")
#define null NULL
#define spin( _ ) while( _ )
#define loop spin( 1 )
#define fn_ptr( _ ) pure( val(_) )()
#define fn_ptr_( _ ) ((fn_ptr())( _ ))
#define fn_ptr_param1( _, _1 ) pure ( * _ )( _1 )
#define fn_ptr_param2( _, _1, _2 ) pure ( * _ )( _1, _2 )
#define fn_ptr_param3( _, _1, _2, _3 ) pure ( * _ )( _1, _2, _3 )
#define fn_ptr_param4( _, _1, _2, _3, _4 ) pure ( * _ )( _1, _2, _3, _4 )
#define pure_ptr ptr( pure )
#define pure_ptr_( _ ) (( pure_ptr )( _ ))
#define cast( type, value ) val(((ptr(type))(adr(value))))
#define do_once once u8 VAR_LINE(o) = 1; if((VAR_LINE(o) == 1 ? VAR_LINE(o)-- : 0))

#define and &&
#define or ||
#define mod %

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

make_type( uint8_t ) u8;
#define u8_( _ ) (( u8 )( _ ))
#define u8_size (sizeof(u8))
#define u8_min u8_(0)
#define u8_max u8_(UINT8_MAX)

make_type( uint16_t ) u16;
#define u16_( _ ) (( u16 )( _ ))
#define u16_size (sizeof(u16))
#define u16_min u16_(0)
#define u16_max u16_(UINT16_MAX)

make_type( uint32_t ) u32;
#define u32_( _ ) (( u32 )( _ ))
#define u32_size (sizeof(u32))
#define u32_min u32_(0)
#define u32_max u32_(UINT32_MAX)

make_type( uint64_t ) u64;
#define u64_( _ ) (( u64 )( _ ))
#define u64_size (sizeof(u64))
#define u64_min u64_(0)
#define u64_max u64_(UINT64_MAX)

make_type( int8_t ) s8;
#define s8_( _ ) (( s8 )( _ ))
#define s8_size (sizeof(s8))
#define s8_min s8_(INT8_MIN)
#define s8_max s8_(INT8_MAX)

make_type( int16_t ) s16;
#define s16_( _ ) (( s16 )( _ ))
#define s16_size (sizeof(s16))
#define s16_min s16_(INT16_MIN)
#define s16_max s16_(INT16_MAX)

make_type( int32_t ) s32;
#define s32_( _ ) (( s32 )( _ ))
#define s32_size (sizeof(s32))
#define s32_min s32_(INT32_MIN)
#define s32_max s32_(INT32_MAX)

make_type( int64_t ) s64;
#define s64_( _ ) (( s64 )( _ ))
#define s64_size (sizeof(s64))
#define s64_min s64_(INT64_MIN)
#define s64_max s64_(INT64_MAX)

make_type( float ) f32;
#define f32_( _ ) (( f32 )( _ ))
#define f32_size (sizeof(f32))

make_type( long double ) f64;
#define f64_( _ ) (( f64 )( _ ))
#define f64_size (sizeof(f64))

make_type( ptr( char )) str;

make_union{
	s8 s;
	u8 u;
} safe_8;

make_union{
	s16 s;
	u16 u;
} safe_16;

make_union{
	s32 s;
	u32 u;
	f32 f;
} safe_32;

make_union{
	s64 s;
	u64 u;
	f64 f;
} safe_64;

#define safe_s8_ptr_set( safe_ptr, set_value ) _InterlockedExchange8((safe ptr(s8))(safe_ptr), (set_value))
#define safe_s16_ptr_set( safe_ptr, set_value ) _InterlockedExchange16((safe ptr(s16))(safe_ptr), (set_value))
#define safe_s32_ptr_set( safe_ptr, set_value ) _InterlockedExchange((safe ptr(s32))(safe_ptr), (set_value))
#define safe_s64_ptr_set( safe_ptr, set_value ) _InterlockedExchange64((safe ptr(s64))(safe_ptr), (set_value))
#define safe_s8_ptr_get( safe_ptr ) _InterlockedOr8((safe ptr(s8))(safe_ptr), 0)
#define safe_s16_ptr_get( safe_ptr ) _InterlockedOr16((safe ptr(s16))(safe_ptr), 0)
#define safe_s32_ptr_get( safe_ptr ) _InterlockedOr((safe ptr(s32))(safe_ptr), 0)
#define safe_s64_ptr_get( safe_ptr ) _InterlockedOr64((safe ptr(s64))(safe_ptr), 0)
#define safe_s8_set( safe_var, set_value ) safe_s8_ptr_set(adr(safe_var), set_value)
#define safe_s16_set( safe_var, set_value ) safe_s16_ptr_set(adr(safe_var), set_value)
#define safe_s32_set( safe_var, set_value ) safe_s32_ptr_set(adr(safe_var), set_value)
#define safe_s64_set( safe_var, set_value ) safe_s64_ptr_set(adr(safe_var), set_value)
#define safe_s8_get( safe_var ) safe_s8_ptr_get(adr(safe_var))
#define safe_s16_get( safe_var ) safe_s16_ptr_get(adr(safe_var))
#define safe_s32_get( safe_var ) safe_s32_ptr_get(adr(safe_var))
#define safe_s64_get( safe_var ) safe_s64_ptr_get(adr(safe_var))

#define safe_u8_ptr_set( safe_ptr, set_value ) safe_s8_ptr_set(safe_ptr, ((safe safe_8){.u = (set_value)}).s)
#define safe_u16_ptr_set( safe_ptr, set_value ) safe_s16_ptr_set(safe_ptr, ((safe safe_16){.u = (set_value)}).s)
#define safe_u32_ptr_set( safe_ptr, set_value ) safe_s32_ptr_set(safe_ptr, ((safe safe_32){.u = (set_value)}).s)
#define safe_u64_ptr_set( safe_ptr, set_value ) safe_s64_ptr_set(safe_ptr, ((safe safe_64){.u = (set_value)}).s)
#define safe_u8_ptr_get( safe_ptr ) ((safe safe_8){.s = safe_s8_ptr_get( safe_ptr )}.u)
#define safe_u16_ptr_get( safe_ptr ) ((safe safe_16){.s = safe_s16_ptr_get( safe_ptr )}.u)
#define safe_u32_ptr_get( safe_ptr ) ((safe safe_32){.s = safe_s32_ptr_get( safe_ptr )}.u)
#define safe_u64_ptr_get( safe_ptr ) ((safe safe_64){.s = safe_s64_ptr_get( safe_ptr )}.u)
#define safe_u8_set( safe_var, set_value ) safe_s8_set(safe_var, ((safe safe_8){.u = (set_value)}).s)
#define safe_u16_set( safe_var, set_value ) safe_s16_set(safe_var, ((safe safe_16){.u = (set_value)}).s)
#define safe_u32_set( safe_var, set_value ) safe_s32_set(safe_var, ((safe safe_32){.u = (set_value)}).s)
#define safe_u64_set( safe_var, set_value ) safe_s64_set(safe_var, ((safe safe_64){.u = (set_value)}).s)
#define safe_u8_get( safe_var ) ((safe safe_8){.s = safe_s8_get( safe_var )}.u)
#define safe_u16_get( safe_var ) ((safe safe_16){.s = safe_s16_get( safe_var )}.u)
#define safe_u32_get( safe_var ) ((safe safe_32){.s = safe_s32_get( safe_var )}.u)
#define safe_u64_get( safe_var ) ((safe safe_64){.s = safe_s64_get( safe_var )}.u)

#define safe_f32_ptr_set( safe_ptr, set_value ) safe_s32_ptr_set(safe_ptr, ((safe safe_32){.f = (set_value)}).s)
#define safe_f64_ptr_set( safe_ptr, set_value ) safe_s64_ptr_set(safe_ptr, ((safe safe_64){.f = (set_value)}).s)
#define safe_f32_ptr_get( safe_ptr ) ((safe safe_32){.s = safe_s32_ptr_get( safe_ptr )}.f)
#define safe_f64_ptr_get( safe_ptr ) ((safe safe_64){.s = safe_s64_ptr_get( safe_ptr )}.f)
#define safe_f32_set( safe_var, set_value ) safe_s32_set(safe_var, ((safe safe_32){.f = (set_value)}).s)
#define safe_f64_set( safe_var, set_value ) safe_s64_set(safe_var, ((safe safe_64){.f = (set_value)}).s)
#define safe_f32_get( safe_var ) ((safe safe_32){.s = safe_s32_get( safe_var )}.f)
#define safe_f64_get( safe_var ) ((safe safe_64){.s = safe_s64_get( safe_var )}.f)

#define safe_ptr_set( safe_ptr, set_value ) _InterlockedExchangePointer(adr(safe_ptr),pure_ptr_(set_value))
#define safe_ptr_get( safe_ptr ) _InterlockedCompareExchangePointer(adr(safe_ptr), NULL, NULL)

make_type(safe s8) spinlock;

#define spinlock_engage( lock ) spin(safe_s8_set( lock, 1)) // {SDL_CPUPauseInstruction();}
#define spinlock_vacate( lock ) safe_s8_set( lock, 0 )

//

make_struct{
	u8
		r, g, b, a;
} rgba;
#define rgba_( _ ) ((rgba)( _ ))
#define make_rgba( r, g, b, a ) (rgba){ r, g, b, a }

//

#define avg( a, b ) (((a) + (b))/2.)

#define new_str( default_str, char_buffer ) memcpy(new_mem(char,strlen(default_str) + (char_buffer) + 1), default_str, strlen(default_str))
#define str_size( str_to_get_size ) strlen( str_to_get_size )
#define str_add( str_base, str_to_add ) strcat_s(str_base, str_size(str_base) + str_size(str_to_add) + 1, str_to_add)
#define str_end( str_base ) str_add(str_base, "\0")
#define delete_str( str_to_delete ) if(str_to_delete == null) free(str_to_delete)

#define new_mem( type, n ) (( ptr( type ) )calloc( n, sizeof( type ) ))
#define new_mem_ptr( type, n, ptr_type ) (( ptr_type )calloc( n, sizeof( type ) ))

#define CAT_LINE_SET( a, line ) $##a##_##line
#define CAT_LINE( a, line ) CAT_LINE_SET( a, line )
#define VAR_LINE( name ) CAT_LINE( name, __LINE__ )

#define iter( to_n, var ) register s32 VAR_LINE( to ) = (to_n); if(VAR_LINE( to )) for( register s32 var = 0; var < VAR_LINE( to ); var++ )

#define rep( n ) register u32 VAR_LINE( rep ) = n; for( register u32 VAR_LINE( LOOP ) = 0; VAR_LINE( LOOP ) < VAR_LINE( rep ); ++VAR_LINE( LOOP ) )

#define ifn( _ ) if( !( _ ) )

#define avg( a, b ) (((a) + (b))/2.)

#define array_size( array ) (sizeof(array) / sizeof(array[0]))

//

#define sign( _ ) ( ( _ ) < 0 ? -1 : ( ( _ ) > 0 ? 1 : 0) )

//

make_struct{
	spinlock
		lock;
	s32
		size,
		size_mem,
		size_type;
	ptr( u8 )
		data;
} list_struct;
make_ptr( list_struct ) list;

fn list $new_list( u32 size, u32 size_mem, u32 size_type, ptr( u8 ) data ) {
	list l = new_mem( list_struct, 1 );

	l->size = size;
	l->size_mem = size_mem;
	l->size_type = size_type;
	l->data = data;

	out l;
}
#define new_list( type ) $new_list( 0, 1, sizeof( type ), new_mem( u8, sizeof( type ) ) )
#define new_list_size( type, size ) $new_list( size, size, sizeof( type ), new_mem_ptr( type, size, ptr( u8 ) ) )
#define new_list_data( type, size, data ) $new_list( size, size, sizeof( type ), data )

#define list_alloc( l ) \
    do { \
        if (l->size == l->size_mem) { \
            s32 new_size_mem = (s32)(l->size_mem << 1); \
            pure_ptr new_data = realloc(l->data, new_size_mem * l->size_type); \
            if (new_data == null) { \
                break; \
            } \
            l->size_mem = new_size_mem; \
            l->data = new_data; \
        } \
    } spin(0)

#define list_set( l, type, pos, val ) ((ptr(type))(l->data))[pos] = val

#define list_add( l, type, val ) \
    do { \
        list_alloc(l); \
		list_set( l, type, l->size++, val ); \
    } spin(0)

#define list_insert( l, pos, val ) \
    do { \
        list_alloc(l); \
        memmove((l)->data + ((pos + 1) * l->size_type), (l)->data + (pos * l->size_type), ((l)->size - (pos)) * l->size_type); \
        ((ptr(typeof(val)))(l->data))[pos] = val; \
        ++(l)->size; \
    } spin(0)

#define list_pop_back( l ) \
    ((l)->size > 0 ? (l)->data[--(l)->size] : null)

#define list_fill( l, val ) \
    do { \
        to(l->size, n) { \
            (l)->data[n] = val; \
        } \
    } spin(0)

#define list_free( l ) free((l)->data)

#define list_get( l, type, pos ) ( ( ptr( type ) )( l->data ) )[ pos ]

//

make_struct{
	spinlock
		lock;
	u32
		pos_prev;
	list
		id_list, free_list;
} pool_struct;
make_ptr(pool_struct) pool;

fn pool $new_pool( list il) {
	pool p = new_mem(pool_struct, 1);

	p->pos_prev = 0;
	p->id_list = il;
	p->free_list = new_list(u32);

	out p;
}
#define new_pool( type ) $new_pool(new_list(type))

#define pool_add( p, type, val ) \
    do { \
		if(p->free_list->size) { \
			p->pos_prev = list_pop_back(p->free_list); \
			list_set( p->id_list, type, p->pos_prev, val );\
		} else { \
			p->pos_prev = p->id_list->size; \
			list_alloc(p->id_list); \
			list_set( p->id_list, type, p->id_list->size++, val ); \
		} \
    } spin(0)

#define pool_free( p, type, pos ) \
	do { \
		list_add(p->free_list, u32, pos); \
		list_set(p->id_list, type, pos, null); \
	} spin(0)

//

#define list_lock( list_to_lock_for_access ) spinlock_engage(list_to_lock_for_access->lock)
#define list_unlock( list_to_unlock ) spinlock_vacate(list_to_unlock->lock)

#define pool_lock( pool_to_lock_for_access ) spinlock_engage(pool_to_lock_for_access->lock)
#define pool_unlock( pool_to_unlock ) spinlock_vacate(pool_to_unlock->lock)

//

make_struct {
	f32
		r, d;
} dual;

#define dual_( _ ) ((dual)( _ ))
#define make_dual( r, d ) dual{ r, d }

//

make_struct {
	f32
		x, y;
} vec2_struct;
#define $vec2_( _ ) ((vec2_struct)( _ ))
#define $make_vec2_struct( x, y ) (vec2_struct){ x, y }

make_struct{
	f32
		x, y, z;
} vec3_struct;
#define $vec3_( _ ) ((vec3_struct)( _ ))
#define $make_vec3_struct( x, y, z ) (vec3_struct){ x, y, z }

make_struct{
	f32
		x, y, z, w;
} vec4_struct;
#define $vec4_( _ ) ((vec4_struct)( _ ))
#define $make_vec4_struct( x, y, z, w ) (vec4_struct){ x, y, z, w }

make_struct{
	s32
		x, y;
} ivec2_struct;
#define $ivec2_( _ ) ((ivec2_struct)( _ ))
#define $make_ivec2_struct( x, y ) (ivec2_struct){ x, y }

make_struct{
	s32
		x, y, z;
} ivec3_struct;
#define $ivec3_( _ ) ((ivec3_struct)( _ ))
#define $make_ivec3_struct( x, y, z ) (ivec3_struct){ x, y, z }

make_struct{
	s32
		x, y, z, w;
} ivec4_struct;
#define $ivec4_( _ ) ((ivec4_struct)( _ ))
#define $make_ivec4_struct( x, y, z, w ) (ivec4_struct){ x, y, z, w }

make_struct{
	u32
		x, y;
} uvec2_struct;
#define $uvec2_( _ ) ((uvec2_struct)( _ ))
#define $make_uvec2_struct( x, y ) (uvec2_struct){ x, y }

make_struct{
	u32
		x, y, z;
} uvec3_struct;
#define $uvec3_( _ ) ((uvec3_struct)( _ ))
#define $make_uvec3_struct( x, y, z ) (uvec3_struct){ x, y, z }

make_struct{
	u32
		x, y, z, w;
} uvec4_struct;
#define $uvec4_( _ ) ((uvec4_struct)( _ ))
#define $make_uvec4_struct( x, y, z, w ) (uvec4_struct){ x, y, z, w }

typedef __m128 vec;

#define vec_( _ ) ((vec)( _ ))
#define make_vec( x, y, z ) _mm_set_ps(0, z, y, x)

#define vec_x( v ) (val((vec4_struct*)((pure_ptr)(&v)))).x
#define vec_y( v ) (val((vec4_struct*)((pure_ptr)(&v)))).y
#define vec_z( v ) (val((vec4_struct*)((pure_ptr)(&v)))).z
#define vec_w( v ) (val((vec4_struct*)((pure_ptr)(&v)))).z

fn vec vec_floor( vec v ) {
	out _mm_floor_ps( v );
}

fn vec vec_round( vec v ) {
	out _mm_round_ps( v, _MM_FROUND_TO_NEAREST_INT );
}

fn vec vec_neg( vec v ) {
	out _mm_xor_ps( v, _mm_set1_ps( -.0f ));
}

fn vec vec_add( vec a, vec b ) {
	out _mm_add_ps( a, b );
}

fn vec vec_sub( vec a, vec b ) {
	out _mm_sub_ps( a, b );
}

fn vec vec_sub_f32( vec a, f32 b ) {
	out _mm_sub_ps( a, _mm_set1_ps( b ));
}

fn vec vec_mul( vec a, vec b ) {
	out _mm_mul_ps( a, b );
}

fn vec vec_mul_f32( vec a, f32 b ) {
	out _mm_mul_ps( a, _mm_set1_ps( b ));
}

fn vec vec_div( vec a, vec b ) {
	out _mm_div_ps( a, b );
}

fn vec vec_div_f32( vec a, f32 b ) {
	out _mm_div_ps( a, _mm_set1_ps( b ));
}

fn vec vec_dot( vec a, vec b ) {
	out _mm_dp_ps( a, b, 255 );
}

fn vec vec_dot2( vec v ) {
	out vec_dot( v, v );
}

fn vec vec_len( vec v ) {
	out _mm_sqrt_ps( vec_dot2( v ));
}

fn vec vec_norm( vec v ) {
	out vec_div(v, vec_len(v));
}

fn vec vec_norm_fast( vec v ) {
	out _mm_mul_ps(v, _mm_rsqrt_ps(vec_dot2(v)));
}

fn vec vec_cross( vec a, vec b ) {
	vec shuf1 = _mm_shuffle_ps( a, a, _MM_SHUFFLE( 3, 0, 2, 1 ));
	vec shuf2 = _mm_shuffle_ps( b, b, _MM_SHUFFLE( 3, 1, 0, 2 ));
	vec mul1 = _mm_mul_ps( shuf1, shuf2 );
	
	shuf1 = _mm_shuffle_ps( a, a, _MM_SHUFFLE( 3, 1, 0, 2 ));
	shuf2 = _mm_shuffle_ps( b, b, _MM_SHUFFLE( 3, 0, 2, 1 ));
	vec mul2 = _mm_mul_ps( shuf1, shuf2 );
	out _mm_sub_ps( mul1, mul2 );
}

fn vec vec_avg( vec a, vec b ) {
	out vec_mul_f32( vec_add( a, b ), .5f );
}

//

make_struct {
	f32 a;
	vec v;
} quat;

#define quat_( _ ) ((quat)( _ ))
#define make_quat( a, v ) (quat){ a, v }

fn quat quat_conj( quat q ) {
	out make_quat(q.a, vec_neg(q.v));
}

fn quat quat_neg( quat q ) {
	out make_quat( -q.a, vec_neg( q.v ));
}

fn f32 norm_squared( quat q ) {
	once vec temp_vec;
	temp_vec = vec_dot( q.v, q.v );
	out q.a * q.a + vec_x( temp_vec );
}

fn quat quat_add( quat a, quat b ) {
	out make_quat( a.a + b.a, vec_add( a.v, b.v ));
}

fn quat quat_mul( quat a, quat b ) {
	once vec temp_vec;
	temp_vec = vec_dot( a.v, b.v );
	out make_quat(( a.a * b.a ) - vec_x( temp_vec ), vec_add( vec_add( vec_mul_f32( b.v, a.a ), vec_mul_f32( a.v, b.a )), vec_cross( a.v, b.v )));
}

#define quat_mul3( a, b, c ) quat_mul((a), quat_mul((b), (c)))

fn quat quat_mul_f32( quat a, f32 b ) {
	out make_quat( a.a * b, vec_mul_f32( a.v, b ));
}

fn quat quat_div_f32( quat a, f32 b ) {
	out make_quat( a.a / b, vec_div_f32( a.v, b ));
}

fn quat quat_inverse( quat q ) {
	out quat_div_f32( quat_conj( q ), norm_squared( q ));
}

fn quat quat_norm(quat q) {
	out quat_div_f32(q, sqrtf(norm_squared(q)));
}

fn vec vec_rot( vec v, quat q ) {
	out quat_mul( quat_mul( q, make_quat( 0, v ) ), quat_conj( q )).v;
}

fn vec quat_get_forward( quat q ) {
	out vec_norm( vec_rot( make_vec( 0, 0, -1 ), quat_conj( q )));
}

fn vec quat_get_right( quat q ) {
	out vec_norm( vec_rot( make_vec( 1, 0, 0 ), quat_conj( q )));
}

fn vec quat_get_up( quat q ) {
	out vec_norm( vec_rot( make_vec( 0, 1, 0 ), quat_conj( q )));
}

fn quat new_quat(f32 angle, vec axis) {
	out make_quat(cosf(angle / 2.f), vec_mul_f32(vec_norm(axis), sinf(angle / 2.f)));
}

fn quat new_quat_look( f32 pitch_x, f32 roll_y, f32 yaw_z ) {
	out quat_mul3( new_quat( pitch_x, make_vec( 1, 0, 0 )), new_quat( roll_y, make_vec( 0, 1, 0 )), new_quat( yaw_z, make_vec( 0, 0, 1 )) );
}

fn quat quat_avg(quat a, quat b) {
	out quat_mul_f32(quat_add(a, b), .5f);
}

//

make_struct {
	quat
		r, d;
} dual_quat;
#define dual_quat_( _ ) ((dual_quat)( _ ))
#define make_dual_quat( r, d ) (dual_quat){ r, d }

make_struct {
	struct { f32 rx, ry, rz, rw; };
	struct { f32 dx, dy, dz, dw; };
} dual_quat_struct;
#define make_dual_quat_struct( rx, ry, rz, rw, dx, dy, dz, dw ) (dual_quat_struct){ {rx, ry, rz, rw},{dx, dy, dz, dw} }

fn dual_quat new_dual_quat( quat dir, vec pos ) {
	out make_dual_quat( dir, quat_mul_f32( quat_mul( make_quat( 0, pos ), dir ), .5 ));
}

fn dual_quat dual_quat_mul( dual_quat a, dual_quat b ) {
	out make_dual_quat( quat_mul( a.r, b.r ), quat_add( quat_mul( a.r, b.d ), quat_mul( a.d, b.r )));
}

fn vec dual_quat_trans( dual_quat dq, vec p ) {
	out dual_quat_mul( dual_quat_mul( dq, make_dual_quat( make_quat( 1., make_vec( 0., 0., 0. )), make_quat( 0., p )) ), make_dual_quat( quat_conj( dq.r ), quat_neg( quat_conj( dq.d ))) ).d.v;
}

//

make_struct {
	dual_quat
		dq;
	vec4_struct
		proj;
} dual_quat_proj;
#define dual_quat_proj_( _ ) ((dual_quat_proj)( _ ))
#define new_dual_quat_proj( dq, p ) (dual_quat_proj){ dq, p }

make_struct {
	struct { f32 rx, ry, rz, rw; };
	struct { f32 dx, dy, dz, dw; };
	struct { f32 px, py, pz, pw; };
} $dual_quat_proj;
#define $new_dual_quat_proj( rx, ry, rz, rw, dx, dy, dz, dw, px, py, pz, pw ) ($dual_quat_proj){ {rx, ry, rz, rw}, {dx, dy, dz, dw}, {px, py, pz, pw} }

fn dual_quat_proj dual_quat_proj_set( quat dir, vec pos, f32 fov, f32 aspect, f32 near, f32 far ) {
	f32 f, d;
	f = 1.0f / tanf( fov * 0.5f );
	d = 1.0f / ( near - far );
	out new_dual_quat_proj( new_dual_quat( dir, pos ), $make_vec4_struct( f / aspect, f, ( near + far ) * d, near ));
	//out new_dual_quat_proj( new_dual_quat( dir, pos ), $make_vec4_struct( f / aspect, f, ( near + far ) * d, 2.0f * near * far * d ));
}

//

make_struct {
	vec4_struct
		a, b, c, d;
} matrix;
#define matrix_( _ ) ((matrix)( _ ))
#define new_matrix( a, b, c, d ) (matrix){ a, b, c, d }

//

str dec_to_str[] = {
	"0",
	"1",
	"2",
	"3",
	"4",
	"5",
	"6",
	"7",
	"8",
	"9"
};

#endif

//

// public domain zlib decode    v0.2  Sean Barrett 2006-11-18
//    simple implementation
//      - all input must be provided in an upfront buffer
//      - all output is written to a single output buffer (can malloc/realloc)
//    performance
//      - fast huffman

#ifndef STB_NO_ZLIB

#define stb_inline inline
#define STB_NOTUSED(v)  (void)(v)

#define STB_ASSERT(x) //SDL_assert(x)
#define STB_MALLOC(sz)           malloc(sz)
#define STB_REALLOC(p,newsz)     realloc(p,newsz)
#define STB_REALLOC_SIZED(p,oldsz,newsz) STB_REALLOC(p,newsz)
#define STB_FREE(p)              free(p)

#define STB_MEMMOVE(a,b,sz) memmove(a,b,sz)

const char *stb__g_failure_reason;
const char *stb_failure_reason(void)
{
   return stb__g_failure_reason;
}

static int stb__err(const char *str)
{
   stb__g_failure_reason = str;
   return 0;
}

static void *stb__malloc(size_t size)
{
    return STB_MALLOC(size);
}

typedef unsigned char stb_uc;
typedef unsigned short stb_us;
typedef uint16_t stb__uint16;
typedef int16_t  stb__int16;
typedef uint32_t stb__uint32;
typedef int32_t  stb__int32;

#define STB_UCHAR(x) (unsigned char) ((x) & 0xff)

// fast-way is faster to check than jpeg huffman, but slow way is slower
#define STB__ZFAST_BITS  9 // accelerate all cases in default tables
#define STB__ZFAST_MASK  ((1 << STB__ZFAST_BITS) - 1)
#define STB__ZNSYMS 288 // number of symbols in literal/length alphabet

// zlib-style huffman encoding
// (jpegs packs from left, zlib from right, so can't share code)
typedef struct
{
   stb__uint16 fast[1 << STB__ZFAST_BITS];
   stb__uint16 firstcode[16];
   int maxcode[17];
   stb__uint16 firstsymbol[16];
   stb_uc  size[STB__ZNSYMS];
   stb__uint16 value[STB__ZNSYMS];
} stb__zhuffman;

stb_inline static int stb__bitreverse16(int n)
{
  n = ((n & 0xAAAA) >>  1) | ((n & 0x5555) << 1);
  n = ((n & 0xCCCC) >>  2) | ((n & 0x3333) << 2);
  n = ((n & 0xF0F0) >>  4) | ((n & 0x0F0F) << 4);
  n = ((n & 0xFF00) >>  8) | ((n & 0x00FF) << 8);
  return n;
}

stb_inline static int stb__bit_reverse(int v, int bits)
{
   STB_ASSERT(bits <= 16);
   return stb__bitreverse16(v) >> (16-bits);
}

static int stb__zbuild_huffman(stb__zhuffman *z, const stb_uc *sizelist, int num)
{
   int i, k = 0, code, next_code[16], sizes[17] = {0};

   memset(z->fast, 0, sizeof(z->fast));

   for (i = 0; i < num; ++i)
      ++sizes[sizelist[i]];

   for (code = 0, i = 1; i < 16; ++i) {
      if (sizes[i] > (1 << i))
         return stb__err("bad sizes", "Corrupt PNG");

      next_code[i] = code;
      z->firstcode[i] = (stb__uint16) code;
      z->firstsymbol[i] = (stb__uint16) k;

      code += sizes[i];
      if (sizes[i] && (code - 1 >= (1 << i)))
         return stb__err("bad codelengths", "Corrupt PNG");

      z->maxcode[i] = code << (16 - i);
      code <<= 1;
      k += sizes[i];
   }
   z->maxcode[16] = 0x10000;

   for (i = 0; i < num; ++i) {
      int s = sizelist[i];
      if (s) {
         int c = next_code[s] - z->firstcode[s] + z->firstsymbol[s];
         stb__uint16 fastv = (stb__uint16)((s << 9) | i);
         z->size[c] = (stb_uc)s;
         z->value[c] = (stb__uint16)i;

         if (s <= STB__ZFAST_BITS) {
            int j = stb__bit_reverse(next_code[s], s);
            int end = (1 << STB__ZFAST_BITS);
            for (; j < end; j += (1 << s)) {
               z->fast[j] = fastv;
            }
         }
         ++next_code[s];
      }
   }
   return 1;
}



typedef struct
{
   stb_uc *zbuffer, *zbuffer_end;
   int num_bits;
   stb__uint32 code_buffer;

   char *zout;
   char *zout_start;
   char *zout_end;
   int   z_expandable;

   stb__zhuffman z_length, z_distance;
} stb__zbuf;

stb_inline static int stb__zeof(stb__zbuf *z)
{
   return (z->zbuffer >= z->zbuffer_end);
}

stb_inline static stb_uc stb__zget8(stb__zbuf *z)
{
   return stb__zeof(z) ? 0 : *z->zbuffer++;
}

static void stb__fill_bits(stb__zbuf *z)
{
   do {
      if (z->code_buffer >= (1U << z->num_bits)) {
        z->zbuffer = z->zbuffer_end;  /* treat this as EOF so we fail. */
        return;
      }
      z->code_buffer |= (unsigned int) stb__zget8(z) << z->num_bits;
      z->num_bits += 8;
   } while (z->num_bits <= 24);
}

stb_inline static unsigned int stb__zreceive(stb__zbuf *z, int n)
{
   unsigned int k;
   if (z->num_bits < n) stb__fill_bits(z);
   k = z->code_buffer & ((1 << n) - 1);
   z->code_buffer >>= n;
   z->num_bits -= n;
   return k;
}

static int stb__zhuffman_decode_slowpath(stb__zbuf *a, stb__zhuffman *z)
{
   int b,s,k;
   // not resolved by fast table, so compute it the slow way
   // use jpeg approach, which requires MSbits at top
   k = stb__bit_reverse(a->code_buffer, 16);
   for (s=STB__ZFAST_BITS+1; ; ++s)
      if (k < z->maxcode[s])
         break;
   if (s >= 16) return -1; // invalid code!
   // code size is s, so:
   b = (k >> (16-s)) - z->firstcode[s] + z->firstsymbol[s];
   if (b >= STB__ZNSYMS) return -1; // some data was corrupt somewhere!
   if (z->size[b] != s) return -1;  // was originally an assert, but report failure instead.
   a->code_buffer >>= s;
   a->num_bits -= s;
   return z->value[b];
}

stb_inline static int stb__zhuffman_decode(stb__zbuf *a, stb__zhuffman *z)
{
   int b,s;
   if (a->num_bits < 16) {
      if (stb__zeof(a)) {
         return -1;   /* report error for unexpected end of data. */
      }
      stb__fill_bits(a);
   }
   b = z->fast[a->code_buffer & STB__ZFAST_MASK];
   if (b) {
      s = b >> 9;
      a->code_buffer >>= s;
      a->num_bits -= s;
      return b & 511;
   }
   return stb__zhuffman_decode_slowpath(a, z);
}

static const int stb__zlength_base[31] = {
   3,4,5,6,7,8,9,10,11,13,
   15,17,19,23,27,31,35,43,51,59,
   67,83,99,115,131,163,195,227,258,0,0 };

static const int stb__zlength_extra[31]=
{ 0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0,0,0 };

static const int stb__zdist_base[32] = { 1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,
257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577,0,0};

static const int stb__zdist_extra[32] =
{ 0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13};

static int stb__parse_huffman_block(stb__zbuf *a)
{
   char *zout = a->zout;
   for(;;) {
      int z = stb__zhuffman_decode(a, &a->z_length);
      if (z < 256) {
         if (z < 0) return stb__err("bad huffman code","Corrupt PNG"); // error in huffman codes
         *zout++ = (char) z;
      } else {
         stb_uc *p;
         int len,dist;
         if (z == 256) {
            a->zout = zout;
            return 1;
         }
         if (z >= 286) return stb__err("bad huffman code","Corrupt PNG"); // per DEFLATE, length codes 286 and 287 must not appear in compressed data
         z -= 257;
         len = stb__zlength_base[z];
         if (stb__zlength_extra[z]) len += stb__zreceive(a, stb__zlength_extra[z]);
         z = stb__zhuffman_decode(a, &a->z_distance);
         if (z < 0 || z >= 30) return stb__err("bad huffman code","Corrupt PNG"); // per DEFLATE, distance codes 30 and 31 must not appear in compressed data
         dist = stb__zdist_base[z];
		          if (stb__zdist_extra[z]) dist += stb__zreceive(a, stb__zdist_extra[z]);
         if (zout - a->zout_start < dist) return stb__err("bad dist","Corrupt PNG");
         p = (stb_uc *) (zout - dist);
         if (dist == 1) { // run of one byte; common in images.
            stb_uc v = *p;
            if (len) { do *zout++ = v; while (--len); }
         } else {
            if (len) { do *zout++ = *p++; while (--len); }
         }
      }
   }
}

static int stb__compute_huffman_codes(stb__zbuf *a)
{
   static const stb_uc length_dezigzag[19] = { 16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15 };
   stb__zhuffman z_codelength;
   stb_uc lencodes[286+32+137];//padding for maximum single op
   stb_uc codelength_sizes[19];
   int i,n;

   int hlit  = stb__zreceive(a,5) + 257;
   int hdist = stb__zreceive(a,5) + 1;
   int hclen = stb__zreceive(a,4) + 4;
   int ntot  = hlit + hdist;

   memset(codelength_sizes, 0, sizeof(codelength_sizes));
   for (i=0; i < hclen; ++i) {
      int s = stb__zreceive(a,3);
      codelength_sizes[length_dezigzag[i]] = (stb_uc) s;
   }
   if (!stb__zbuild_huffman(&z_codelength, codelength_sizes, 19)) return 0;

   n = 0;
   while (n < ntot) {
      int c = stb__zhuffman_decode(a, &z_codelength);
      if (c < 0 || c >= 19) return stb__err("bad codelengths", "Corrupt PNG");
      if (c < 16)
         lencodes[n++] = (stb_uc) c;
      else {
         stb_uc fill = 0;
         if (c == 16) {
            c = stb__zreceive(a,2)+3;
            if (n == 0) return stb__err("bad codelengths", "Corrupt PNG");
            fill = lencodes[n-1];
         } else if (c == 17) {
            c = stb__zreceive(a,3)+3;
         } else if (c == 18) {
            c = stb__zreceive(a,7)+11;
         } else {
            return stb__err("bad codelengths", "Corrupt PNG");
         }
         if (ntot - n < c) return stb__err("bad codelengths", "Corrupt PNG");
         memset(lencodes+n, fill, c);
         n += c;
      }
   }
   if (n != ntot) return stb__err("bad codelengths","Corrupt PNG");
   if (!stb__zbuild_huffman(&a->z_length, lencodes, hlit)) return 0;
   if (!stb__zbuild_huffman(&a->z_distance, lencodes+hlit, hdist)) return 0;
   return 1;
}

static int stb__parse_zlib_header(stb__zbuf *a)
{
   int cmf   = stb__zget8(a);
   int cm    = cmf & 15;
   /* int cinfo = cmf >> 4; */
   int flg   = stb__zget8(a);
   if (stb__zeof(a)) return stb__err("bad zlib header","Corrupt PNG"); // zlib spec
   if ((cmf*256+flg) % 31 != 0) return stb__err("bad zlib header","Corrupt PNG"); // zlib spec
   if (flg & 32) return stb__err("no preset dict","Corrupt PNG"); // preset dictionary not allowed in png
   if (cm != 8) return stb__err("bad compression","Corrupt PNG"); // DEFLATE required for png
   // window = 1 << (8 + cinfo)... but who cares, we fully buffer output
   return 1;
}

static const stb_uc stb__zdefault_length[STB__ZNSYMS] =
{
   8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8, 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
   8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8, 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
   8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8, 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
   8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8, 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
   8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7, 7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8
};
static const stb_uc stb__zdefault_distance[32] =
{
   5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
};
/*
Init algorithm:
{
   int i;   // use <= to match clearly with spec
   for (i=0; i <= 143; ++i)     stb__zdefault_length[i]   = 8;
   for (   ; i <= 255; ++i)     stb__zdefault_length[i]   = 9;
   for (   ; i <= 279; ++i)     stb__zdefault_length[i]   = 7;
   for (   ; i <= 287; ++i)     stb__zdefault_length[i]   = 8;
   for (i=0; i <=  31; ++i)     stb__zdefault_distance[i] = 5;
}
*/

static int stb__parse_zlib(stb__zbuf *a, int parse_header)
{
   int final, type;
   if (parse_header)
      if (!stb__parse_zlib_header(a)) return 0;
   a->num_bits = 0;
   a->code_buffer = 0;
   do {
      final = stb__zreceive(a,1);
      type = stb__zreceive(a,2);
      if (type == 0) {
         //if (!stb__parse_uncompressed_block(a)) return 0;
      } else if (type == 3) {
         return 0;
      } else {
         if (type == 1) {
            // use fixed code lengths
            if (!stb__zbuild_huffman(&a->z_length  , stb__zdefault_length  , STB__ZNSYMS)) return 0;
            if (!stb__zbuild_huffman(&a->z_distance, stb__zdefault_distance,  32)) return 0;
         } else {
            if (!stb__compute_huffman_codes(a)) return 0;
         }
         if (!stb__parse_huffman_block(a)) return 0;
      }
   } while (!final);
   return 1;
}

static int stb__do_zlib(stb__zbuf *a, char *obuf, int olen, int exp, int parse_header)
{
   a->zout_start = obuf;
   a->zout       = obuf;
   a->zout_end   = obuf + olen;
   a->z_expandable = exp;

   return stb__parse_zlib(a, parse_header);
}

int stb_zlib_decode_buffer(char *obuffer, int olen, char const *ibuffer, int ilen)
{
   stb__zbuf a;
   a.zbuffer = (stb_uc *) ibuffer;
   a.zbuffer_end = (stb_uc *) ibuffer + ilen;
   if (stb__do_zlib(&a, obuffer, olen, 0, 1))
      return (int) (a.zout - a.zout_start);
   else
      return -1;
}

////////////////////////////////////////////////////////////////////

#define stb__sbraw(a) ((int *) (void *) (a) - 2)
#define stb__sbm(a)   stb__sbraw(a)[0]
#define stb__sbn(a)   stb__sbraw(a)[1]

#define stb__sbneedgrow(a,n)  ((a)==0 || stb__sbn(a)+n >= stb__sbm(a))
#define stb__sbmaybegrow(a,n) (stb__sbneedgrow(a,(n)) ? stb__sbgrow(a,n) : 0)
#define stb__sbgrow(a,n)  stb__sbgrowf((void **) &(a), (n), sizeof(*(a)))

#define stb__sbpush(a, v)      (stb__sbmaybegrow(a,1), (a)[stb__sbn(a)++] = (v))
#define stb__sbcount(a)        ((a) ? stb__sbn(a) : 0)
#define stb__sbfree(a)         ((a) ? STB_FREE(stb__sbraw(a)),0 : 0)

static int stb__zlib_bitrev(int code, int codebits)
{
   int res=0;
   while (codebits--) {
      res = (res << 1) | (code & 1);
      code >>= 1;
   }
   return res;
}

static unsigned int stb__zlib_countm(unsigned char *a, unsigned char *b, int limit)
{
   int i;
   for (i=0; i < limit && i < 258; ++i)
      if (a[i] != b[i]) break;
   return i;
}

static unsigned int stb__zhash(unsigned char *data)
{
   u32 hash = data[0] + (data[1] << 8) + (data[2] << 16);
   hash ^= hash << 3;
   hash += hash >> 5;
   hash ^= hash << 4;
   hash += hash >> 17;
   hash ^= hash << 25;
   hash += hash >> 6;
   return hash;
}

static void *stb__sbgrowf(void **arr, int increment, int itemsize)
{
   int m = *arr ? 2*stb__sbm(*arr)+increment : increment+1;
   void *p = STB_REALLOC_SIZED(*arr ? stb__sbraw(*arr) : 0, *arr ? (stb__sbm(*arr)*itemsize + sizeof(int)*2) : 0, itemsize * m + sizeof(int)*2);
   STB_ASSERT(p);
   if (p) {
      if (!*arr) ((int *) p)[1] = 0;
      *arr = (void *) ((int *) p + 2);
      stb__sbm(*arr) = m;
   }
   return *arr;
}

static unsigned char *stb__zlib_flushf(unsigned char *data, unsigned int *bitbuffer, int *bitcount)
{
   while (*bitcount >= 8) {
      stb__sbpush(data, STB_UCHAR(*bitbuffer));
      *bitbuffer >>= 8;
      *bitcount -= 8;
   }
   return data;
}

#define stb__zlib_flush() (out_ptr = stb__zlib_flushf(out_ptr, &bitbuf, &bitcount))
#define stb__zlib_add(code,codebits) \
      (bitbuf |= (code) << bitcount, bitcount += (codebits), stb__zlib_flush())
#define stb__zlib_huffa(b,c)  stb__zlib_add(stb__zlib_bitrev(b,c),c)
// default huffman tables
#define stb__zlib_huff1(n)  stb__zlib_huffa(0x30 + (n), 8)
#define stb__zlib_huff2(n)  stb__zlib_huffa(0x190 + (n)-144, 9)
#define stb__zlib_huff3(n)  stb__zlib_huffa(0 + (n)-256,7)
#define stb__zlib_huff4(n)  stb__zlib_huffa(0xc0 + (n)-280,8)
#define stb__zlib_huff(n)  ((n) <= 143 ? stb__zlib_huff1(n) : (n) <= 255 ? stb__zlib_huff2(n) : (n) <= 279 ? stb__zlib_huff3(n) : stb__zlib_huff4(n))
#define stb__zlib_huffb(n) ((n) <= 143 ? stb__zlib_huff1(n) : stb__zlib_huff2(n))

#define stb__ZHASH   16384

unsigned char * stb_zlib_compress(unsigned char *data, int data_len, int *out_len, int quality)
{
   static unsigned short lengthc[] = { 3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258, 259 };
   static unsigned char  lengtheb[]= { 0,0,0,0,0,0,0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4,  4,  5,  5,  5,  5,  0 };
   static unsigned short distc[]   = { 1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577, 32768 };
   static unsigned char  disteb[]  = { 0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13 };
   unsigned int bitbuf=0;
   int i,j, bitcount=0;
   unsigned char *out_ptr = NULL;
   unsigned char ***hash_table = (unsigned char***) STB_MALLOC(stb__ZHASH * sizeof(unsigned char**));
   if (hash_table == NULL)
      return NULL;
   if (quality < 5) quality = 5;

   stb__sbpush(out_ptr, 0x78);   // DEFLATE 32K window
   stb__sbpush(out_ptr, 0x5e);   // FLEVEL = 1
   stb__zlib_add(1,1);  // BFINAL = 1
   stb__zlib_add(1,2);  // BTYPE = 1 -- fixed huffman

   for (i=0; i < stb__ZHASH; ++i)
      hash_table[i] = NULL;

   i=0;
   while (i < data_len-3) {
      // hash next 3 bytes of data to be compressed
      int h = stb__zhash(data+i)&(stb__ZHASH-1), best=3;
      unsigned char *bestloc = 0;
      unsigned char **hlist = hash_table[h];
      int n = stb__sbcount(hlist);
      for (j=0; j < n; ++j) {
         if (hlist[j]-data > i-32768) { // if entry lies within window
            int d = stb__zlib_countm(hlist[j], data+i, data_len-i);
            if (d >= best) { best=d; bestloc=hlist[j]; }
         }
      }
      // when hash table entry is too long, delete half the entries
      if (hash_table[h] && stb__sbn(hash_table[h]) == 2*quality) {
         STB_MEMMOVE(hash_table[h], hash_table[h]+quality, sizeof(hash_table[h][0])*quality);
         stb__sbn(hash_table[h]) = quality;
      }
      stb__sbpush(hash_table[h],data+i);

      if (bestloc) {
         // "lazy matching" - check match at *next* byte, and if it's better, do cur byte as literal
         h = stb__zhash(data+i+1)&(stb__ZHASH-1);
         hlist = hash_table[h];
         n = stb__sbcount(hlist);
         for (j=0; j < n; ++j) {
            if (hlist[j]-data > i-32767) {
               int e = stb__zlib_countm(hlist[j], data+i+1, data_len-i-1);
               if (e > best) { // if next match is better, bail on current match
                  bestloc = NULL;
                  break;
               }
            }
         }
      }

      if (bestloc) {
         int d = (int) (data+i - bestloc); // distance back
         STB_ASSERT(d <= 32767 && best <= 258);
         for (j=0; best > lengthc[j+1]-1; ++j);
         stb__zlib_huff(j+257);
         if (lengtheb[j]) stb__zlib_add(best - lengthc[j], lengtheb[j]);
         for (j=0; d > distc[j+1]-1; ++j);
         stb__zlib_add(stb__zlib_bitrev(j,5),5);
         if (disteb[j]) stb__zlib_add(d - distc[j], disteb[j]);
         i += best;
      } else {
         stb__zlib_huffb(data[i]);
         ++i;
      }
   }
   // write out_ptr final bytes
   for (;i < data_len; ++i)
      stb__zlib_huffb(data[i]);
   stb__zlib_huff(256); // end of block
   // pad with 0 bits to byte boundary
   while (bitcount)
      stb__zlib_add(0,1);

   for (i=0; i < stb__ZHASH; ++i)
      (void) stb__sbfree(hash_table[i]);
   STB_FREE(hash_table);

   // store uncompressed instead if compression was worse
   if (stb__sbn(out_ptr) > data_len + 2 + ((data_len+32766)/32767)*5) {
      stb__sbn(out_ptr) = 2;  // truncate to DEFLATE 32K window and FLEVEL = 1
      for (j = 0; j < data_len;) {
         int blocklen = data_len - j;
         if (blocklen > 32767) blocklen = 32767;
         stb__sbpush(out_ptr, data_len - j == blocklen); // BFINAL = ?, BTYPE = 0 -- no compression
         stb__sbpush(out_ptr, STB_UCHAR(blocklen)); // LEN
         stb__sbpush(out_ptr, STB_UCHAR(blocklen >> 8));
         stb__sbpush(out_ptr, STB_UCHAR(~blocklen)); // NLEN
         stb__sbpush(out_ptr, STB_UCHAR(~blocklen >> 8));
         memcpy(out_ptr+stb__sbn(out_ptr), data+j, blocklen);
         stb__sbn(out_ptr) += blocklen;
         j += blocklen;
      }
   }

   {
      // compute adler32 on input
      unsigned int s1=1, s2=0;
      int blocklen = (int) (data_len % 5552);
      j=0;
      while (j < data_len) {
         for (i=0; i < blocklen; ++i) { s1 += data[j+i]; s2 += s1; }
         s1 %= 65521; s2 %= 65521;
         j += blocklen;
         blocklen = 5552;
      }
      stb__sbpush(out_ptr, STB_UCHAR(s2 >> 8));
      stb__sbpush(out_ptr, STB_UCHAR(s2));
      stb__sbpush(out_ptr, STB_UCHAR(s1 >> 8));
      stb__sbpush(out_ptr, STB_UCHAR(s1));
   }
   *out_len = stb__sbn(out_ptr);
   // make returned pointer freeable
   STB_MEMMOVE(stb__sbraw(out_ptr), out_ptr, *out_len);
   return (unsigned char *) stb__sbraw(out_ptr);
}

static unsigned int stb__crc32(unsigned char *buffer, int len)
{
   static unsigned int crc_table[256] =
   {
      0x00000000, 0x77073096, 0xEE0E612C, 0x990951BA, 0x076DC419, 0x706AF48F, 0xE963A535, 0x9E6495A3,
      0x0eDB8832, 0x79DCB8A4, 0xE0D5E91E, 0x97D2D988, 0x09B64C2B, 0x7EB17CBD, 0xE7B82D07, 0x90BF1D91,
      0x1DB71064, 0x6AB020F2, 0xF3B97148, 0x84BE41DE, 0x1ADAD47D, 0x6DDDE4EB, 0xF4D4B551, 0x83D385C7,
      0x136C9856, 0x646BA8C0, 0xFD62F97A, 0x8A65C9EC, 0x14015C4F, 0x63066CD9, 0xFA0F3D63, 0x8D080DF5,
      0x3B6E20C8, 0x4C69105E, 0xD56041E4, 0xA2677172, 0x3C03E4D1, 0x4B04D447, 0xD20D85FD, 0xA50AB56B,
      0x35B5A8FA, 0x42B2986C, 0xDBBBC9D6, 0xACBCF940, 0x32D86CE3, 0x45DF5C75, 0xDCD60DCF, 0xABD13D59,
      0x26D930AC, 0x51DE003A, 0xC8D75180, 0xBFD06116, 0x21B4F4B5, 0x56B3C423, 0xCFBA9599, 0xB8BDA50F,
      0x2802B89E, 0x5F058808, 0xC60CD9B2, 0xB10BE924, 0x2F6F7C87, 0x58684C11, 0xC1611DAB, 0xB6662D3D,
      0x76DC4190, 0x01DB7106, 0x98D220BC, 0xEFD5102A, 0x71B18589, 0x06B6B51F, 0x9FBFE4A5, 0xE8B8D433,
      0x7807C9A2, 0x0F00F934, 0x9609A88E, 0xE10E9818, 0x7F6A0DBB, 0x086D3D2D, 0x91646C97, 0xE6635C01,
      0x6B6B51F4, 0x1C6C6162, 0x856530D8, 0xF262004E, 0x6C0695ED, 0x1B01A57B, 0x8208F4C1, 0xF50FC457,
      0x65B0D9C6, 0x12B7E950, 0x8BBEB8EA, 0xFCB9887C, 0x62DD1DDF, 0x15DA2D49, 0x8CD37CF3, 0xFBD44C65,
      0x4DB26158, 0x3AB551CE, 0xA3BC0074, 0xD4BB30E2, 0x4ADFA541, 0x3DD895D7, 0xA4D1C46D, 0xD3D6F4FB,
      0x4369E96A, 0x346ED9FC, 0xAD678846, 0xDA60B8D0, 0x44042D73, 0x33031DE5, 0xAA0A4C5F, 0xDD0D7CC9,
      0x5005713C, 0x270241AA, 0xBE0B1010, 0xC90C2086, 0x5768B525, 0x206F85B3, 0xB966D409, 0xCE61E49F,
      0x5EDEF90E, 0x29D9C998, 0xB0D09822, 0xC7D7A8B4, 0x59B33D17, 0x2EB40D81, 0xB7BD5C3B, 0xC0BA6CAD,
      0xEDB88320, 0x9ABFB3B6, 0x03B6E20C, 0x74B1D29A, 0xEAD54739, 0x9DD277AF, 0x04DB2615, 0x73DC1683,
      0xE3630B12, 0x94643B84, 0x0D6D6A3E, 0x7A6A5AA8, 0xE40ECF0B, 0x9309FF9D, 0x0A00AE27, 0x7D079EB1,
      0xF00F9344, 0x8708A3D2, 0x1E01F268, 0x6906C2FE, 0xF762575D, 0x806567CB, 0x196C3671, 0x6E6B06E7,
      0xFED41B76, 0x89D32BE0, 0x10DA7A5A, 0x67DD4ACC, 0xF9B9DF6F, 0x8EBEEFF9, 0x17B7BE43, 0x60B08ED5,
      0xD6D6A3E8, 0xA1D1937E, 0x38D8C2C4, 0x4FDFF252, 0xD1BB67F1, 0xA6BC5767, 0x3FB506DD, 0x48B2364B,
      0xD80D2BDA, 0xAF0A1B4C, 0x36034AF6, 0x41047A60, 0xDF60EFC3, 0xA867DF55, 0x316E8EEF, 0x4669BE79,
      0xCB61B38C, 0xBC66831A, 0x256FD2A0, 0x5268E236, 0xCC0C7795, 0xBB0B4703, 0x220216B9, 0x5505262F,
      0xC5BA3BBE, 0xB2BD0B28, 0x2BB45A92, 0x5CB36A04, 0xC2D7FFA7, 0xB5D0CF31, 0x2CD99E8B, 0x5BDEAE1D,
      0x9B64C2B0, 0xEC63F226, 0x756AA39C, 0x026D930A, 0x9C0906A9, 0xEB0E363F, 0x72076785, 0x05005713,
      0x95BF4A82, 0xE2B87A14, 0x7BB12BAE, 0x0CB61B38, 0x92D28E9B, 0xE5D5BE0D, 0x7CDCEFB7, 0x0BDBDF21,
      0x86D3D2D4, 0xF1D4E242, 0x68DDB3F8, 0x1FDA836E, 0x81BE16CD, 0xF6B9265B, 0x6FB077E1, 0x18B74777,
      0x88085AE6, 0xFF0F6A70, 0x66063BCA, 0x11010B5C, 0x8F659EFF, 0xF862AE69, 0x616BFFD3, 0x166CCF45,
      0xA00AE278, 0xD70DD2EE, 0x4E048354, 0x3903B3C2, 0xA7672661, 0xD06016F7, 0x4969474D, 0x3E6E77DB,
      0xAED16A4A, 0xD9D65ADC, 0x40DF0B66, 0x37D83BF0, 0xA9BCAE53, 0xDEBB9EC5, 0x47B2CF7F, 0x30B5FFE9,
      0xBDBDF21C, 0xCABAC28A, 0x53B39330, 0x24B4A3A6, 0xBAD03605, 0xCDD70693, 0x54DE5729, 0x23D967BF,
      0xB3667A2E, 0xC4614AB8, 0x5D681B02, 0x2A6F2B94, 0xB40BBE37, 0xC30C8EA1, 0x5A05DF1B, 0x2D02EF8D
   };

   unsigned int crc = ~0u;
   int i;
   for (i=0; i < len; ++i)
      crc = (crc >> 8) ^ crc_table[buffer[i] ^ (crc & 0xff)];
   return ~crc;
}

#define stb__wpng4(o,a,b,c,d) ((o)[0]=STB_UCHAR(a),(o)[1]=STB_UCHAR(b),(o)[2]=STB_UCHAR(c),(o)[3]=STB_UCHAR(d),(o)+=4)
#define stb__wp32(data,v) stb__wpng4(data, (v)>>24,(v)>>16,(v)>>8,(v));
#define stb__wptag(data,s) stb__wpng4(data, s[0],s[1],s[2],s[3])

static void stb__wpcrc(unsigned char **data, int len)
{
   unsigned int crc = stb__crc32(*data - len - 4, len+4);
   stb__wp32(*data, crc);
}

static unsigned char stb__paeth(int a, int b, int c)
{
   int p = a + b - c, pa = abs(p-a), pb = abs(p-b), pc = abs(p-c);
   if (pa <= pb && pa <= pc) return STB_UCHAR(a);
   if (pb <= pc) return STB_UCHAR(b);
   return STB_UCHAR(c);
}

static void stb__encode_png_line(unsigned char *pixels, int stride_bytes, int width, int height, int y, int n, int filter_type, signed char *line_buffer)
{
    static int mapping[] = { 0, 1, 2, 3, 4 };
    static int firstmap[] = { 0, 1, 0, 5, 6 };
    int *mymap = (y != 0) ? mapping : firstmap;
    int type = mymap[filter_type];
    unsigned char *z = pixels + stride_bytes * y;

    if (type == 0) {
        memcpy(line_buffer, z, width * n);
        return;
    }

    int prev, above, above_prev;
    for (int i = 0; i < width * n; ++i) {
        prev = i >= n ? z[i - n] : 0;
        above = y != 0 ? z[i - stride_bytes] : 0;
        above_prev = y != 0 && i >= n ? z[i - stride_bytes - n] : 0;

        if (i < n) {
            switch (type) {
                case 1: line_buffer[i] = z[i]; break;
                case 2: line_buffer[i] = z[i] - above; break;
                case 3: line_buffer[i] = z[i] - (above >> 1); break;
                case 4: line_buffer[i] = (signed char)(z[i] - stb__paeth(0, above, 0)); break;
                case 5: line_buffer[i] = z[i]; break;
                case 6: line_buffer[i] = z[i]; break;
            }
        } else {
            switch (type) {
                case 1: line_buffer[i] = z[i] - prev; break;
                case 2: line_buffer[i] = z[i] - above; break;
                case 3: line_buffer[i] = z[i] - ((prev + above) >> 1); break;
                case 4: line_buffer[i] = z[i] - stb__paeth(prev, above, above_prev); break;
                case 5: line_buffer[i] = z[i] - (prev >> 1); break;
                case 6: line_buffer[i] = z[i] - stb__paeth(prev, 0, 0); break;
            }
        }
    }
}

unsigned char *stb_write_png_to_mem(const unsigned char *pixels, int stride_bytes, int x, int y, int n, int *out_len)
{
   int ctype[5] = { -1, 0, 4, 2, 6 };
   unsigned char sig[8] = { 137,80,78,71,13,10,26,10 };
   unsigned char *out_ptr, *o, *filt, *zlib;
   signed char *line_buffer;
   int j, zlen;

   if (stride_bytes == 0)
      stride_bytes = x * n;

   filt = (unsigned char *) STB_MALLOC((x * n + 1) * y); if (!filt) return 0;
   line_buffer = (signed char *) STB_MALLOC(x * n); if (!line_buffer) { STB_FREE(filt); return 0; }
   for (j = 0; j < y; ++j) {
      int filter_type, best_filter = 0, best_filter_val = 0x7fffffff, est, i;
      for (filter_type = 0; filter_type < 5; filter_type++) {
         stb__encode_png_line((unsigned char*)(pixels), stride_bytes, x, y, j, n, filter_type, line_buffer);

         for (est = 0, i = 0; i < x * n; ++i) {
            est += (line_buffer[i] ^ (line_buffer[i] >> 31)) - (line_buffer[i] >> 31);
         }
         if (est < best_filter_val) {
            best_filter_val = est;
            best_filter = filter_type;
         }
      }
      stb__encode_png_line((unsigned char*)(pixels), stride_bytes, x, y, j, n, best_filter, line_buffer);

      filt[j * (x * n + 1)] = (unsigned char) best_filter;
      memcpy(filt + j * (x * n + 1) + 1, line_buffer, x * n);
   }
   STB_FREE(line_buffer);
   zlib = stb_zlib_compress(filt, y * (x * n + 1), &zlen, 8);
   STB_FREE(filt);
   if (!zlib) return 0;

   *out_len = 8 + 12 + 13 + 12 + zlen + 12;
   out_ptr = (unsigned char *) STB_MALLOC(*out_len);
   if (!out_ptr) return 0;

   o = out_ptr;
   memcpy(o, sig, 8); o += 8;
   stb__wp32(o, 13);
   stb__wptag(o, "IHDR");
   stb__wp32(o, x);
   stb__wp32(o, y);
   *o++ = 8;
   *o++ = STB_UCHAR(ctype[n]);
   *o++ = 0;
   *o++ = 0;
   *o++ = 0;
   stb__wpcrc(&o, 13);

   stb__wp32(o, zlen);
   stb__wptag(o, "IDAT");
   memcpy(o, zlib, zlen);
   o += zlen;
   STB_FREE(zlib);
   stb__wpcrc(&o, zlen);

   stb__wp32(o, 0);
   stb__wptag(o, "IEND");
   stb__wpcrc(&o, 0);

   STB_ASSERT(o == out_ptr + *out_len);

   return out_ptr;
}

static FILE *stb__fopen(char const *filename, char const *mode)
{
   FILE *f;
   if (0 != fopen_s(&f, filename, mode))
      f=0;
   return f;
}

int stb_write_png(char const *filename, int x, int y, int comp, const void *data, int stride_bytes)
{
   FILE *f;
   int len;
   unsigned char *png = stb_write_png_to_mem((const unsigned char *) data, stride_bytes, x, y, comp, &len);
   if (png == NULL) return 0;

   f = stb__fopen(filename, "wb");
   if (!f) { STB_FREE(png); return 0; }
   fwrite(png, 1, len, f);
   fclose(f);
   STB_FREE(png);
   return 1;
}

//

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t color_type;
    uint8_t bit_depth;
    uint8_t *data;
} PNGImage;

uint32_t read_uint32(FILE *file) {
    uint32_t value;
    fread(&value, 4, 1, file);
    return ((value >> 24) & 0xFF) | ((value >> 8) & 0xFF00) | ((value << 8) & 0xFF0000) | ((value << 24) & 0xFF000000);
}

int png_sig_cmp(uint8_t *sig, size_t start, size_t num_to_check) {
    static uint8_t png_signature[8] = {137, 80, 78, 71, 13, 10, 26, 10};
    return memcmp(sig, png_signature + start, num_to_check);
}

int paeth_predictor(int a, int b, int c) {
    int p = a + b - c;
    int pa = abs(p - a);
    int pb = abs(p - b);
    int pc = abs(p - c);

    if (pa <= pb && pa <= pc) {
        return a;
    } else if (pb <= pc) {
        return b;
    } else {
        return c;
    }
}

int png_load(const char *filename, PNGImage *image) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        return -1;
    }

    uint8_t header[8];
    fread(header, 1, 8, file);
    if (png_sig_cmp(header, 0, 8)) {
        fclose(file);
        return -1;
    }

    uint32_t chunk_size;
    uint8_t chunk_type[4];
    uint8_t *compressed_data = NULL;
    uint32_t compressed_data_size = 0;
    uint32_t compressed_data_capacity = 0;

    while (!feof(file)) {
        chunk_size = read_uint32(file);
        fread(chunk_type, 1, 4, file);

        if (memcmp(chunk_type, "IHDR", 4) == 0) {
            image->width = read_uint32(file);
            image->height = read_uint32(file);
            fread(&image->bit_depth, 1, 1, file);
            fread(&image->color_type, 1, 1, file);
            fseek(file, 3, SEEK_CUR); // Skip compression, filter, and interlace bytes
        } else if (memcmp(chunk_type, "IDAT", 4) == 0) {
            if (compressed_data_size + chunk_size > compressed_data_capacity) {
                compressed_data_capacity = (compressed_data_size + chunk_size) * 2;
                compressed_data = (uint8_t *)realloc(compressed_data, compressed_data_capacity);
            }
            fread(compressed_data + compressed_data_size, 1, chunk_size, file);
            compressed_data_size += chunk_size;
        } else if (memcmp(chunk_type, "IEND", 4) == 0) {
            break;
        } else {
            fseek(file, chunk_size, SEEK_CUR);
        }
        fseek(file, 4, SEEK_CUR); // Skip CRC
    }

    uint32_t row_size = (image->color_type == 2 ? 3 : 4) * image->width;
    uint32_t data_size = row_size * image->height;

    //uint8_t *intermediate_data = (uint8_t *)malloc((row_size + 1) * image->height);
    image->data = (uint8_t *)malloc((row_size + 1) * image->height);

    int outlen = stb_zlib_decode_buffer(
        (char *)image->data, (row_size + 1) * image->height,
        (char *)compressed_data, compressed_data_size
    );

    if (outlen <= 0) {
        free(image->data);
        free(compressed_data);
        fclose(file);
        return -1;
    }

    // Check if there are any non-zero filter bytes
    u8 has_filters = 0;
    for (uint32_t y = 0; y < image->height; y++) {
        if (image->data[y * (row_size + 1)] != 0) {
            has_filters = 1;
            break;
        }
    }

    if (has_filters) {

		uint32_t row_size = (image->color_type == 2 ? 3 : 4) * image->width;
		for (uint32_t y = 0; y < image->height; y++) {
			uint8_t filter_type = image->data[y * (row_size + 1)];

			for (uint32_t x = 0; x < row_size; x++) {
				uint8_t a = (x >= (image->color_type == 2 ? 3 : 4)) ? image->data[y * (row_size + 1) + x + 1 - (image->color_type == 2 ? 3 : 4)] : 0;
				uint8_t b = (y > 0) ? image->data[(y - 1) * (row_size + 1) + x + 1] : 0;
				uint8_t c = (x >= (image->color_type == 2 ? 3 : 4) && y > 0) ? image->data[(y - 1) * (row_size + 1) + x + 1 - (image->color_type == 2 ? 3 : 4)] : 0;

				uint8_t recon = 0;

				switch (filter_type) {
					case 0:
						recon = image->data[y * (row_size + 1) + x + 1];
						break;
					case 1:
						recon = image->data[y * (row_size + 1) + x + 1] + a;
						break;
					case 2:
						recon = image->data[y * (row_size + 1) + x + 1] + b;
						break;
					case 3:
						recon = image->data[y * (row_size + 1) + x + 1] + (a + b) / 2;
						break;
					case 4:
						recon = image->data[y * (row_size + 1) + x + 1] + paeth_predictor(a, b, c);
						break;
					default:
						break;
				}

				image->data[y * (row_size + 1) + x + 1] = recon;
			}
		}
    }

    for (uint32_t y = 0; y < image->height; y++) {
        memcpy(image->data + y * row_size, image->data + y * (row_size + 1) + 1, row_size);
    }

    free(compressed_data);
    fclose(file);
    return 0;
}
#endif