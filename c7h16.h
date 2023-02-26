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
#define global volatile once
#define fn once inline
#define out return
#define make_type( _ ) typedef _
#define make_struct make_type( struct )
#define make_enum make_type( enum )
#define make_union make_type( union )
#define make_ptr( _ ) make_type( ptr( _ ) )
#define print printf
#define null NULL
#define loop while( 1 )
#define fn_ptr( _ ) void ( val(_) )()
#define fn_ptr_param1( _, _1 ) void ( * _ )( _1 )
#define fn_ptr_param2( _, _1, _2 ) void ( * _ )( _1, _2 )
#define fn_ptr_param3( _, _1, _2, _3 ) void ( * _ )( _1, _2, _3 )
#define fn_ptr_param4( _, _1, _2, _3, _4 ) void ( * _ )( _1, _2, _3, _4 )
#define void_ptr ptr( void )
#define void_ptr_( _ ) ( void_ptr )( _ )
#define cast( type, value ) val(((ptr(type))(adr(value))))

#define and &&
#define or ||
#define mod %

#define pi 3.14159265358979
#define pi_mul2 6.28318530717959
#define pi_mul3 9.42477796076937
#define pi_mul4 12.56637061435917
#define pi_div2 1.57079632679489
#define pi_div3 1.04719755119659
#define pi_div4 0.78539816339744
#define pi_pow2 9.86960440108935
#define pi_pow3 31.00627668029982
#define pi_pow4 97.40909103400243

//

make_type( uint8_t ) u8;
#define u8_( _ ) (( u8 )( _ ))
make_type( uint16_t ) u16;
#define u16_( _ ) (( u16 )( _ ))
make_type( uint32_t ) u32;
#define u32_( _ ) (( u32 )( _ ))
make_type( uint64_t ) u64;
#define u64_( _ ) (( u64 )( _ ))
make_type( int16_t ) s16;
#define s16_( _ ) (( s16 )( _ ))
make_type( int32_t ) s32;
#define s32_( _ ) (( s32 )( _ ))
make_type( int64_t ) s64;
#define s64_( _ ) (( s64 )( _ ))
make_type( float ) f32;
#define f32_( _ ) (( f32 )( _ ))
make_type( long double ) f64;
#define f64_( _ ) (( f64 )( _ ))

make_type( ptr( const char )) str;

//

#define new_mem( type, n ) ( ptr( type ) ) calloc( n, sizeof( type ) )
#define new_mem_ptr( type, n, ptr_type ) ( ptr_type ) calloc( n, sizeof( type ) )

#define CAT_LINE_SET( a, line ) $##a##_##line
#define CAT_LINE( a, line ) CAT_LINE_SET( a, line )
#define VAR_LINE( name ) CAT_LINE( name, __LINE__ )

#define iter( to_n, var ) register s32 VAR_LINE( to ) = (to_n); if(VAR_LINE( to )) for( register s32 var = 0; var < VAR_LINE( to ); var++ )

#define rep( n ) register u32 VAR_LINE( rep ) = n; for( register u32 VAR_LINE( LOOP ) = 0; VAR_LINE( LOOP ) < VAR_LINE( rep ); ++VAR_LINE( LOOP ) )

#define ifn( _ ) if( !( _ ) )

//

#define sign( _ ) ( ( _ ) < 0 ? -1 : ( ( _ ) > 0 ? 1 : 0) )

//

make_struct {
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
            size_t new_size_mem = (size_t)(l->size_mem << 1); \
            void* new_data = realloc(l->data, new_size_mem * l->size_type); \
            if (new_data == null) { \
                break; \
            } \
            l->size_mem = new_size_mem; \
            l->data = new_data; \
        } \
    } while (0)

#define list_set( l, type, pos, val ) ((ptr(type))(l->data))[pos] = val

#define list_add( l, type, val ) \
    do { \
        list_alloc(l); \
		list_set( l, type, l->size++, val ); \
    } while (0)

#define list_insert( l, pos, val ) \
    do { \
        list_alloc(l); \
        memmove((l)->data + ((pos + 1) * l->size_type), (l)->data + (pos * l->size_type), ((l)->size - (pos)) * l->size_type); \
        ((ptr(typeof(val)))(l->data))[pos] = val; \
        ++(l)->size; \
    } while (0)

#define list_pop_back( l ) \
    ((l)->size > 0 ? (l)->data[--(l)->size] : null)

#define list_fill( l, val ) \
    do { \
        to(l->size, n) { \
            (l)->data[n] = val; \
        } \
    } while (0)

#define list_free( l ) free((l)->data)

#define list_get( l, type, pos ) ( ( ptr( type ) )( l->data ) )[ pos ]

//

make_struct{
	u32 pos_prev;
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
    } while (0)

#define pool_free( p, type, pos ) \
	do { \
		list_add(p->free_list, u32, pos); \
		list_set(p->id_list, type, pos, 0); \
	} while (0)

//



//

make_struct {
	f32
		r, d;
} dual;

#define dual_( _ ) (dual)( _ )
#define make_dual( r, d ) dual{ r, d }

//

make_struct {
	f32
		x, y;
} vec2_struct;
#define $vec2_( _ ) (vec2_struct)( _ )
#define $make_vec2_struct( x, y ) (vec2_struct){ x, y }

make_struct{
	f32
		x, y, z;
} vec3_struct;
#define $vec3_( _ ) (vec3_struct)( _ )
#define $make_vec3_struct( x, y, z ) (vec3_struct){ x, y, z }

make_struct{
	f32
		x, y, z, w;
} vec4_struct;
#define $vec4_( _ ) (vec4_struct)( _ )
#define $make_vec4_struct( x, y, z, w ) (vec4_struct){ x, y, z, w }

make_struct{
	s32
		x, y;
} ivec2_struct;
#define $ivec2_( _ ) (ivec2_struct)( _ )
#define $make_ivec2_struct( x, y ) (ivec2_struct){ x, y }

make_struct{
	s32
		x, y, z;
} ivec3_struct;
#define $ivec3_( _ ) (ivec3_struct)( _ )
#define $make_ivec3_struct( x, y, z ) (ivec3_struct){ x, y, z }

make_struct{
	s32
		x, y, z, w;
} ivec4_struct;
#define $ivec4_( _ ) (ivec4_struct)( _ )
#define $make_ivec4_struct( x, y, z, w ) (ivec4_struct){ x, y, z, w }

make_struct{
	u32
		x, y;
} uvec2_struct;
#define $uvec2_( _ ) (uvec2_struct)( _ )
#define $make_uvec2_struct( x, y ) (uvec2_struct){ x, y }

make_struct{
	u32
		x, y, z;
} uvec3_struct;
#define $uvec3_( _ ) (uvec3_struct)( _ )
#define $make_uvec3_struct( x, y, z ) (uvec3_struct){ x, y, z }

make_struct{
	u32
		x, y, z, w;
} uvec4_struct;
#define $uvec4_( _ ) (uvec4_struct)( _ )
#define $make_uvec4_struct( x, y, z, w ) (uvec4_struct){ x, y, z, w }

typedef __m128 vec;

#define vec_( _ ) (vec)( _ )
#define make_vec( x, y, z ) _mm_set_ps(0, z, y, x)

#define vec_x( v ) (val((vec4_struct*)((void_ptr)(&v)))).x
#define vec_y( v ) (val((vec4_struct*)((void_ptr)(&v)))).y
#define vec_z( v ) (val((vec4_struct*)((void_ptr)(&v)))).z
#define vec_w( v ) (val((vec4_struct*)((void_ptr)(&v)))).z

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
	out vec_div_f32(v, vec_len(v));
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

#define quat_( _ ) (quat)( _ )
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

//

make_struct {
	quat
		r, d;
} dual_quat;
#define dual_quat_( _ ) (dual_quat)( _ )
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
#define dual_quat_proj_( _ ) (dual_quat_proj)( _ )
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
	out new_dual_quat_proj( dual_quat_set( dir, pos ), $make_vec4_struct( f / aspect, f, ( near + far ) * d, 2.0f * near * far * d ));
}

//

make_struct {
	vec4_struct
		a, b, c, d;
} matrix;
#define matrix_( _ ) (matrix)( _ )
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