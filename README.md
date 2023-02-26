-------
-------
-------
-------
-------
-------
-------
# ***c7h16_heptane : : : .***
***an explicit, minimal, and esoteric abstraction of C99 to alter it into a fuel for graphical applications and interfaces.***

```c
#include "c7h16.h"

make_struct{
	u32
		pool_id;
	u8
		a;
} TYPE_struct;
make_ptr(TYPE_struct) TYPE;

global pool global_TYPE_pool;

fn pure TYPE_( TYPE in_TYPE ) {
	in_TYPE->a = 0;
}

fn TYPE $new_TYPE( u8 in_a ) {
#ifdef debug
	if (in_a == "" or in_a == null) print_error_out("INVALID INPUT a");
#endif
	//
	TYPE temp_TYPE = new_mem(TYPE_struct, 1);
	temp_TYPE->a = 1;
	//
	pool_add(global_TYPE_pool, TYPE, temp_TYPE);
	temp_TYPE->pool_id = global_TYPE_pool->pos_prev;
	out temp_TYPE;
}
#define new_TYPE( t ) $new_TYPE( sizeof( t ) )

fn TYPE delete_TYPE( TYPE in_TYPE ) {
#ifdef debug
	if (in_TYPE == "" or in_TYPE == null) print_error_out("INVALID INPUT TYPE");
#endif
	//
	free(in_TYPE);
}
```
