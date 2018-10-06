Result function1(Closure closure, Arguments arguments) {
	Value print = get_closure_value(closure, 0);
	pos("examples1.simp", 
	Value i = make_number(99);
loop1:
	Value aux1 = make_number(0);
	Value aux2 = greater_than(i, aux1);
	if (!is_true(aux2)) goto loop1_end;
	call(print, i);
	Value aux3 = make_string(" bottles of beer on the wall\n");
	call(print, aux3);
	call(print, i);
	Value aux4 = make_string(" bottles of beer\n");
	call(print, aux4);
	Value aux5 = make_string("Take one down, pass it around");
	call(print, aux5);
	Value aux6 = make_number(1);
	i = subtract(i, aux6);
	call(print, i);
	call(print, make_string(" bottles of beer on the wall\n");
	goto loop1;
loop1_end:
	return make_null();
}

void module_examples1(Environment env) {
	Value print = import(env, "std", "print");
	Value input = import(env, "std", "input");
	Value range = import(env, "std", "range");
	Value toNumber = import(env, "std", "toNumber");

	Closure the99bottles_closure = make_closure();
	set_closure_value(the99bottles_closure, 0, print);
	Value the99bottles = make_function(function1, the99bottles_closure);
	export(env, "examples1", "the99bottles", the99bottles);
}


