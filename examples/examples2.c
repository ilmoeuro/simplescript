Result function1(Closure closure, Arguments arguments) {
    Value print = get_closure_value(closure, 0);
    pos("examples2.simp", 9);
    Value i = make_number(99);
loop1:
    pos("examples2.simp", 10);
    Value aux1 = make_number(0);
    Value aux2 = greater_than(i, aux1);
    if (!is_true(aux2)) goto loop1_end;
    pos("examples2.simp", 11);
    call(print, i);
    pos("examples2.simp", 12);
    Value aux3 = make_string(" bottles of beer on the wall\n");
    call(print, aux3);
    pos("examples2.simp", 13);
    call(print, i);
    pos("examples2.simp", 14);
    Value aux4 = make_string(" bottles of beer\n");
    call(print, aux4);
    pos("examples2.simp", 15);
    Value aux5 = make_string("Take one down, pass it around");
    call(print, aux5);
    pos("examples2.simp", 16);
    Value aux6 = make_number(1);
    i = subtract(i, aux6);
    pos("examples2.simp", 17);
    call(print, i);
    pos("examples2.simp", 18);
    Value aux7 = make_string(" bottles of beer on the wall\n");
    call(print, aux7);
    goto loop1;
loop1_end:
    return make_null();
}

void module_examples2(Environment env) {
    pos("examples2.simp", 1);
    Value print = import(env, "std", "print");
    pos("examples2.simp", 5);
    Closure the99bottles_closure = make_closure(1);
    set_closure_value(the99bottles_closure, 0, print);
    Value the99bottles = make_function(function1, the99bottles_closure);
    export(env, "examples2", "the99bottles", the99bottles);
}