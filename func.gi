#F PrintJITTo(<c>, <opts>)
#F    Prints the intermediate code into a file for JIT compliation
#F
PrintJITTo := function(name, code, opts)
    local pts, pts1, repli, zip, zip2, x, l, g, j, k, i, cg;
    pts := PrintToString(opts.prettyPrint(code));
    cg := -1;
    l := [];
    for i in [1..Length(pts)] do
        if Length(pts[i]) >= 12 and SubString(pts[i], 1, 12) = "void destroy" then
            g := Position(pts, "}", i);
            cg := i;
            for j in [i..g] do
                Add(l, pts[j]);
            od;
        fi;
        if Length(pts[i]) >= 9 and SubString(pts[i], 1, 9) = "void init" then
            g := Position(pts, "}", i);
            for j in [i..g] do
                Add(l, pts[j]);
            od;
        fi;
    od;
    cg := cg -1;
    pts1 := Sublist(pts, [6..cg]);
    repli := Replicate(Length(pts1), "\\n\\");
    zip := Flat(Zip2(pts1, repli));
    x := 0;
    PrintTo(name, pts[1]);
    for k in [2..4] do
        AppendTo(name, pts[k],"\n");
    od;
    AppendTo(name, "\n");
    AppendTo(name, "const char * kernels = \"");
    for i in zip do
        if x mod 2 = 0 then
            AppendTo(name, i);
            x := x + 1;
        else 
            AppendTo(name, i, "\n");
            x := x + 1;
        fi;
    od;
   AppendTo(name,"\";", "\n");
    for k in l do
       AppendTo(name, k, "\n");
    od;
end;

#F PrintJIT(<c>, <opts>)
#F    Prints the intermediate code into a string for JIT compliation
#F
PrintJIT := function(code, opts)
    local pts, pts1, repli, zip, zip2, x, name, l, g, j, k, i, cg, code2;
    name := "jit_generated.cpp";
    pts := PrintToString(opts.prettyPrint(code));
    cg := -1;
    l := [];
    for i in [1..Length(pts)] do
        if Length(pts[i]) >= 12 and SubString(pts[i], 1, 12) = "void destroy" then
            g := Position(pts, "}", i);
            cg := i;
            for j in [i..g] do
                Add(l, pts[j]);
            od;
        fi;
        if Length(pts[i]) >= 9 and SubString(pts[i], 1, 9) = "void init" then
            g := Position(pts, "}", i);
            for j in [i..g] do
                Add(l, pts[j]);
            od;
        fi;
    od;
    cg := cg -1;
    pts1 := Sublist(pts, [6..cg]);
    repli := Replicate(Length(pts1), "\\n\\");
    zip := Flat(Zip2(pts1, repli));
    x := 0;
    for k in [1..4] do
        Print(pts[k],"\n");
    od;
    Print("\n");
    Print("const char * kernels = \"");
    for i in zip do
        if x mod 2 = 0 then
            Print(i);
            x := x + 1;
        else 
            Print(i, "\n");
            x := x + 1;
        fi;
    od;
    Print("\";", "\n");
    for k in l do
        Print(k, "\n");
    od;
end;

#F PrintJIT2(<c>, <opts>)
#F    Prints generated code into parseable text file for FFTX jitting
#F
PrintJIT2 := function(code, opts)
    local pts, collection2, x, y, name, j, i, ptr_length, values_ptr, datas, old_includes, old_skip;
    datas := Collect(code, data);
    collection2 := Set(Collect(Collect(code, @(1,var, e-> IsArrayT(e.t) or IsPtrT(e.t) and IsBound(e.decl_specs) = true)), 
                @(1,var, e->e.decl_specs[1] = "__device__" and IsBound(e.value) = false)));
    ptr_length := Collect(code, @(1, call, e-> e.args[1].id = "cudaMalloc")); #getting sizes of device ptrs
    values_ptr := Collect(ptr_length, @(1,Value, e-> IsInt(e.v))); # getting sizes of device ptrs
    code := SubstTopDown(code, @(1,func, e->e.id <> "transform"), e->skip());    
    code := SubstTopDown(code, @(1,specifiers_func), e->let(g := Cond(IsBound(e.decl_specs) and e.decl_specs[1] = "__global__", ["extern \"C\" __global__"], e.decl_specs[1]), specifiers_func(g, e.ret, e.id, e.params, e.cmd))); #changing params to be all inputs
    code := SubstTopDown(code, @(1, func, e->e.id = "transform"), e-> specifiers_func(["extern \"C\" __global__"], e.ret, opts.cudasubName, e.params, e.cmd));
    old_skip := opts.unparser.skip;
    old_includes := opts.includes;
    opts.unparser.skip := (self, o, i, is) >> Print("");
    opts.includes := [];
    pts := PrintToString(opts.prettyPrint(code));
    opts.unparser.skip := old_skip;
    opts.includes := old_includes;
    x := 0;
    y := 1;
    Print("JIT BEGIN\n");
    for i in [1..Length(collection2)] do
        if IsPtrT(collection2[i].t) then
            Print(0, " ", collection2[i], " ", _unwrap(values_ptr[y]), " ", "pointer_", collection2[i].t.t.ctype, "\n");
            Print(3, " ", x, " ", _unwrap(values_ptr[y]), " ");
                if collection2[i].t.t.ctype = "int" then
                    Print(0," ");
                elif collection2[i].t.t.ctype = "float" then
                    Print(1, " ");
                elif collection2[i].t.t.ctype = "double" then
                    Print(2, " ");
                else
                    Print("how???\n");
                fi;
            Print("\n");
            x := x+1;
            y := y+1;
        elif IsArrayT(collection2[i].t) then
            Print(0, " ", collection2[i], " ", collection2[i].t.size, " ", collection2[i].t.t.ctype, "\n");
            Print(3, " ", x, " ", collection2[i].t.size, " "); 
                if collection2[i].t.t.ctype = "int" then
                    Print(0," ");
                elif collection2[i].t.t.ctype = "float" then
                    Print(1, " ");
                elif collection2[i].t.t.ctype = "double" then
                    Print(2, " ");
                else
                    Print("how???\n");
                fi;
            Print("\n");
            x := x+1;
        else
            Print("it got here how???\n");
        fi;
    od;
    for i in [1..Length(datas)] do
        if datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = false then
            Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", datas[i].var.t.t.ctype, "\n");
            Print(3, " ", x, " ", datas[i].var.t.size, " "); 
            if datas[i].var.t.t.ctype = "int" then
                Print(0," ");
            elif datas[i].var.t.t.ctype = "float" then
                Print(1, " ");
            elif datas[i].var.t.t.ctype = "double" then
                Print(2, " ");
            else
                Print("how???\n");
            fi;
            for j in [1..datas[i].var.t.size] do 
                Print(_unwrap(_unwrap(datas[i].value)[j]), " ");
            od;
            Print("\n");
            x := x+1;
        elif datas[1].var.decl_specs[1] = "__constant__" or (datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = true) then
            Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", "constant", "\n");
            Print(3, " ", x, " ", datas[i].var.t.size, " ", "3\n"); 
            x := x+1;
        else
            Print("how???\n");
        fi;
    od;
    Print("2", " ", opts.cudasubName, "\n");
    Print("------------------\n");
    Print(SubString(pts, 88, Length(pts)));
end;


#F PrintJIT2(<c>, <opts>)
#F    Prints generated code into parseable text file for FFTX jitting
#F
OLDPrintJIT2 := function(code, opts)
    local pts, collection1, collection2, x, name, j,  i, cg, code2, vars, datas, beglength;
    code2 := SubstBottomUp(Copy(code), @(1,func, e->e.id <> "transform"), e->skip());
    name := "jit_generated.cpp";
    pts := PrintToString(opts.prettyPrint(code2));
    datas := Collect(code, data);
    collection1 := Collect(code, @(1,var, e-> IsArrayT(e.t) and IsBound(e.decl_specs) = true));
    collection2 := Set(Collect(collection1, @(1,var, e->e.decl_specs[1] = "__device__")));
    vars := [];
    x := 0;
    Print("JIT BEGIN\n");
    for i in [1..Length(collection2)] do
        Print(0, " ", collection2[i], " ", collection2[i].t.size, " ", collection2[i].t.t.ctype, "\n");
        Print(3, " ", x, " ", collection2[i].t.size, " "); 
            if collection2[i].t.t.ctype = "int" then
                Print(0," ");
            elif collection2[i].t.t.ctype = "float" then
                Print(1, " ");
            elif collection2[i].t.t.ctype = "double" then
                Print(2, " ");
            else
                Print("how???\n");
            fi;
        Print("\n");
        x := x+1;
    od;
    for i in [1..Length(datas)] do
        if datas[1].var.decl_specs[1] = "__device__" then
            Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", datas[i].var.t.t.ctype, "\n");
            Print(3, " ", x, " ", datas[i].var.t.size, " "); 
            if datas[i].var.t.t.ctype = "int" then
                Print(0," ");
            elif datas[i].var.t.t.ctype = "float" then
                Print(1, " ");
            elif datas[i].var.t.t.ctype = "double" then
                Print(2, " ");
            else
                Print("how???\n");
            fi;
            for j in [1..datas[i].var.t.size] do 
                Print(_unwrap(_unwrap(datas[i].value)[j]), " ");
            od;
            Print("\n");
            x := x+1;
        elif datas[1].var.decl_specs[1] = "__constant__" then
            Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", "constant", "\n");
            Print(3, " ", x, " ", datas[i].var.t.size, " ", "3\n"); 
            x := x+1;
        else
            Print("how???\n");
        fi;
    od;
    Print("2", " ", opts.cudasubName, "\n");
    Print("------------------\n");
    beglength := 7+Length(opts.includes);
    for i in [beglength..(Length(pts)-1)] do
        if pts[i] = "/* skip */" then 
            Print("");
        elif Length(pts[i]) > 4 and SubString(pts[i],0,4) = "void" then 
            Print("extern \"C\" __global__ ", pts[i],"\n");
        else
            Print(pts[i], "\n");
        fi;
    od;
end;


#F PrintHIPJIT2(<c>, <opts>)
#F    Prints generated code into parseable text file for FFTX jitting with HIP
#F
PrintHIPJIT2 := function(code, opts)
    local pts, collection1, collection2, x, y, j,  i, cg, code2, vars, datas, params, kernels, ckernels, values_ptr, ptr_length;
    kernels := Collect(code, cu_call); #get kernel names for input
    ckernels := Collect(code, specifiers_func); #get kernel signatures
    params := Collect(code, @(1, func, e-> e.id = "transform"))[1].params; #get launch params
    code2 := SubstBottomUp(Copy(code), @(1,func, e->e.id <> "transform"), e->skip()); #removing init/destory
    code2 := SubstBottomUp(code2, @(1,func, e->e.id = "transform"), e->skip());# removing transform
    code2 := SubstTopDown(code2, @(1,specifiers_func), e->specifiers_func(e.decl_specs, e.ret, e.id, params, e.cmd)); #changing params to be all inputs
    pts := PrintToString(opts.prettyPrint(code2)); #print to string
    datas := Collect(code, data); #get device/constant arrays with value
    collection2 := Set(Collect(Collect(code, @(1,var, e-> IsArrayT(e.t) or IsPtrT(e.t) and IsBound(e.decl_specs) = true)), 
                @(1,var, e->e.decl_specs[1] = "__device__" and IsBound(e.value) = false))); #collect none value device arrays
    ptr_length := Collect(code, @(1, func, e-> e.id = "init")); #getting sizes of device ptrs
    values_ptr := Collect(ptr_length, @(1,Value, e-> IsInt(e.v))); # getting sizes of device ptrs
    x := 0;
    y := 1;
    Print("JIT BEGIN\n");
    for i in [1..Length(collection2)] do
        if IsPtrT(collection2[i].t) then
            Print(0, " ", collection2[i], " ", _unwrap(values_ptr[y]), " ", "pointer_", collection2[i].t.t.ctype, "\n");
            Print(3, " ", x, " ", _unwrap(values_ptr[y]), " ");
                if collection2[i].t.t.ctype = "int" then
                    Print(0," ");
                elif collection2[i].t.t.ctype = "float" then
                    Print(1, " ");
                elif collection2[i].t.t.ctype = "double" then
                    Print(2, " ");
                else
                    Print("how???\n");
                fi;
            Print("\n");
            x := x+1;
            y := y+1;
        elif IsArrayT(collection2[i].t) then
            Print(0, " ", collection2[i], " ", collection2[i].t.size, " ", collection2[i].t.t.ctype, "\n");
            Print(3, " ", x, " ", collection2[i].t.size, " "); 
                if collection2[i].t.t.ctype = "int" then
                    Print(0," ");
                elif collection2[i].t.t.ctype = "float" then
                    Print(1, " ");
                elif collection2[i].t.t.ctype = "double" then
                    Print(2, " ");
                else
                    Print("how???\n");
                fi;
            Print("\n");
            x := x+1;
        else
            Print("it got here how???\n");
        fi;
    od;
    for i in [1..Length(datas)] do
        if datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = false then
            Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", datas[i].var.t.t.ctype, "\n");
            Print(3, " ", x, " ", datas[i].var.t.size, " "); 
            if datas[i].var.t.t.ctype = "int" then
                Print(0," ");
            elif datas[i].var.t.t.ctype = "float" then
                Print(1, " ");
            elif datas[i].var.t.t.ctype = "double" then
                Print(2, " ");
            else
                Print("how???\n");
            fi;
            for j in [1..datas[i].var.t.size] do 
                Print(_unwrap(_unwrap(datas[i].value)[j]), " ");
            od;
            Print("\n");
            x := x+1;
        elif datas[1].var.decl_specs[1] = "__constant__" or (datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = true) then
            Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", "constant", "\n");
            Print(3, " ", x, " ", datas[i].var.t.size, " ", "3\n"); 
            x := x+1;
        else
            Print("how???\n");
        fi;
    od;
    for i in [1..Length(kernels)] do
        Print("2", " ", kernels[i].func, " ", _unwrap(kernels[i].dim_grid.x.value), " ", _unwrap(kernels[i].dim_grid.y.value), " ", _unwrap(kernels[i].dim_grid.z.value), " ", _unwrap(kernels[i].dim_block.x.value), 
        " ", _unwrap(kernels[i].dim_block.y.value), " ", _unwrap(kernels[i].dim_block.z.value));
        Print("\n");
    od;
    Print("------------------");
    for i in [5..(Length(pts)-1)] do
        if Length(pts[i]) > 4 and SubString(pts[i], 0, 8) = "#include" then
            Print("");
        elif pts[i] = "/* skip */" then 
            Print("");
        elif Length(pts[i]) > 4 and SubString(pts[i],0,10) = "__global__" then 
            Print("extern \"C\" ", pts[i],"\n");
        else
            Print(pts[i], "\n");
        fi;
    od;
end;

#F PrintHIPJIT_8.4<(<c>, <opts>)
#F    Prints generated code into parseable text file for FFTX jitting with HIP
#F
PrintHIPJIT_8_4 := function(code, opts)
    local pts, collection1, collection2, x, y, j,  i, cg, code2, vars, datas, params, kernels, ckernels, values_ptr, ptr_length;
    kernels := Collect(code, cu_call); #get kernel names for input
    ckernels := Collect(code, specifiers_func); #get kernel signatures
    params := Collect(code, @(1, func, e-> e.id = "transform"))[1].params; #get launch params
    datas := Collect(code, data); #get device/constant arrays with value
    collection2 := Set(Collect(Collect(code, @(1,var, e-> IsArrayT(e.t) or IsPtrT(e.t) and IsBound(e.decl_specs) = true)), 
                @(1,var, e->e.decl_specs[1] = "__device__" and IsBound(e.value) = false))); #collect none value device arrays
    ptr_length := Collect(code, @(1, func, e-> e.id = "init")); #getting sizes of device ptrs
    values_ptr := Collect(ptr_length, @(1,Value, e-> IsInt(e.v))); # getting sizes of device ptrs
    code := SubstTopDown(code, @(1,func, e->e.id <> "transform"), e->skip()); #removing init/destory
    #Print(opts.prettyPrint(code));
    code := SubstTopDown(code, @(1,func, e->e.id = "transform"), e->skip());# removing transform
    code := SubstTopDown(code, @(1,specifiers_func), e->specifiers_func(e.decl_specs, e.ret, e.id, params, e.cmd)); #changing params to be all inputs
    pts := PrintToString(opts.prettyPrint(code)); #print to string
    x := 0;
    y := 1;
    Print("JIT BEGIN\n");
    for i in [1..Length(collection2)] do
        if IsPtrT(collection2[i].t) then
            Print(0, " ", collection2[i], " ", _unwrap(values_ptr[y]), " ", "pointer_", collection2[i].t.t.ctype, "\n");
            Print(3, " ", x, " ", _unwrap(values_ptr[y]), " ");
                if collection2[i].t.t.ctype = "int" then
                    Print(0," ");
                elif collection2[i].t.t.ctype = "float" then
                    Print(1, " ");
                elif collection2[i].t.t.ctype = "double" then
                    Print(2, " ");
                else
                    Print("how???\n");
                fi;
            Print("\n");
            x := x+1;
            y := y+1;
        elif IsArrayT(collection2[i].t) then
            Print(0, " ", collection2[i], " ", collection2[i].t.size, " ", collection2[i].t.t.ctype, "\n");
            Print(3, " ", x, " ", collection2[i].t.size, " "); 
                if collection2[i].t.t.ctype = "int" then
                    Print(0," ");
                elif collection2[i].t.t.ctype = "float" then
                    Print(1, " ");
                elif collection2[i].t.t.ctype = "double" then
                    Print(2, " ");
                else
                    Print("how???\n");
                fi;
            Print("\n");
            x := x+1;
        else
            Print("it got here how???\n");
        fi;
    od;
    for i in [1..Length(datas)] do
        if datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = false then
            Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", datas[i].var.t.t.ctype, "\n");
            Print(3, " ", x, " ", datas[i].var.t.size, " "); 
            if datas[i].var.t.t.ctype = "int" then
                Print(0," ");
            elif datas[i].var.t.t.ctype = "float" then
                Print(1, " ");
            elif datas[i].var.t.t.ctype = "double" then
                Print(2, " ");
            else
                Print("how???\n");
            fi;
            for j in [1..datas[i].var.t.size] do 
                Print(_unwrap(_unwrap(datas[i].value)[j]), " ");
            od;
            Print("\n");
            x := x+1;
        elif datas[1].var.decl_specs[1] = "__constant__" or (datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = true) then
            Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", "constant", "\n");
            Print(3, " ", x, " ", datas[i].var.t.size, " ", "3\n"); 
            x := x+1;
        else
            Print("how???\n");
        fi;
    od;
    for i in [1..Length(kernels)] do
        Print("2", " ", kernels[i].func, " ", _unwrap(kernels[i].dim_grid.x.value), " ", _unwrap(kernels[i].dim_grid.y.value), " ", _unwrap(kernels[i].dim_grid.z.value), " ", _unwrap(kernels[i].dim_block.x.value), 
        " ", _unwrap(kernels[i].dim_block.y.value), " ", _unwrap(kernels[i].dim_block.z.value));
        Print("\n");
    od;
    Print("------------------");
    for i in [5..(Length(pts)-1)] do
        if Length(pts[i]) > 4 and SubString(pts[i], 0, 8) = "#include" then
            Print("");
        elif pts[i] = "/* skip */" then 
            Print("");
        elif Length(pts[i]) > 4 and SubString(pts[i],0,10) = "__global__" then 
            Print("extern \"C\" ", pts[i],"\n");
        else
            Print(pts[i], "\n");
        fi;
    od;
end;

#F PrintHIPJIT<(<c>, <opts>)
#F    Prints generated code into parseable text file for FFTX jitting with HIP
#F
PrintHIPJIT := function(code, opts)
    local pts, collection1, collection2, x, y, j,  i, cg, code2, vars, datas, params, kernels, ckernels, values_ptr, ptr_length, old_includes, old_skip;
    kernels := Collect(code, cu_call); #get kernel names for input
    ckernels := Collect(code, specifiers_func); #get kernel signatures
    params := Collect(code, @(1, func, e-> e.id = "transform"))[1].params; #get launch params
    datas := Collect(code, data); #get device/constant arrays with value
    collection2 := Set(Collect(Collect(code, @(1,var, e-> IsArrayT(e.t) or IsPtrT(e.t) and IsBound(e.decl_specs) = true)), 
                @(1,var, e->e.decl_specs[1] = "__device__" and IsBound(e.value) = false))); #collect none value device arrays
    ptr_length := Collect(code, @(1, func, e-> e.id = "init")); #getting sizes of device ptrs
    values_ptr := Collect(ptr_length, @(1,Value, e-> IsInt(e.v))); # getting sizes of device ptrs
    code := SubstTopDown(code, @(1,func, e->e.id <> "transform"), e->skip()); #removing init/destory
    #Print(opts.prettyPrint(code));
    code := SubstTopDown(code, @(1,func, e->e.id = "transform"), e->skip());# removing transform
    code := SubstTopDown(code, @(1,specifiers_func), e->let(g := Cond(IsBound(e.decl_specs) and e.decl_specs[1] = "__global__", ["extern \"C\" __global__"], e.decl_specs[1]), specifiers_func(g, e.ret, e.id, params, e.cmd))); #changing params to be all inputs
    # old_includes := opts.includes;
    old_skip := opts.unparser.skip;
    opts.unparser.skip := (self, o, i, is) >> Print("");
    # opts.includes := [];
    pts := PrintToString(opts.prettyPrint(code)); #print to string
    # opts.includes := old_includes;
    opts.unparser.skip := old_skip;
    x := 0;
    y := 1;
    Print("JIT BEGIN\n");
    for i in [1..Length(collection2)] do
        if IsPtrT(collection2[i].t) then
            Print(0, " ", collection2[i], " ", _unwrap(values_ptr[y]), " ", "pointer_", collection2[i].t.t.ctype, "\n");
            Print(3, " ", x, " ", _unwrap(values_ptr[y]), " ");
                if collection2[i].t.t.ctype = "int" then
                    Print(0," ");
                elif collection2[i].t.t.ctype = "float" then
                    Print(1, " ");
                elif collection2[i].t.t.ctype = "double" then
                    Print(2, " ");
                else
                    Print("how???\n");
                fi;
            Print("\n");
            x := x+1;
            y := y+1;
        elif IsArrayT(collection2[i].t) then
            Print(0, " ", collection2[i], " ", collection2[i].t.size, " ", collection2[i].t.t.ctype, "\n");
            Print(3, " ", x, " ", collection2[i].t.size, " "); 
                if collection2[i].t.t.ctype = "int" then
                    Print(0," ");
                elif collection2[i].t.t.ctype = "float" then
                    Print(1, " ");
                elif collection2[i].t.t.ctype = "double" then
                    Print(2, " ");
                else
                    Print("how???\n");
                fi;
            Print("\n");
            x := x+1;
        else
            Print("it got here how???\n");
        fi;
    od;
    for i in [1..Length(datas)] do
        if datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = false then
            Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", datas[i].var.t.t.ctype, "\n");
            Print(3, " ", x, " ", datas[i].var.t.size, " "); 
            if datas[i].var.t.t.ctype = "int" then
                Print(0," ");
            elif datas[i].var.t.t.ctype = "float" then
                Print(1, " ");
            elif datas[i].var.t.t.ctype = "double" then
                Print(2, " ");
            else
                Print("how???\n");
            fi;
            for j in [1..datas[i].var.t.size] do 
                Print(_unwrap(_unwrap(datas[i].value)[j]), " ");
            od;
            Print("\n");
            x := x+1;
        elif datas[1].var.decl_specs[1] = "__constant__" or (datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = true) then
            Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", "constant", "\n");
            Print(3, " ", x, " ", datas[i].var.t.size, " ", "3\n"); 
            x := x+1;
        else
            Print("how???\n");
        fi;
    od;
    for i in [1..Length(kernels)] do
        Print("2", " ", kernels[i].func, " ", _unwrap(kernels[i].dim_grid.x.value), " ", _unwrap(kernels[i].dim_grid.y.value), " ", _unwrap(kernels[i].dim_grid.z.value), " ", _unwrap(kernels[i].dim_block.x.value), 
        " ", _unwrap(kernels[i].dim_block.y.value), " ", _unwrap(kernels[i].dim_block.z.value));
        Print("\n");
    od;
    Print("------------------");
    Print(SubString(pts, 88, Length(pts)));#skip spiral gen comments and default includes, prints just kernel code
end;


#F PrintOpenCLJIT(<c>, <opts>)
#F    Prints metadata + generated code into parseable text file for FFTX jitting with OpenCL/Sycl
#F
PrintOpenCLJIT := function(code, opts)
    local pts, collection1, collection2, x, y, j, i, k, localkernels, code2, vars, datas, params, kernels, ckernels, values_ptr, ptr_length, var_t, old_includes, old_skip;
    kernels := Collect(code, cu_call); #get kernel names for input
    params := Collect(code, @(1, func, e-> e.id = "transform"))[1].params; #get launch params
    datas := Collect(code, data); #get device/constant arrays with value
    collection2 := Set(Collect(Collect(code, @(1,var, e-> IsArrayT(e.t) or IsPtrT(e.t) and IsBound(e.decl_specs) = true)), 
                @(1,var, e-> e.decl_specs[1] = "global" and IsBound(e.value) = false))); #collect none value device arrays
    collection1 := Set(Collect(Collect(code, @(1,var, e-> IsArrayT(e.t) or IsPtrT(e.t) and IsBound(e.decl_specs) = true)), 
                @(1,var, e-> e.decl_specs[1] = "local" and IsBound(e.value) = false))); #collect none value device arrays
    ptr_length := Collect(code, @(1, func, e-> e.id = "init")); #getting sizes of device ptrs
    values_ptr := Collect(ptr_length, @(1,Value, e-> IsInt(e.v))); # getting sizes of device ptrs
    code := SubstTopDown(code, @(1,func, e->e.id <> "transform"), e->skip()); #removing init/destory
    code := SubstTopDown(code, @(1,func, e->e.id = "transform"), e->skip());# removing transform
    # if Length(collection2) > 0 then 
    #     code.cmds[1] := decl([], code.cmds[1].cmd);
    #     for i in collection2 do
    #         if IsPtrT(i) then
    #             i.t.qualifiers[1] := "";
    #             code := SubstTopDown(code, @(1, specifiers_func, e-> i in Collect(e.cmd, var)), e-> let(Append(e.params, [i]),specifiers_func(e.decl_specs, e.ret, e.id, e.params, e.cmd)));
    #         else 
    #             var_t := var(i.id, TPtr(i.t.t));
    #             code := SubstTopDown(code, @(1, specifiers_func, e-> i in Collect(e.cmd, var)), e-> let(Append(e.params, [var_t]),specifiers_func(e.decl_specs, e.ret, e.id, e.params, e.cmd)));
    #         fi;
    #     od;
    # fi;
    # if Length(datas) > 0 then
    #     for i in datas do 
    #         var_t := var(i.var.id, TPtr(i.var.t.t));
    #         code := SubstTopDown(code, @(1, specifiers_func, e-> i.var in Collect(e.cmd, var)), e-> let(Append(e.params, [var_t]),specifiers_func(e.decl_specs, e.ret, e.id, e.params, e.cmd)));
    #     od;
    # fi;
    # code := SubstTopDown(code, data, e-> e.cmd);
    # code := SubstTopDown(code, @(1,specifiers_func), e->let(g := Cond(IsBound(e.decl_specs) and e.decl_specs[1] = "__global__", ["extern \"C\" __global__"], e.decl_specs[1]), specifiers_func(g, e.ret, e.id, e.params, e.cmd))); #changing params to be all inputs
    ckernels := Collect(code, specifiers_func); #get kernel signatures
    x := 0;
    y := 1;
    Print("JIT BEGIN\n");
    for i in collection2::collection1 do
        if IsPtrT(i.t) then
            Print(0, " ", i, " ", _unwrap(i.t.size), " ", "pointer_", i.t.t.ctype, " ", i.decl_specs[1], "\n");
            Print(3, " ", x, " ", _unwrap(i.t.size), " ");
                if i.t.t.ctype = "int" then
                    Print(0," ");
                elif i.t.t.ctype = "float" then
                    Print(1, " ");
                elif i.t.t.ctype = "double" then
                    Print(2, " ");
                else
                    Print("how???\n");
                fi;
            Print("\n");
            x := x+1;
            y := y+1;
        # elif IsArrayT(collection2[i].t) then
        #     Print(0, " ", collection2[i], " ", _unwrap(collection2[i].t.size), " ", collection2[i].t.t.ctype, "\n");
        #     Print(3, " ", x, " ", _unwrap(collection2[i].t.size), " "); 
        #         if collection2[i].t.t.ctype = "int" then
        #             Print(0," ");
        #         elif collection2[i].t.t.ctype = "float" then
        #             Print(1, " ");
        #         elif collection2[i].t.t.ctype = "double" then
        #             Print(2, " ");
        #         else
        #             Print("how???\n");
        #         fi;
        #     Print("\n");
        #     x := x+1;
        # else
        #     Print("it got here how???\n");
        fi;
    od;
    # for i in [1..Length(datas)] do
    #     if datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = false then
    #         Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", datas[i].var.t.t.ctype, "\n");
    #         Print(3, " ", x, " ", datas[i].var.t.size, " "); 
    #         if datas[i].var.t.t.ctype = "int" then
    #             Print(0," ");
    #         elif datas[i].var.t.t.ctype = "float" then
    #             Print(1, " ");
    #         elif datas[i].var.t.t.ctype = "double" then
    #             Print(2, " ");
    #         else
    #             Print("how???\n");
    #         fi;
    #         for j in [1..datas[i].var.t.size] do 
    #             Print(_unwrap(_unwrap(datas[i].value)[j]), " ");
    #         od;
    #         Print("\n");
    #         x := x+1;
    #     elif datas[1].var.decl_specs[1] = "__constant__" or (datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = true) then
    #         Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", "constant", "\n");
    #         Print(3, " ", x, " ", datas[i].var.t.size, " ", "3 ");
    #         for j in [1..Length(_unwrap(datas[i].value))] do
    #             Print(_unwrap(_unwrap(datas[i].value)[j]), " ");
    #         od;
    #         Print("\n"); 
    #         x := x+1;
    #     else
    #         Print("how???\n");
    #     fi;
    # od;
    for i in [1..Length(kernels)] do
        Print("2", " ", kernels[i].func, " ", _unwrap(kernels[i].dim_grid.z.value), " ", _unwrap(kernels[i].dim_grid.y.value), " ", _unwrap(kernels[i].dim_grid.x.value), " ", _unwrap(kernels[i].dim_block.z.value), 
        " ", _unwrap(kernels[i].dim_block.y.value), " ", _unwrap(kernels[i].dim_block.x.value));
        for j in ckernels[i].params do
            Print(" ", j);
        od;
        Print("\n");
    od;
    for i in [1..Length(params)] do 
        if not IsBound(params[i].t.qualifiers) then
            Error("all OpenCL kernel params require global or local tag\n");
        fi;
        Print("4 ", params[i].id);
        if IsBound(params[i].t.t) and IsBound(params[i].t.t.ctype) then
            Print(" pointer_", params[i].t.t.ctype, " ", i, "\n");
        else
            Print(" ", params[i].t.ctype, " ", i, "\n");
        fi;
    od;
    Print("------------------");
    # old_includes := opts.includes;
    old_skip := opts.unparser.skip;
    opts.unparser.skip := (self, o, i, is) >> Print("");
    # opts.includes := [];
    pts := PrintToString(opts.prettyPrint(code)); #print to string
    # opts.includes := old_includes;
    opts.unparser.skip := old_skip;
    Print(SubString(pts, 88, Length(pts)));#skip spiral gen comments and default includes, prints just kernel code
end;


#F PrintIRISMETAJIT(<c>, <opts>)
#F    Prints metadata + generated code into parseable text file for FFTX jitting with IRIS
#F
PrintIRISMETAJIT := function(code, opts)
    local pts, collection1, collection2, x, y, j, i, k, localkernels, code2, vars, datas, params, 
            kernels, ckernels, values_ptr, ptr_length, var_t, old_includes, old_skip, read_list,
            write_list, function_interest, local_size;
    kernels := Collect(code, cu_call); #get kernel names for input
    params := Collect(code, @(1, func, e-> e.id = "transform"))[1].params; #get launch params
    datas := Collect(code, data); #get device/constant arrays with value
    collection2 := Set(Collect(Collect(code, @(1,var, e-> IsArrayT(e.t) or IsPtrT(e.t) and IsBound(e.decl_specs) = true)), 
                @(1,var, e->e.decl_specs[1] = "__device__" and IsBound(e.value) = false))); #collect none value device arrays
    # ptr_length := Collect(code, @(1, func, e-> e.id = "init")); #getting sizes of device ptrs
    ptr_length := Collect(code, @(1, call, e-> e.args[1].id = "cudaMalloc" or e.args[1].id = "hipMalloc"));#getting sizes of device ptrs
    values_ptr := Collect(ptr_length, @(1,Value, e-> IsInt(e.v))); # getting sizes of device ptrs
    code := SubstTopDown(code, @(1,func, e->e.id <> "transform"), e->skip()); #removing init/destory
    code := SubstTopDown(code, @(1,func, e->e.id = "transform"), e->skip());# removing transform
    if Length(collection2) > 0 then 
        code.cmds[1] := decl([], code.cmds[1].cmd);
        for i in collection2 do
            if IsPtrT(i) then
                i.t.qualifiers[1] := "";
                code := SubstTopDown(code, @(1, specifiers_func, e-> i in Collect(e.cmd, var)), e-> let(Append(e.params, [i]),specifiers_func(e.decl_specs, e.ret, e.id, e.params, e.cmd)));
            else
                local_size := i.t.size; 
                var_t := var(i.id, TPtr(i.t.t));
                var_t.t.size := local_size;
                code := SubstTopDown(code, @(1, specifiers_func, e-> i in Collect(e.cmd, var)), e-> let(Append(e.params, [var_t]),specifiers_func(e.decl_specs, e.ret, e.id, e.params, e.cmd)));
            fi;
        od;
    fi;
    if Length(datas) > 0 then
        for i in datas do 
            var_t := var(i.var.id, TPtr(i.var.t.t));
            code := SubstTopDown(code, @(1, specifiers_func, e-> i.var in Collect(e.cmd, var)), e-> let(Append(e.params, [var_t]),specifiers_func(e.decl_specs, e.ret, e.id, e.params, e.cmd)));
        od;
    fi;
    code := SubstTopDown(code, data, e-> e.cmd);
    code := SubstTopDown(code, @(1,specifiers_func), e->let(g := Cond(IsBound(e.decl_specs) and e.decl_specs[1] = "__global__", ["extern \"C\" __global__"], e.decl_specs[1]), specifiers_func(g, e.ret, e.id, e.params, e.cmd))); #changing params to be all inputs
    ckernels := Collect(code, specifiers_func); #get kernel signatures
    x := 0;
    y := 1;
    Print("JIT BEGIN\n");
    for i in [1..Length(collection2)] do
        if IsPtrT(collection2[i].t) then
            Print(0, " ", collection2[i], " ", When(IsBound(collection2[i].t.size) and collection2[i].t.size <> 0, _unwrap(collection2[i].t.size), _unwrap(values_ptr[y])), " ", "pointer_", collection2[i].t.t.ctype, "\n");
            Print(3, " ", x, " ", When(IsBound(collection2[i].t.size) and collection2[i].t.size <> 0, _unwrap(collection2[i].t.size), _unwrap(values_ptr[y])), " ");
                if collection2[i].t.t.ctype = "int" then
                    Print(0," ");
                elif collection2[i].t.t.ctype = "float" then
                    Print(1, " ");
                elif collection2[i].t.t.ctype = "double" then
                    Print(2, " ");
                else
                    Print("how???\n");
                fi;
            Print("\n");
            x := x+1;
            y := y+1;
        elif IsArrayT(collection2[i].t) then
            Print(0, " ", collection2[i], " ", _unwrap(collection2[i].t.size), " ", collection2[i].t.t.ctype, "\n");
            Print(3, " ", x, " ", _unwrap(collection2[i].t.size), " "); 
                if collection2[i].t.t.ctype = "int" then
                    Print(0," ");
                elif collection2[i].t.t.ctype = "float" then
                    Print(1, " ");
                elif collection2[i].t.t.ctype = "double" then
                    Print(2, " ");
                else
                    Print("how???\n");
                fi;
            Print("\n");
            x := x+1;
        else
            Print("it got here how???\n");
        fi;
    od;
    for i in [1..Length(datas)] do
        if datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = false then
            Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", datas[i].var.t.t.ctype, "\n");
            Print(3, " ", x, " ",  datas[i].var.t.size, " "); 
            if datas[i].var.t.t.ctype = "int" then
                Print(0," ");
            elif datas[i].var.t.t.ctype = "float" then
                Print(1, " ");
            elif datas[i].var.t.t.ctype = "double" then
                Print(2, " ");
            else
                Print("how???\n");
            fi;
            Print("\n");
            x := x+1;
        elif datas[1].var.decl_specs[1] = "__constant__" or (datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = true) then
            Print(0, " ", datas[i].var, " ", Length(_unwrap(datas[1].value)), " ", "constant", "\n");
            Print(3, " ", x, " ", Length(_unwrap(datas[1].value)), " ", "3 ");
            for j in [1..Length(_unwrap(datas[i].value))] do
                Print(_unwrap(_unwrap(datas[i].value)[j]), " ");
            od;
            Print("\n"); 
            x := x+1;
        else
            Print("how???\n");
        fi;
    od;
    for i in [1..Length(kernels)] do
        Print("2", " ", kernels[i].func, " ", _unwrap(kernels[i].dim_grid.x.value), " ", _unwrap(kernels[i].dim_grid.y.value), " ", _unwrap(kernels[i].dim_grid.z.value), " ", _unwrap(kernels[i].dim_block.x.value), 
        " ", _unwrap(kernels[i].dim_block.y.value), " ", _unwrap(kernels[i].dim_block.z.value));
        # function_interest := Collect(code, @(1, specifiers_func, e-> e.id = kernels[i].func));
        for j in ckernels[i].params do
            Print(" ", j);
            read_list := Collect(code, @(1, assign, e-> j in Collect(e.exp, var)));
            write_list := Collect(code, @(1, assign, e-> j in Collect(e.loc, var)));
            if read_list <> [] and write_list = [] then
                Print(" -1");
            elif read_list = [] and write_list <> [] then
                Print(" -2");
            elif read_list <> [] and write_list <> [] then
                Print(" -3");
            else
                Error("value is never used but in argument list?");
            fi;
        od;
        Print("\n");
    od;
    for i in [1..Length(params)] do 
        Print("4 ", params[i].id);
        if IsBound(params[i].t.t) and IsBound(params[i].t.t.ctype) then
            Print(" pointer_", params[i].t.t.ctype, "\n");
        else
            Print(" ", params[i].t.ctype, "\n");
        fi;
    od;
    Print("------------------");
    # old_includes := opts.includes;
    old_skip := opts.unparser.skip;
    opts.unparser.skip := (self, o, i, is) >> Print("");
    # opts.includes := [];
    pts := PrintToString(opts.prettyPrint(code)); #print to string
    # opts.includes := old_includes;
    opts.unparser.skip := old_skip;
    Print(SubString(pts, 88, Length(pts)));#skip spiral gen comments and default includes, prints just kernel code
end;



#F PrintIRISJIT(<c>, <opts>)
#F    Prints generated code into parseable text file for FFTX jitting with IRIS
#F
PrintIRISJIT := function(code, opts)
    local pts, collection1, collection2, x, y, j,  i, cg, code2, vars, datas, params, kernels, ckernels, values_ptr, ptr_length, var_t, old_includes, old_skip;
    # kernels := Collect(code, cu_call); #get kernel names for input
    # ckernels := Collect(code, specifiers_func); #get kernel signatures
    params := Collect(code, @(1, func, e-> e.id = "transform"))[1].params; #get launch params
    datas := Collect(code, data); #get device/constant arrays with value
    collection2 := Set(Collect(Collect(code, @(1,var, e-> IsBound(e.decl_specs) = true)), 
                @(1,var, e->e.decl_specs[1] = "__device__" and IsBound(e.value) = false))); #collect none value device arrays
    # ptr_length := Collect(code, @(1, func, e-> e.id = "init")); #getting sizes of device ptrs
    # values_ptr := Collect(ptr_length, @(1,Value, e-> IsInt(e.v))); # getting sizes of device ptrs
    code := SubstTopDown(code, @(1,func, e->e.id <> "transform"), e->skip()); #removing init/destory
    #Print(opts.prettyPrint(code));
    code := SubstTopDown(code, @(1,func, e->e.id = "transform"), e->skip());# removing transform
        # Error();
    if Length(collection2) > 0 then 
        code.cmds[1] := decl([], code.cmds[1].cmd);
        for i in collection2 do
            if IsPtrT(i) then
                i.t.qualifiers[1] := "";
                Append(params, [i]);
            else 
                var_t := var(i.id, TPtr(i.t.t));
                Append(params, [var_t]);
            fi;
                
        od;
    fi;
    if Length(datas) > 0 then
        for i in datas do 
            var_t := var(i.var.id, TPtr(i.var.t.t));
            Append(params, [var_t]);
        od;
    fi;
            
    code := SubstTopDown(code, data, e-> e.cmd);
    code := SubstTopDown(code, @(1,specifiers_func), e->let(g := Cond(IsBound(e.decl_specs) and e.decl_specs[1] = "__global__", ["extern \"C\" __global__"], e.decl_specs[1]), specifiers_func(g, e.ret, e.id, params, e.cmd))); #changing params to be all inputs
    old_includes := opts.includes;
    old_skip := opts.unparser.skip;
    opts.unparser.skip := (self, o, i, is) >> Print("");
    opts.includes := [];
    pts := PrintToString(opts.prettyPrint(code)); #print to string
    opts.includes := old_includes;
    opts.unparser.skip := old_skip;
    # x := 0;
    # y := 1;
    # Print("JIT BEGIN\n");
    # for i in [1..Length(collection2)] do
    #     if IsPtrT(collection2[i].t) then
    #         Print(0, " ", collection2[i], " ", _unwrap(values_ptr[y]), " ", "pointer_", collection2[i].t.t.ctype, "\n");
    #         Print(3, " ", x, " ", _unwrap(values_ptr[y]), " ");
    #             if collection2[i].t.t.ctype = "int" then
    #                 Print(0," ");
    #             elif collection2[i].t.t.ctype = "float" then
    #                 Print(1, " ");
    #             elif collection2[i].t.t.ctype = "double" then
    #                 Print(2, " ");
    #             else
    #                 Print("how???\n");
    #             fi;
    #         Print("\n");
    #         x := x+1;
    #         y := y+1;
    #     elif IsArrayT(collection2[i].t) then
    #         Print(0, " ", collection2[i], " ", collection2[i].t.size, " ", collection2[i].t.t.ctype, "\n");
    #         Print(3, " ", x, " ", collection2[i].t.size, " "); 
    #             if collection2[i].t.t.ctype = "int" then
    #                 Print(0," ");
    #             elif collection2[i].t.t.ctype = "float" then
    #                 Print(1, " ");
    #             elif collection2[i].t.t.ctype = "double" then
    #                 Print(2, " ");
    #             else
    #                 Print("how???\n");
    #             fi;
    #         Print("\n");
    #         x := x+1;
    #     else
    #         Print("it got here how???\n");
    #     fi;
    # od;
    # for i in [1..Length(datas)] do
    #     if datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = false then
    #         Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", datas[i].var.t.t.ctype, "\n");
    #         Print(3, " ", x, " ", datas[i].var.t.size, " "); 
    #         if datas[i].var.t.t.ctype = "int" then
    #             Print(0," ");
    #         elif datas[i].var.t.t.ctype = "float" then
    #             Print(1, " ");
    #         elif datas[i].var.t.t.ctype = "double" then
    #             Print(2, " ");
    #         else
    #             Print("how???\n");
    #         fi;
    #         for j in [1..datas[i].var.t.size] do 
    #             Print(_unwrap(_unwrap(datas[i].value)[j]), " ");
    #         od;
    #         Print("\n");
    #         x := x+1;
    #     elif datas[1].var.decl_specs[1] = "__constant__" or (datas[1].var.decl_specs[1] = "__device__" and IsBound(datas[1].value) = true) then
    #         Print(0, " ", datas[i].var, " ", datas[i].var.t.size, " ", "constant", "\n");
    #         Print(3, " ", x, " ", datas[i].var.t.size, " ", "3\n"); 
    #         x := x+1;
    #     else
    #         Print("how???\n");
    #     fi;
    # od;
    # for i in [1..Length(kernels)] do
    #     Print("2", " ", kernels[i].func, " ", _unwrap(kernels[i].dim_grid.x.value), " ", _unwrap(kernels[i].dim_grid.y.value), " ", _unwrap(kernels[i].dim_grid.z.value), " ", _unwrap(kernels[i].dim_block.x.value), 
    #     " ", _unwrap(kernels[i].dim_block.y.value), " ", _unwrap(kernels[i].dim_block.z.value));
    #     Print("\n");
    # od;
    # Print("------------------");
    Print(SubString(pts, 88, Length(pts)));#skip spiral gen comments and default includes, prints just kernel code
end;
