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

#F PrintHIPJIT(<c>, <opts>)
#F    Prints generated code into parseable text file for FFTX jitting with HIP
#F
PrintHIPJIT := function(code, opts)
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

#F PrintIRISMETAJIT(<c>, <opts>)
#F    Prints metadata + generated code into parseable text file for FFTX jitting with IRIS
#F
PrintIRISMETAJIT := function(code, opts)
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
            Print(3, " ", x, " ", datas[i].var.t.size, " ", "3 ");
            # Error();
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



#F PrintIRISJIT(<c>, <opts>)
#F    Prints generated code into parseable text file for FFTX jitting with IRIS
#F
PrintIRISJIT := function(code, opts)
    local pts, collection1, collection2, x, y, j,  i, cg, code2, vars, datas, params, kernels, ckernels, values_ptr, ptr_length;
    # kernels := Collect(code, cu_call); #get kernel names for input
    # ckernels := Collect(code, specifiers_func); #get kernel signatures
    params := Collect(code, @(1, func, e-> e.id = "transform"))[1].params; #get launch params
    datas := Collect(code, data); #get device/constant arrays with value
    collection2 := Set(Collect(Collect(code, @(1,var, e-> IsPtrT(e.t) and IsBound(e.decl_specs) = true)), 
                @(1,var, e->e.decl_specs[1] = "__device__" and IsBound(e.value) = false))); #collect none value device arrays
    # ptr_length := Collect(code, @(1, func, e-> e.id = "init")); #getting sizes of device ptrs
    # values_ptr := Collect(ptr_length, @(1,Value, e-> IsInt(e.v))); # getting sizes of device ptrs
    code := SubstTopDown(code, @(1,func, e->e.id <> "transform"), e->skip()); #removing init/destory
    #Print(opts.prettyPrint(code));
    code := SubstTopDown(code, @(1,func, e->e.id = "transform"), e->skip());# removing transform
    if Length(collection2) > 0 then 
        code.cmds[1] := decl([], code.cmds[1].cmd);
        for i in collection2 do
            i.t.qualifiers[1] := "";
        od;
        Append(params, collection2);
    fi;
    if Length(datas) > 0 then
        for i in datas do 
            var_t := var(i.var.id, TPtr(i.var.t.t));
            Append(params, [var_t]);
        od;
    fi;
            
    code := SubstTopDown(code, data, e-> e.cmd);
    code := SubstTopDown(code, @(1,specifiers_func), e->specifiers_func(e.decl_specs, e.ret, e.id, params, e.cmd)); #changing params to be all inputs
    pts := PrintToString(opts.prettyPrint(code)); #print to string
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
    for i in [5..(Length(pts)-1)] do
        # if Length(pts[i]) > 4 and SubString(pts[i], 0, 8) = "#include" then
        #     Print("");
        if pts[i] = "/* skip */" then 
            Print("");
        elif Length(pts[i]) > 4 and SubString(pts[i],0,10) = "__global__" then 
            Print("extern \"C\" ", pts[i],"\n");
        else
            Print(pts[i], "\n");
        fi;
    od;
end;