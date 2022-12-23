##  preamble for spiral_gpu backend

Load(fftx);
ImportAll(fftx);
ImportAll(simt);
Load(jit);
Import(jit);


# use the configuration for small mutidimensional real convolutions
# later we will have to auto-derive the correct options class

conf := FFTXGlobals.defaultHIPConf();

t := 160;

##  end of preamble
var_1:= var("var_1", BoxND([t,t,t], TReal));
var_2:= var("var_2", BoxND([t,t,t], TReal));
var_3:= var("var_3", BoxND([t,t,t], TReal));
var_2:= X;
var_3:= Y;
symvar := var("sym", TPtr(TReal));
transform:= TFCall(TDecl(TDAG([
   TDAGNode(TTensorI(MDDFT([t,t,t],-1),1,APar, APar), var_1,var_2),
   TDAGNode(TTensorI(MDDFT([t,t,t],1),1,APar, APar), var_3,var_1),

]),
   [var_1]
),
rec(fname:="imddft_spiral", params:= [symvar])
);
prefix:="imddft";
##  Start of codegen -- for GPU
if 1 = 1 then
opts:=conf.getOpts(transform);
tt:= opts.tagIt(transform);
if(IsBound(fftx_includes)) then opts.includes:=fftx_includes; fi;
c:=opts.fftxGen(tt);
fi;
#opts.prettyPrint(c);
##changed cpp to txt for jit parsing
# PrintTo(prefix::".fftx.source.cpp",opts.prettyPrint(c));
PrintHIPJIT2(c,opts);
PrintTo("imddftgenerated"::".txt", PrintHIPJIT2(c, opts));