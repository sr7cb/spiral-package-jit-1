##  preamble for spiral_gpu backend

Load(fftx);
ImportAll(fftx);
Load(jit);
Import(jit);
ImportAll(simt);

# use the configuration for small mutidimensional real convolutions
# later we will have to auto-derive the correct options class

conf := FFTXGlobals.defaultHIPConf();;

##  end of preamble
var_1:= var("var_1", BoxND([0,0,0], TReal));
var_2:= var("var_2", BoxND([24,32,40], TReal));
var_3:= var("var_3", BoxND([24,32,40], TReal));
var_2:= X;
var_3:= Y;
symvar := var("sym", TPtr(TReal));
transform:= TFCall(TDecl(TDAG([
   TDAGNode(TTensorI(MDDFT([24,32,40],-1),1,APar, APar), var_3,var_2),

]),
   [var_1]
),
rec(fname:="mddft_spiral", params:= [symvar])
);
prefix:="mddft";
##  Start of codegen -- for GPU

opts:=conf.getOpts(transform);
tt:= opts.tagIt(transform);
if(IsBound(fftx_includes)) then opts.includes:=fftx_includes; fi;
c:=opts.fftxGen(tt);

##changed cpp to txt for jit parsing
#PrintTo(prefix::".fftx.source.cpp",opts.prettyPrint(c));
PrintTo("mddftgenerated"::".txt", PrintHIPJIT(c, opts));