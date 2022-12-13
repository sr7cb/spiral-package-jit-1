##  preamble for spiral_hip backend

Load(fftx);
ImportAll(fftx);
ImportAll(simt);
Load(jit);
Import(jit);
# use the configuration for small mutidimensional real convolutions
# later we will have to auto-derive the correct options class

conf := FFTXGlobals.defaultHIPConf();

##  end of preamble
var_1:= var("var_1", BoxND([128,128,181], TReal));
var_2:= var("var_2", BoxND([128,128,181], TReal));
var_3:= var("var_3", BoxND([128,128,360], TReal));
var_4:= var("var_4", BoxND([128,128,360], TReal));
var_5:= var("var_5", BoxND([128,128,181], TReal));
var_3:= X;
var_4:= Y;
symvar := var("sym", TPtr(TReal));
transform:= TFCall(TDecl(TDAG([
    TDAGNode(MDPRDFT([128,128,360],-1), var_1,var_3),
    TDAGNode(Diag(diagTensor(FDataOfs(symvar,2965504,0),fConst(TReal, 2, 1))), var_2,var_1),
    TDAGNode(IMDPRDFT([128,128,360],1), var_4,var_2),

]),
   [var_1,var_2]
),
rec(fname:="rconv3_spiral", params:= [symvar])
);
prefix:="rconv3";
##  Start of codegen -- for HIP

opts:=conf.getOpts(transform);
tt:= opts.tagIt(transform);
if(IsBound(fftx_includes)) then opts.includes:=fftx_includes; fi;
c:=opts.fftxGen(tt);

#PrintTo(prefix::".fftx.source.cpp",opts.prettyPrint(c));
PrintTo("rconvgenerated"::".txt", PrintHIPJIT(c, opts));