# spiral-package-jit
Spiral package supporting Just-In-Time (aka RTC) Compilation

#Usage
This package is dependent on spiral-package-fftx and spiral-package-simt 
these must be installed in order to get correct functionality

#Loading into SPIRAL shell
to properly use JIT you must load in the following manner: 

Load(fftx);
ImportAll(fftx);
ImportAll(simt);
Load(jit);
Import(jit);