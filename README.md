# bf
Fast Haskell brainfuck interpreter

Installation: 

 - Install [Stack](http://docs.haskellstack.org/en/stable/README/)
 - In the package directory run `stack install`
 
Note: if you don't have [LLVM](http://llvm.org/) 3.5 installed and on the PATH, you have to instead install with `stack install --ghc-options="-fasm"`. In this case, the program will be somewhat slower. 
