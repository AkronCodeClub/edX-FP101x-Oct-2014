# Hello, World

`hello.hs` is the source file. It contains the following line of code

```haskell
main = putStrLn "Hello, world!"
```

The `Makefile` describes, in a little detail, the recipe for constructing an executable from the source file.

```make
EXE = hello
OBJS = hello.o
INTERFACES = $(OBJS:%.o=%.hi)

.PHONY : clean

$(EXE) : $(OBJS)
	ghc -o $@ $^

%.o : %.hs
	ghc -o $@ -c $<

clean :
	rm -f $(EXE) $(OBJS) $(INTERFACES)
```

The build process generates a `hello.o` from the `hello.hs`. There is also a `hello.hi` produced as a side-effect. The .o file is the *traditional* object file (compiled code without the executable header); the .hi is a haskell interface file, which contains all the exported symbols from our Haskell module and is used when another module references this one. The compiler (`ghc`) syntax is very similar to `gcc`.

My `hello.hs` also contains some typing information which implies that main takes no paramters and retuns an IO aciton.

```haskell
main :: IO ()
```
