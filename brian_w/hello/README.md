# Hello World

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
