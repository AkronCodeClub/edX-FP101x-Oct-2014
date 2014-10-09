EXE = parallel
OBJS = parallel.o
INTERFACES = $(OBJS:%.o=%.hi)

HSFLAGS = -O2 -threaded -rtsopts

.PHONY : clean

$(EXE) : $(OBJS)
	ghc $(HSFLAGS) -o $@ $^

%.o : %.hs
	ghc $(HSFLAGS) -o $@ -c $<

clean :
	rm -f $(EXE) $(OBJS) $(INTERFACES)
