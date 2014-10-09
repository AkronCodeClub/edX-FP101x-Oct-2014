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
