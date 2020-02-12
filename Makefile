.phony: all clean test

FC := gfortran
FFLAGS := -O3 -Wall
PROGS := example testhash benchmark

all: $(PROGS)

clean:
	$(RM) $(PROGS) *.o *.mod

test: testhash
	./testhash

# Compilation rules
%: %.f90
	$(FC) -o $@ $^ $(FFLAGS)
%.o: %.f90
	$(FC) -c -o $@ $^ $(FFLAGS)

# Dependencies
$(PROGS).o: m_murmur3.o
$(PROGS): m_murmur3.o
