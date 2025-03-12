FC=gfortran
EXE=khun.x
LIB=libkhun.a

.PHONY: driver
driver:
	$(FC) -c mod_khun.f90
	$(FC) -c main.f90
	$(FC) -o $(EXE) mod_khun.o main.o

.PHONY: lib
lib:
	rm -f $(LIB)
	$(FC) -c mod_khun.f90
	ar cr $(LIB) mod_khun.o

.PHONY: all
all: fullclean driver lib

.PHONY: fullclean
fullclean:
	rm -f *.o *.a *.mod *.x

.PHONY: clean
clean:
	rm -f *.o *.mod

