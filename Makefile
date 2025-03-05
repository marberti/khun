FC=gfortran
EXE=khun.x

.PHONY: default
default:
	$(FC) -c mod_khun.f90
	$(FC) -c main.f90
	$(FC) -o $(EXE) mod_khun.o main.o

.PHONY: clean
clean:
	rm -f *.o *.mod

