# Makefile

FC = gfortran
FLINKER = $(FC)
ARCH = ar
ARCH_FLAGS = -rsc
DYN_LINK_FLAGS = -fPIC -shared

.PHONY: all

all: rand_rot_trans

.PHONY: rand_rot_trans

rand_rot_trans: rand_rot_trans.exe

rand_rot_trans.exe: rand_rot_trans.o molecule_operation.o quaternion.o progress_bar.o rand_init.o
	@echo Linking $@ ...
	$(FLINKER) -o $@ $^

rand_rot_trans.o: rand_rot_trans.f90 molecule_module.mod \
quaternion_module.mod progress_bar_module.mod rand_init_module.mod
	@echo Compiling $@ ...
	$(FC) -o $@ -fno-realloc-lhs -c $<
	rem $(FC) -o $@ -c $<

molecule_module.mod: molecule_operation.o

molecule_operation.o: molecule_operation.f90 quaternion_module.mod
	@echo Compiling $@ ...
	$(FC) -o $@ -c $<

quaternion_module.mod: quaternion.o

quaternion.o: quaternion.f90
	@echo Compiling $@ ...
	$(FC) -o $@ -c $<

progress_bar_module.mod: progress_bar.o

progress_bar.o: progress_bar.f90
	@echo Compiling $@ ...
	$(FC) -o $@ -c $<

rand_init_module.mod: rand_init.o

rand_init.o: rand_init.f90
	@echo Compiling $@ ...
	$(FC) -o $@ -c $<

.PHONY: clean

clean: clean_tmp
	-del rand_rot_trans.exe 1> NUL 2> NUL

.PHONY: clean_tmp

clean_tmp:
	-del rand_init_module.mod rand_init.o 1> NUL 2> NUL
	-del progress_bar_module.mod progress_bar.o 1> NUL 2> NUL
	-del quaternion_module.mod quaternion.o 1> NUL 2> NUL
	-del molecule_module.mod molecule_operation.o 1> NUL 2> NUL
	-del rand_rot_trans.o 1> NUL 2> NUL

