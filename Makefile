# Makefile

SHELL = cmd
FC = gfortran
FLINKER = $(FC)
ARCH = ar
ARCH_FLAGS = -rsc
DYN_LINK_FLAGS = -fPIC -shared

.PHONY: all

all: rand_rot_trans

.PHONY: rand_rot_trans

rand_rot_trans: rand_rot_trans.exe

rand_rot_trans.exe: rand_rot_trans.obj molecule_operation.obj quaternion.obj \
vector_operation.obj progress_bar.obj rand_init.obj constant.obj
	@echo Linking $@ ...
	$(FLINKER) -o $@ $^ -static

rand_rot_trans.obj: rand_rot_trans.f90 molecule_module.mod \
quaternion_module.mod progress_bar_module.mod rand_init_module.mod
	@echo Compiling $@ ...
	$(FC) -o $@ -fno-realloc-lhs -c $<
	rem $(FC) -o $@ -c $<

molecule_module.mod: molecule_operation.obj

molecule_operation.obj: molecule_operation.f90 quaternion_module.mod
	@echo Compiling $@ ...
	$(FC) -o $@ -c $<

quaternion_module.mod: quaternion.obj

quaternion.obj: quaternion.f90 vector_operation_module.mod constant_module.mod
	@echo Compiling $@ ...
	$(FC) -o $@ -c $<

vector_operation_module.mod: vector_operation.obj 

vector_operation.obj: vector_operation.f90 constant_module.mod
	@echo Compiling $@ ...
	$(FC) -o $@ -c $<

progress_bar_module.mod: progress_bar.obj

progress_bar.obj: progress_bar.f90
	@echo Compiling $@ ...
	$(FC) -o $@ -c $< -fallow-invalid-boz

rand_init_module.mod: rand_init.obj

rand_init.obj: rand_init.f90
	@echo Compiling $@ ...
	$(FC) -o $@ -c $<

constant_module.mod: constant.obj

constant.obj: constant.f90
	@echo Compiling $@ ...
	$(FC) -o $@ -c $<

.PHONY: clean

clean: clean_tmp
	-del rand_rot_trans.exe 1> NUL 2> NUL

.PHONY: clean_tmp

clean_tmp:
	-del constant_module.mod constant.obj 1> NUL 2> NUL
	-del rand_init_module.mod rand_init.obj 1> NUL 2> NUL
	-del progress_bar_module.mod progress_bar.obj 1> NUL 2> NUL
	-del quaternion_module.mod quaternion.obj 1> NUL 2> NUL
	-del vector_operation_module.mod vector_operation.obj 1> NUL 2> NUL
	-del molecule_module.mod molecule_operation.obj 1> NUL 2> NUL
	-del rand_rot_trans.obj 1> NUL 2> NUL

