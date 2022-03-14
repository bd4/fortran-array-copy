.PHONY: all
all: build/array_copy_ifort build/array_copy_ifx build/array_copy_gfortran build/array_copy_ifort_debug build/array_copy_ifx_debug build/array_copy_gfortran_debug

.PHONY: mkbuilddir
mkbuilddir:
	@mkdir -p build

build/array_copy_ifort: array_copy.f90 mkbuilddir
	ifort -O2 -g -check arg_temp_created $< -o $@

build/array_copy_ifx: array_copy.f90 mkbuilddir
	ifx -O2 -g -check arg_temp_created $< -o $@

build/array_copy_gfortran: array_copy.f90 mkbuilddir
	gfortran -O2 -g -fcheck-array-temporaries $< -o $@

build/array_copy_ifort_debug: array_copy.f90 mkbuilddir
	ifort -g -check arg_temp_created $< -o $@

build/array_copy_ifx_debug: array_copy.f90 mkbuilddir
	ifx -g -check arg_temp_created $< -o $@

build/array_copy_gfortran_debug: array_copy.f90 mkbuilddir
	gfortran -g -fcheck-array-temporaries $< -o $@

build/pointer_bounds_associate_ifort: pointer_bounds_associate.f90 mkbuilddir
	ifort -O2 -g -check arg_temp_created $< -o $@

build/pointer_bounds_associate_ifx: pointer_bounds_associate.f90 mkbuilddir
	ifx -O2 -g -check arg_temp_created $< -o $@

build/pointer_bounds_associate_gfortran: pointer_bounds_associate.f90 mkbuilddir
	gfortran -O2 -g -fcheck-array-temporaries $< -o $@

.PHONY: clean
clean:
	@rm -f build/*
