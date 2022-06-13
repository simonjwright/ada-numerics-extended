# GNAT Math Extensions #

The purpose of this project is to provide additional matrix capabilities beyond those defined in the Standard Library (Annex G of the ARM).

The Standard defines real and complex matrix and vector operations. Not every possible operation is supported, so for example only symmetric real or [hermitian](http://en.wikipedia.org/wiki/Hermitian_matrix) complex matrices can be solved.

The implementations, which are only for GNAT, are in the form of bindings to the [LAPACK](https://www.netlib.org/lapack) and [BLAS](https://www.netlib.org/blas/") libraries which are widely available if not already provided on all operating systems.

## Prerequisites ##

BLAS and LAPACK are required to be on the library lookup path (so that the linker switches `-llapack -lblas` find them).

On macOS, they are part of the vecLib framework (though, as of Monterey 12.3.1, LAPACK is still at version 3.2.1).

On Debian, check out `liblapack3`, `libblas3`.

On Windows, check out LAPACK for Windows at http://icl.cs.utk.edu/lapack-for-windows/lapack/

Installation
============

The software is provided in a directory structure where

* `src/`  contains the library code,
* `test/` contains unit tests.

Execute "make". This will build a static library for use in your own programs, and a test program (test/tests) using AUnit.

In order to use the library within your own code, you need to use a GPR referencing the extensions' GPR, `gnat_math_extensions`. You can

* say `with "gnat_math_extensions";` and include `/where/ever/gnat_math_extensions` in your `GPR_PROJECTS_PATH` environment variable; or

* say `with "/where/ever/gnat_math_extensions";` (this path can be relative, if you like).

* install with your compiler by saying `make install` (perhaps more likely, `sudo make install`). You can then say `with "gnat_math_extensions";`

Testing
=======

Unit tests are based on AUnit version 3, which is expected to be installed.
