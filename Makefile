#  This package is free software; you can redistribute it and/or
#  modify it under terms of the GNU General Public License as
#  published by the Free Software Foundation; either version 3, or
#  (at your option) any later version.  It is distributed in the
#  hope that it will be useful, but WITHOUT ANY WARRANTY; without
#  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#  PARTICULAR PURPOSE.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; see the file COPYING3.  If not, see
#  <http://www.gnu.org/licenses/>.
#
#  Copyright Simon Wright <simon@pushface.org>

# This Makefile is part of the Ada 2005 Math Extensions package, and
# is used to build, test, clean, construct releases and upload the
# documentation.

all::

install::

clean::

dist::

# Compute the prefix of the current GNAT installation
prefix ?= $(realpath $(dir $(shell which gnatls))..)

# Work out where to install the GPR
debian = $(and $(wildcard /etc/debian_version),$(filter $(prefix),/usr))
GPR_INSTALL_SUBDIR = $(if $(debian),share/ada/adainclude,lib/gnat)

all:: force
	gprbuild -p -P gnat_math_extensions
	cd test; make all

install::
	gprinstall					\
	  -f						\
	  --prefix=$(prefix)				\
	  -P gnat_math_extensions.gpr			\
	  --install-name=gnat_math_extensions		\
	  --project-subdir=$(GPR_INSTALL_SUBDIR)	\
	  -XLIBRARY_TYPE=static				\
	  --mode=dev					\
	  --create-missing-dirs				\
	  --build-var=LIBRARY_TYPE			\
	  --build-name=static

clean::
	gprclean -f -P gnat_math_extensions.gpr
	-rm -rf .build
	cd test; make clean

# Used to construct release IDs (eg, gnat-math-extn-20100731). You can
# set the whole thing from the command line -- for example, if
# creating a patch release.
DATE = $(shell date +%Y%m%d)

dist:: gnat-math-extn-$(DATE).tar.gz gnat-math-extn-$(DATE).zip

DISTRIBUTION_FILES =				\
  CHANGES					\
  Makefile					\
  README					\
  gnat_math_extensions.gpr			\
  src/ada_numerics-float_arrays.ads		\
  src/ada_numerics-generic_arrays.adb		\
  src/ada_numerics-generic_arrays.ads		\
  src/ada_numerics-long_float_arrays.ads	\
  src/ada_numerics.ads

DISTRIBUTION_FILES +=					\
  test/Makefile						\
  test/lapack_version.adb				\
  test/lapack_version.ads				\
  test/tests.gpr					\
  test/tests.ads					\
  test/tests-main.adb					\
  test/tests-complex_general_eigenvalues.adb		\
  test/tests-complex_general_eigenvalues.ads		\
  test/tests-real_general_eigenvalues.adb		\
  test/tests-real_general_eigenvalues.ads		\
  test/tests-complex_generalized_eigenvalues.adb	\
  test/tests-complex_generalized_eigenvalues.ads	\
  test/tests-real_generalized_eigenvalues.adb		\
  test/tests-real_generalized_eigenvalues.ads

gnat-math-extn-$(DATE).tar.gz: gnat-math-extn-$(DATE)
	rm -f $@
	tar zcvf $@ $</

gnat-math-extn-$(DATE).zip: gnat-math-extn-$(DATE)
	rm -f $@
	zip -r -9 $@ $</*

gnat-math-extn-$(DATE): $(DISTRIBUTION_FILES)
	rm -rf $@
	mkdir $@
	tar cf - $^ | tar xvf - -C $@


# Documentation upload to SF

SFUSER ?= simonjwright

upload-docs:
	rsync \
	  --compress \
	  --copy-unsafe-links \
	  --cvs-exclude \
	  --delete \
	  --perms \
	  --recursive \
	  --rsh=ssh \
	  --times \
	  --update \
	  --verbose \
	  doc/*.{css,html} \
	  $(SFUSER),gnat-math-extn@web.sourceforge.net:htdocs/index.html


.PHONY: all install clean dist upload-docs force
