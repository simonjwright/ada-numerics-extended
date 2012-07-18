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
# is used to construct releases and upload the documentation..

all::

dist::

# Used to construct release IDs (eg, gnat-math-extn-20100731). You can
# set the whole thing from the command line -- for example, if
# creating a patch release.
DATE = $(shell date +%Y%m%d)

dist:: gnat-math-extn-$(DATE).tar.gz gnat-math-extn-$(DATE).zip

DISTRIBUTION_FILES =					\
  README						\
  CHANGES						\
  gnat_math_extensions.gpr				\
  src/ada_numerics-generic_arrays.adb			\
  src/ada_numerics-generic_arrays.ads			\
  src/ada_numerics.ads

DISTRIBUTION_FILES +=				\
  test/demo_extensions.adb			\
  test/demo_extensions.gpr

DISTRIBUTION_FILES +=					\
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


.PHONY: dist upload-docs
