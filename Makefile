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

# This Makefile is distributed with the Ada 2005 Math Extensions
# package.

all::

dist::

# Used to construct release IDs (eg, gnat-math-extn-20100731). You can
# set the whole thing from the command line -- for example, if
# creating a patch release.
DATE = $(shell date +%Y%m%d)

dist:: gnat-math-extn-$(DATE).tar.gz gnat-math-extn-$(DATE).zip

DISTRIBUTION_FILES =						\
  README							\
  src/ada-numerics-generic_complex_arrays-extensions.adb	\
  src/ada-numerics-generic_complex_arrays-extensions.ads	\
  src/ada_math_build.gpr

DISTRIBUTION_FILES +=				\
  test/test_extensions.adb			\
  test/test_extensions.gpr

DISTRIBUTION_FILES +=				\
  test/aunit.gpr				\
  test/tests.gpr				\
  test/tests.ads				\
  test/tests-main.adb				\
  test/tests-complex_general_eigenvalues.adb	\
  test/tests-complex_general_eigenvalues.ads

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

.PHONY: dist
