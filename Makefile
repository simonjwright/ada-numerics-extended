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

# This Makefile is part of the GNAT Math Extensions package, and
# is used to build, test, and clean.

all::

install::

clean::

# Compute the prefix of the current GNAT installation
prefix ?= $(realpath $(dir $(shell which gnatls))..)

# Work out where to install the GPR
debian = $(and $(wildcard /etc/debian_version),$(filter $(prefix),/usr))
GPR_INSTALL_SUBDIR = $(if $(debian),share/ada/adainclude,lib/gnat)

all:: force
	gprbuild -j0 -P gnat_math_extensions
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

.PHONY: all install clean dist upload-docs force
