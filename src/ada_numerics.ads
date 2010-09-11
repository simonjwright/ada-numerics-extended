--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version.  It is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are
--  granted additional permissions described in the GCC Runtime
--  Library Exception, version 3.1, as published by the Free Software
--  Foundation.
--
--  You should have received a copy of the GNU General Public License
--  and a copy of the GCC Runtime Library Exception along with this
--  program; see the files COPYING3 and COPYING.RUNTIME respectively.
--  If not, see <http://www.gnu.org/licenses/>.
--
--  Copyright Simon Wright <simon@pushface.org>

pragma License (Modified_GPL);

--  This package is the root of a set of packages which extend Ada
--  2005's support for matrix operations.
--
--  The packages were originally rooted under the standard
--  Ada.Numerics, but this has disadvantages: first, that adding
--  packages within the standard library requires unusual compiler
--  options, and second, it adds an unwarranted air of authenticity to
--  the work.

package Ada_Numerics is

   pragma Pure (Ada_Numerics);

end Ada_Numerics;
