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

generic
package Ada.Numerics.Generic_Complex_Arrays.Extensions is

   pragma Pure (Ada.Numerics.Generic_Complex_Arrays.Extensions);

   --  Obtain the eigenvalues of a non-hermitian complex matrix.
   function Eigenvalues (A : Complex_Matrix) return Complex_Vector;

   --  Obtain the eigenvalues of a non-symmetric real matrix.
   function Eigenvalues (A : Real_Matrix) return Complex_Vector;

end Ada.Numerics.Generic_Complex_Arrays.Extensions;
