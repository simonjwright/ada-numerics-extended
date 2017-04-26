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

with Ada.Numerics.Generic_Complex_Arrays;

generic
   with package Complex_Arrays
      is new Ada.Numerics.Generic_Complex_Arrays (<>);
package Ada_Numerics.Generic_Arrays is
   pragma Pure (Generic_Arrays);

   package Complex_Types renames Complex_Arrays.Complex_Types;
   package Real_Arrays renames Complex_Arrays.Real_Arrays;

   --  Obtain the eigenvalues of a non-hermitian complex matrix.
   --
   --  The range of the result is A'Range (1).
   function Eigenvalues (A : Complex_Arrays.Complex_Matrix)
     return Complex_Arrays.Complex_Vector;

   --  Obtain the eigenvalues and eigenvectors of a non-hermitian
   --  complex matrix.
   --
   --  Values'Range must be the same as Vectors'Range (1).
   --  The ranges of Vectors must be the same as those of A.
   --
   --  The eigenvector corresponding to the jth element of Values is
   --  output in the jth column of Vectors.
   procedure Eigensystem
     (A       :     Complex_Arrays.Complex_Matrix;
      Values  : out Complex_Arrays.Complex_Vector;
      Vectors : out Complex_Arrays.Complex_Matrix);

   --  Obtain the eigenvalues of a non-symmetric real matrix.
   --
   --  The range of the result is A'Range (1).
   function Eigenvalues (A : Real_Arrays.Real_Matrix)
     return Complex_Arrays.Complex_Vector;

   --  Obtain the eigenvalues and eigenvectors of a non-symmetric real
   --  matrix.
   --
   --  Values'Range must be the same as Vectors'Range (1).
   --  The ranges of Vectors must be the same as those of A.
   --
   --  The eigenvector corresponding to the jth element of Values is
   --  output in the jth column of Vectors.
   procedure Eigensystem
     (A       :     Real_Arrays.Real_Matrix;
      Values  : out Complex_Arrays.Complex_Vector;
      Vectors : out Complex_Arrays.Complex_Matrix);

   --  A generalized eigenvalue for a pair of matrices (A,B) is a
   --  scalar lambda or a ratio alpha/beta = lambda, such that A -
   --  lambda*B is singular (or, equivalently, beta*A - alpha*B is
   --  singular).
   --
   --  It is usually represented as the pair (alpha,beta), as there
   --  is a reasonable interpretation for beta = 0, and even for both
   --  being zero.
   type Generalized_Eigenvalue is record
      Alpha : Complex_Types.Complex;
      Beta  : Complex_Types.Complex;
   end record;
   type Generalized_Eigenvalue_Vector
      is array (Integer range <>) of Generalized_Eigenvalue;

   --  Obtain the generalized eigenvalues and the right generalized
   --  eigenvectors of a pair of non-hermitian complex matrices.
   --
   --  The right eigenvector v(j) corresponding to the eigenvalue
   --  lambda(j) of (A,B) satisfies
   --            A * v(j) = lambda(j) * B * v(j).
   --
   --  Values'Range must be the same as A'Range (1).
   --  The ranges of A, B and Vectors must be the same.
   procedure Eigensystem
     (A       :     Complex_Arrays.Complex_Matrix;
      B       :     Complex_Arrays.Complex_Matrix;
      Values  : out Generalized_Eigenvalue_Vector;
      Vectors : out Complex_Arrays.Complex_Matrix);

   --  Obtain the generalized eigenvalues and the right generalized
   --  eigenvectors of a pair of non-symmetric real matrices.
   --
   --  The right eigenvector v(j) corresponding to the eigenvalue
   --  lambda(j) of (A,B) satisfies
   --            A * v(j) = lambda(j) * B * v(j).
   --
   --  Values'Range must be the same as A'Range (1).
   --  The ranges of A, B and Vectors must be the same.
   procedure Eigensystem
     (A       :     Real_Arrays.Real_Matrix;
      B       :     Real_Arrays.Real_Matrix;
      Values  : out Generalized_Eigenvalue_Vector;
      Vectors : out Complex_Arrays.Complex_Matrix);

end Ada_Numerics.Generic_Arrays;
