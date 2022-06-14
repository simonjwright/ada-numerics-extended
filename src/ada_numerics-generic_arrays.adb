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
--  Much of this unit is adapted from the Ada Runtime components of
--  the GNAT compiler, Copyright (C) 2006-2009, Free Software
--  Foundation, Inc under the same licence terms as above. The
--  adaptation and the original work are Copyright Simon Wright
--  <simon@pushface.org>.

pragma License (Modified_GPL);

pragma Warnings (Off);
with Interfaces.Fortran;
with System.Generic_Array_Operations;
pragma Warnings (On);

package body Ada_Numerics.Generic_Arrays is

   use type Interfaces.Fortran.Real;
   use type Interfaces.Fortran.Double_Precision;

   -------------------------------------------------------------------
   --  Compile-time constants to determine which LAPACK subprogram  --
   --  is needed.                                                   --
   -------------------------------------------------------------------

   Is_Single : constant Boolean :=
     Real_Arrays.Real'Machine_Mantissa
     = Interfaces.Fortran.Real'Machine_Mantissa
     and then Interfaces.Fortran.Real (Real_Arrays.Real'First)
     = Interfaces.Fortran.Real'First
     and then Interfaces.Fortran.Real (Real_Arrays.Real'Last)
     = Interfaces.Fortran.Real'Last;

   Is_Double : constant Boolean :=
     Real_Arrays.Real'Machine_Mantissa
     = Interfaces.Fortran.Double_Precision'Machine_Mantissa
     and then Interfaces.Fortran.Double_Precision
     (Real_Arrays.Real'First)
     = Interfaces.Fortran.Double_Precision'First
     and then Interfaces.Fortran.Double_Precision
     (Real_Arrays.Real'Last)
     = Interfaces.Fortran.Double_Precision'Last;

   -------------------------------------------------------------------
   --  Specifications of local  subprograms for handling Real when  --
   --  it isn't directly supported by BLAS/LAPACK.                  --
   -------------------------------------------------------------------

   function To_Double_Precision (X : Real_Arrays.Real)
     return Interfaces.Fortran.Double_Precision;
   pragma Inline (To_Double_Precision);

   function To_Real (X : Interfaces.Fortran.Double_Precision)
     return Real_Arrays.Real;
   pragma Inline (To_Real);

   function To_Double_Complex (X : Complex_Arrays.Complex_Types.Complex)
     return Interfaces.Fortran.Double_Complex;
   pragma Inline (To_Double_Complex);

   function To_Complex (X : Interfaces.Fortran.Double_Complex)
     return Complex_Arrays.Complex_Types.Complex;
   pragma Inline (To_Complex);

   ------------------------------------
   --  Vector/Matrix types for BLAS  --
   ------------------------------------

   --  These were in Interfaces.Fortran.BLAS until GCC 4.7/GNAT GPL 2012.

   --  Commented-out items are retained for completeness. but
   --  commented-out because they aren't used and the package is
   --  compiled in GNAT implementation mode, which treats warnings as
   --  errors.

   package BLAS is

      --  Vector types

      --  type Real_Vector is array (Integer range <>)
      --    of Interfaces.Fortran.Real;

      --  type Complex_Vector is array (Integer range <>)
      --    of Interfaces.Fortran.Complex;

      type Double_Precision_Vector is array (Integer range <>)
        of Interfaces.Fortran.Double_Precision;

      type Double_Complex_Vector is array (Integer range <>)
        of Interfaces.Fortran.Double_Complex;

      --  Matrix types

      --  type Real_Matrix is array (Integer range <>, Integer range <>)
      --    of Interfaces.Fortran.Real;

      type Double_Precision_Matrix
         is array (Integer range <>, Integer range <>)
        of Interfaces.Fortran.Double_Precision;

      --  type Complex_Matrix is array (Integer range <>, Integer range <>)
      --    of Interfaces.Fortran.Complex;

      type Double_Complex_Matrix is array (Integer range <>, Integer range <>)
        of Interfaces.Fortran.Double_Complex;

   end BLAS;

   -----------------------
   --  Instantiations.  --
   -----------------------

   --  Commented-out items are retained for completeness. but
   --  commented-out because they aren't used and the package is
   --  compiled in GNAT implementation mode, which treats warnings as
   --  errors.

   procedure Transpose
   is new System.Generic_Array_Operations.Transpose
     (Scalar => Real_Arrays.Real'Base,
      Matrix => Real_Arrays.Real_Matrix);

   procedure Transpose
   is new System.Generic_Array_Operations.Transpose
     (Scalar => Complex_Arrays.Complex_Types.Complex,
      Matrix => Complex_Arrays.Complex_Matrix);

   --  function To_Double_Precision is new
   --    System.Generic_Array_Operations.Vector_Elementwise_Operation
   --    (X_Scalar      => Real'Base,
   --     Result_Scalar => Interfaces.Fortran.Double_Precision,
   --     X_Vector      => Real_Vector,
   --     Result_Vector => BLAS.Double_Precision_Vector,
   --     Operation     => To_Double_Precision);

   function To_Real is new
     System.Generic_Array_Operations.Vector_Elementwise_Operation
     (X_Scalar      => Interfaces.Fortran.Double_Precision,
      Result_Scalar => Real_Arrays.Real'Base,
      X_Vector      => BLAS.Double_Precision_Vector,
      Result_Vector => Real_Arrays.Real_Vector,
      Operation     => To_Real);

   function To_Double_Precision is new
     System.Generic_Array_Operations.Matrix_Elementwise_Operation
     (X_Scalar      => Real_Arrays.Real'Base,
      Result_Scalar => Interfaces.Fortran.Double_Precision,
      X_Matrix      => Real_Arrays.Real_Matrix,
      Result_Matrix => BLAS.Double_Precision_Matrix,
      Operation     => To_Double_Precision);

   function To_Real is new
     System.Generic_Array_Operations.Matrix_Elementwise_Operation
     (X_Scalar      => Interfaces.Fortran.Double_Precision,
      Result_Scalar => Real_Arrays.Real'Base,
      X_Matrix      => BLAS.Double_Precision_Matrix,
      Result_Matrix => Real_Arrays.Real_Matrix,
      Operation     => To_Real);

   --  function To_Double_Complex is new
   --    System.Generic_Array_Operations.Vector_Elementwise_Operation
   --    (X_Scalar      => Complex,
   --     Result_Scalar => Interfaces.Fortran.Double_Complex,
   --     X_Vector      => Complex_Vector,
   --     Result_Vector => BLAS.Double_Complex_Vector,
   --     Operation     => To_Double_Complex);

   function To_Complex is new
     System.Generic_Array_Operations.Vector_Elementwise_Operation
     (X_Scalar      => Interfaces.Fortran.Double_Complex,
      Result_Scalar => Complex_Arrays.Complex_Types.Complex,
      X_Vector      => BLAS.Double_Complex_Vector,
      Result_Vector => Complex_Arrays.Complex_Vector,
      Operation     => To_Complex);

   function To_Double_Complex is new
     System.Generic_Array_Operations.Matrix_Elementwise_Operation
     (X_Scalar      => Complex_Arrays.Complex_Types.Complex,
      Result_Scalar => Interfaces.Fortran.Double_Complex,
      X_Matrix      => Complex_Arrays.Complex_Matrix,
      Result_Matrix => BLAS.Double_Complex_Matrix,
      Operation     => To_Double_Complex);

   function To_Complex is new
     System.Generic_Array_Operations.Matrix_Elementwise_Operation
     (X_Scalar      => Interfaces.Fortran.Double_Complex,
      Result_Scalar => Complex_Arrays.Complex_Types.Complex,
      X_Matrix      => BLAS.Double_Complex_Matrix,
      Result_Matrix => Complex_Arrays.Complex_Matrix,
      Operation     => To_Complex);

   -------------------------------------------------------------------
   --  Bodies of local subprograms for handling Real when it isn't  --
   --  directly supported by BLAS/LAPACK.                           --
   -------------------------------------------------------------------

   function To_Double_Precision (X : Real_Arrays.Real)
     return Interfaces.Fortran.Double_Precision
   is
   begin
      return Interfaces.Fortran.Double_Precision (X);
   end To_Double_Precision;

   function To_Real (X : Interfaces.Fortran.Double_Precision)
     return Real_Arrays.Real
   is
   begin
      return Real_Arrays.Real (X);
   end To_Real;

   function To_Double_Complex (X : Complex_Arrays.Complex_Types.Complex)
     return Interfaces.Fortran.Double_Complex
   is
   begin
      return (Re => Interfaces.Fortran.Double_Precision (X.Re),
              Im => Interfaces.Fortran.Double_Precision (X.Im));
   end To_Double_Complex;

   function To_Complex (X : Interfaces.Fortran.Double_Complex)
     return Complex_Arrays.Complex_Types.Complex
   is
   begin
      return (Re => Real_Arrays.Real (X.Re),
              Im => Real_Arrays.Real (X.Im));
   end To_Complex;

   -------------------------------------------------------------------
   --  Specifications of Ada-ised versions of LAPACK subroutines.   --
   -------------------------------------------------------------------

   --  This declaration is an Ada-ised version of the Fortran
   --  {c,z}geev, without the order/leading dimension parameters
   --  necessary for Fortran; the work areas are internally allocated.
   procedure Complex_geev
     (Jobv_L :        Character;
      Jobv_R :        Character;
      A      : in out Complex_Arrays.Complex_Matrix;
      W      :    out Complex_Arrays.Complex_Vector;
      V_L    :    out Complex_Arrays.Complex_Matrix;
      V_R    :    out Complex_Arrays.Complex_Matrix;
      Info   :    out Integer);

   --  This declaration is an Ada-ised version of the Fortran
   --  {s,d}geev, without the order/leading dimension parameters
   --  necessary for Fortran; the work areas are internally allocated.
   procedure Real_geev
     (Jobv_L :        Character;
      Jobv_R :        Character;
      A      : in out Real_Arrays.Real_Matrix;
      W_R    :    out Real_Arrays.Real_Vector;
      W_I    :    out Real_Arrays.Real_Vector;
      V_L    :    out Real_Arrays.Real_Matrix;
      V_R    :    out Real_Arrays.Real_Matrix;
      Info   :    out Integer);

   --  This declaration is an Ada-ised version of the Fortran
   --  {c,z}ggev, without the order/leading dimension parameters
   --  necessary for Fortran; the work areas are internally allocated.
   procedure Complex_ggev
     (Jobv_L  :        Character;
      Jobv_R  :        Character;
      A       : in out Complex_Arrays.Complex_Matrix;
      B       : in out Complex_Arrays.Complex_Matrix;
      Alpha   :    out Complex_Arrays.Complex_Vector;
      Beta    :    out Complex_Arrays.Complex_Vector;
      V_L     :    out Complex_Arrays.Complex_Matrix;
      V_R     :    out Complex_Arrays.Complex_Matrix;
      Info    :    out Integer);

   --  This declaration is an Ada-ised version of the Fortran
   --  {s,d}ggev, without the order/leading dimension parameters
   --  necessary for Fortran; the work areas are internally allocated.
   procedure Real_ggev
     (Jobv_L  :        Character;
      Jobv_R  :        Character;
      A       : in out Real_Arrays.Real_Matrix;
      B       : in out Real_Arrays.Real_Matrix;
      Alpha_R :    out Real_Arrays.Real_Vector;
      Alpha_I :    out Real_Arrays.Real_Vector;
      Beta    :    out Real_Arrays.Real_Vector;
      V_L     :    out Real_Arrays.Real_Matrix;
      V_R     :    out Real_Arrays.Real_Matrix;
      Info    :    out Integer);

   -------------------------------------
   --  Bodies of subprograms in spec  --
   -------------------------------------

   function Eigenvalues (A : Complex_Arrays.Complex_Matrix)
     return Complex_Arrays.Complex_Vector
   is

      Working_A : Complex_Arrays.Complex_Matrix (A'Range (2), A'Range (1));
      Result : Complex_Arrays.Complex_Vector (A'Range (1));
      Dummy_L_Eigenvectors, Dummy_R_Eigenvectors :
        Complex_Arrays.Complex_Matrix (1 .. 1, 1 .. 1); -- avoid warning
      Info : Integer;

   begin

      Transpose (A, Working_A);

      Complex_geev (Jobv_L => 'N', Jobv_R => 'N',
                    A => Working_A,
                    W => Result,
                    V_L => Dummy_L_Eigenvectors,
                    V_R => Dummy_R_Eigenvectors,
                    Info => Info);

      if Info /= 0 then
         raise Constraint_Error with "no or incomplete result";
      end if;

      return Result;

   end Eigenvalues;

   procedure Eigensystem
     (A       :     Complex_Arrays.Complex_Matrix;
      Values  : out Complex_Arrays.Complex_Vector;
      Vectors : out Complex_Arrays.Complex_Matrix)
   is

      Working_A : Complex_Arrays.Complex_Matrix (A'Range (2), A'Range (1));
      Dummy_L_Eigenvectors : Complex_Arrays.Complex_Matrix (1 .. 1, 1 .. 1);
      Working_R_Eigenvectors :
        Complex_Arrays.Complex_Matrix (Vectors'Range (2), Vectors'Range (1));
      Info : Integer;

   begin

      Transpose (A, Working_A);

      Complex_geev (Jobv_L => 'N', Jobv_R => 'V',
                    A => Working_A,
                    W => Values,
                    V_L => Dummy_L_Eigenvectors,
                    V_R => Working_R_Eigenvectors,
                    Info => Info);

      if Info /= 0 then
         raise Constraint_Error with "no or incomplete result";
      end if;

      Transpose (Working_R_Eigenvectors, Vectors);

   end Eigensystem;

   function Eigenvalues (A : Real_Arrays.Real_Matrix)
     return Complex_Arrays.Complex_Vector
   is

      Working_A : Real_Arrays.Real_Matrix (A'Range (2), A'Range (1));
      W_R, W_I : Real_Arrays.Real_Vector (A'Range (1));
      Result : Complex_Arrays.Complex_Vector (A'Range (1));
      Dummy_L_Eigenvectors, Dummy_R_Eigenvectors :
        Real_Arrays.Real_Matrix (1 .. 1, 1 .. 1); -- avoid overlap warning
      Info : Integer;

   begin

      Transpose (A, Working_A);

      Real_geev (Jobv_L => 'N', Jobv_R => 'N',
                 A => Working_A,
                 W_R => W_R,
                 W_I => W_I,
                 V_L => Dummy_L_Eigenvectors,
                 V_R => Dummy_R_Eigenvectors,
                 Info => Info);

      if Info /= 0 then
         raise Constraint_Error with "no or incomplete result";
      end if;

      Complex_Arrays.Set_Re (Result, W_R);
      Complex_Arrays.Set_Im (Result, W_I);

      return Result;

   end Eigenvalues;

   procedure Eigensystem
     (A       :     Real_Arrays.Real_Matrix;
      Values  : out Complex_Arrays.Complex_Vector;
      Vectors : out Complex_Arrays.Complex_Matrix)
   is

      Working_A : Real_Arrays.Real_Matrix (A'Range (2), A'Range (1));
      W_R, W_I : Real_Arrays.Real_Vector (A'Range (1));
      Dummy_L_Eigenvectors : Real_Arrays.Real_Matrix (1 .. 1, 1 .. 1);
      Working_R_Eigenvectors :
        Real_Arrays.Real_Matrix (Vectors'Range (2), Vectors'Range (1));
      Info : Integer;

      use type Real_Arrays.Real;

   begin

      Transpose (A, Working_A);

      Real_geev (Jobv_L => 'N', Jobv_R => 'V',
                 A => Working_A,
                 W_R => W_R,
                 W_I => W_I,
                 V_L => Dummy_L_Eigenvectors,
                 V_R => Working_R_Eigenvectors,
                 Info => Info);

      if Info /= 0 then
         raise Constraint_Error with "no or incomplete result";
      end if;

      Complex_Arrays.Set_Re (Values, W_R);
      Complex_Arrays.Set_Im (Values, W_I);

      --  This is from the man page for SGEEV:
      --
      --  If JOBVR = 'V', the right eigenvectors v(j) are stored one
      --  after another in the columns of VR, in the same order as
      --  their eigenvalues.  If JOBVR = 'N', VR is not referenced.
      --  If the j-th eigenvalue is real, then v(j) = VR(:,j), the
      --  j-th column of VR.  If the j-th and (j+1)-st eigenvalues
      --  form a complex conjugate pair, then v(j) = VR(:,j) +
      --  i*VR(:,j+1) and v(j+1) = VR(:,j) - i*VR(:,j+1).
      declare

         --  The current Value
         C : Integer := Values'First;

         --  We leave Working_R_Eigenvectors in Fortran order, so the
         --  row/column indices are transposed.
         --
         --  The result of this is that the 'column' index in the
         --  working matrix has to be adjusted to the corresponding
         --  'row' index range.
         J : Integer;

      begin
         loop

            J := C - Vectors'First (1) + Vectors'First (2);

            if W_I (C) = 0.0 then
               for K in Vectors'Range (1) loop
                  Vectors (K, J) := (Working_R_Eigenvectors (J, K), 0.0);
               end loop;
               C := C + 1;
            else
               if W_I (C) /= -W_I (C + 1) then
                  raise Constraint_Error
                    with "eigenvalue pair is not complex conjugate";
               end if;
               for K in Vectors'Range (1) loop
                  Vectors (K, J) := (Working_R_Eigenvectors (J, K),
                                     Working_R_Eigenvectors (J + 1, K));
                  Vectors (K, J + 1) := (Working_R_Eigenvectors (J, K),
                                         -Working_R_Eigenvectors (J + 1, K));
               end loop;
               C := C + 2;
            end if;

            if C > Values'Last + 1 then
               raise Program_Error with "ran off end of eigenvalues";
            end if;

            exit when C > Values'Last;

         end loop;
      end;

   end Eigensystem;

   procedure Eigensystem
     (A       :     Complex_Arrays.Complex_Matrix;
      B       :     Complex_Arrays.Complex_Matrix;
      Values  : out Generalized_Eigenvalue_Vector;
      Vectors : out Complex_Arrays.Complex_Matrix)
   is

      Working_A, Working_B :
        Complex_Arrays.Complex_Matrix (A'Range (2), A'Range (1));
      Alpha, Beta : Complex_Arrays.Complex_Vector (Values'Range);
      Dummy_L_Eigenvectors : Complex_Arrays.Complex_Matrix (1 .. 1, 1 .. 1);
      Working_R_Eigenvectors :
        Complex_Arrays.Complex_Matrix (Vectors'Range (2), Vectors'Range (1));
      Info : Integer;

   begin

      Transpose (A, Working_A);
      Transpose (B, Working_B);

      Complex_ggev (Jobv_L => 'N', Jobv_R => 'V',
                    A => Working_A,
                    B => Working_B,
                    Alpha => Alpha,
                    Beta => Beta,
                    V_L => Dummy_L_Eigenvectors,
                    V_R => Working_R_Eigenvectors,
                    Info => Info);

      if Info /= 0 then
         raise Constraint_Error with "no or incomplete result";
      end if;

      for J in Values'Range loop
         Values (J) := (Alpha => Alpha (J),
                        Beta  => Beta (J));
      end loop;

      Transpose (Working_R_Eigenvectors, Vectors);

   end Eigensystem;

   procedure Eigensystem
     (A       :     Real_Arrays.Real_Matrix;
      B       :     Real_Arrays.Real_Matrix;
      Values  : out Generalized_Eigenvalue_Vector;
      Vectors : out Complex_Arrays.Complex_Matrix)
   is

      Working_A, Working_B :
        Real_Arrays.Real_Matrix (A'Range (2), A'Range (1));
      A_R, A_I, B_R : Real_Arrays.Real_Vector (Values'Range);
      Dummy_L_Eigenvectors : Real_Arrays.Real_Matrix (1 .. 1, 1 .. 1);
      Working_R_Eigenvectors :
        Real_Arrays.Real_Matrix (Vectors'Range (2), Vectors'Range (1));
      Info : Integer;

      use type Real_Arrays.Real;

   begin

      Transpose (A, Working_A);
      Transpose (B, Working_B);

      Real_ggev (Jobv_L => 'N', Jobv_R => 'V',
                 A => Working_A,
                 B => Working_B,
                 Alpha_R => A_R,
                 Alpha_I => A_I,
                 Beta => B_R,
                 V_L => Dummy_L_Eigenvectors,
                 V_R => Working_R_Eigenvectors,
                 Info => Info);

      if Info /= 0 then
         raise Constraint_Error with "no or incomplete result";
      end if;

      for J in Values'Range loop
         Values (J) := (Alpha => (Re => A_R (J),
                                  Im => A_I (J)),
                        Beta  => (Re => B_R (J),
                                  Im => 0.0));
      end loop;

      --  This is from the man page for SGGEV:
      --
      --  VL is REAL array, dimension (LDVL,N)
      --  If JOBVL = 'V', the left eigenvectors u(j) are stored one
      --  after another in the columns of VL, in the same order as
      --  their eigenvalues. If the j-th eigenvalue is real, then
      --  u(j) = VL(:,j), the j-th column of VL. If the j-th and
      --  (j+1)-th eigenvalues form a complex conjugate pair, then
      --  u(j) = VL(:,j)+i*VL(:,j+1) and u(j+1) = VL(:,j)-i*VL(:,j+1).
      declare

         --  The current Value
         C : Integer := Values'First;

         --  We leave Working_R_Eigenvectors in Fortran order, so the
         --  row/column indices are transposed.
         --
         --  The result of this is that the 'column' index in the
         --  working matrix has to be adjusted to the corresponding
         --  'row' index range.
         J : Integer;

      begin
         loop

            J := C - Vectors'First (1) + Vectors'First (2);

            if Values (C).Alpha.Im = 0.0 then
               for K in Vectors'Range (1) loop
                  Vectors (K, J) := (Working_R_Eigenvectors (J, K), 0.0);
               end loop;
               C := C + 1;
            else
               for K in Vectors'Range (1) loop
                  Vectors (K, J) := (Working_R_Eigenvectors (J, K),
                                     Working_R_Eigenvectors (J + 1, K));
                  Vectors (K, J + 1) := (Working_R_Eigenvectors (J, K),
                                         -Working_R_Eigenvectors (J + 1, K));
               end loop;
               C := C + 2;
            end if;

            if C > Values'Last + 1 then
               raise Program_Error with "ran off end of eigenvalues";
            end if;

            exit when C > Values'Last;

         end loop;
      end;

   end Eigensystem;

   -----------------------------------------------------------
   --  Bodies of Ada-ised versions of LAPACK subroutines.   --
   -----------------------------------------------------------

   procedure Complex_geev
     (Jobv_L :        Character;
      Jobv_R :        Character;
      A      : in out Complex_Arrays.Complex_Matrix;
      W      :    out Complex_Arrays.Complex_Vector;
      V_L    :    out Complex_Arrays.Complex_Matrix;
      V_R    :    out Complex_Arrays.Complex_Matrix;
      Info   :    out Integer)
   is
   begin
      if Is_Single then
         declare
            procedure cgeev
              (Jobv_L :        Character;
               Jobv_R :        Character;
               N      :        Positive;
               A      : in out Complex_Arrays.Complex_Matrix;
               Ld_A   :        Positive;
               W      :    out Complex_Arrays.Complex_Vector;
               V_L    :    out Complex_Arrays.Complex_Matrix;
               Ld_V_L :        Integer;
               V_R    :    out Complex_Arrays.Complex_Matrix;
               Ld_V_R :        Integer;
               Work   :    out Complex_Arrays.Complex_Vector;
               L_Work :        Integer;
               R_Work :    out Complex_Arrays.Complex_Vector;
               Info   :    out Integer);
            pragma Import (Fortran, cgeev, "cgeev_");
            Querying_Work : Complex_Arrays.Complex_Vector (1 .. 1);
            R_Work : Complex_Arrays.Complex_Vector (1 .. 2 * A'Length (1));
         begin
            --  Query the optimum size of the Work vector
            cgeev (Jobv_L, Jobv_R,
                   A'Length (1),
                   A, A'Length (1),
                   W,
                   V_L, V_L'Length (1),
                   V_R, V_R'Length (1),
                   Querying_Work, -1,
                   R_Work,
                   Info);
            declare
               Local_Work : Complex_Arrays.Complex_Vector
                 (1 .. Integer (Querying_Work (1).Re));
            begin
               cgeev (Jobv_L, Jobv_R,
                      A'Length (1),
                      A, A'Length (1),
                      W,
                      V_L, V_L'Length (1),
                      V_R, V_R'Length (1),
                      Local_Work, Local_Work'Length,
                      R_Work,
                      Info);
            end;
         end;
      elsif Is_Double then
         declare
            procedure zgeev
              (Jobv_L :        Character;
               Jobv_R :        Character;
               N      :        Positive;
               A      : in out Complex_Arrays.Complex_Matrix;
               Ld_A   :        Positive;
               W      :    out Complex_Arrays.Complex_Vector;
               V_L    :    out Complex_Arrays.Complex_Matrix;
               Ld_V_L :        Integer;
               V_R    :    out Complex_Arrays.Complex_Matrix;
               Ld_V_R :        Integer;
               Work   :    out Complex_Arrays.Complex_Vector;
               L_Work :        Integer;
               R_Work :    out Complex_Arrays.Complex_Vector;
               Info   :    out Integer);
            pragma Import (Fortran, zgeev, "zgeev_");
            Querying_Work : Complex_Arrays.Complex_Vector (1 .. 1);
            R_Work : Complex_Arrays.Complex_Vector (1 .. 2 * A'Length (1));
         begin
            --  Query the optimum size of the Work vector
            zgeev (Jobv_L, Jobv_R,
                   A'Length (1),
                   A, A'Length (1),
                   W,
                   V_L, V_L'Length (1),
                   V_R, V_R'Length (1),
                   Querying_Work, -1,
                   R_Work,
                   Info);
            declare
               Local_Work : Complex_Arrays.Complex_Vector
                 (1 .. Integer (Querying_Work (1).Re));
            begin
               zgeev (Jobv_L, Jobv_R,
                      A'Length (1),
                      A, A'Length (1),
                      W,
                      V_L, V_L'Length (1),
                      V_R, V_R'Length (1),
                      Local_Work, Local_Work'Length,
                      R_Work,
                      Info);
            end;
         end;
      else
         declare
            procedure zgeev
              (Jobv_L :        Character;
               Jobv_R :        Character;
               N      :        Positive;
               A      : in out BLAS.Double_Complex_Matrix;
               Ld_A   :        Positive;
               W      :    out BLAS.Double_Complex_Vector;
               V_L    :    out BLAS.Double_Complex_Matrix;
               Ld_V_L :        Integer;
               V_R    :    out BLAS.Double_Complex_Matrix;
               Ld_V_R :        Integer;
               Work   :    out BLAS.Double_Complex_Vector;
               L_Work :        Integer;
               R_Work :    out BLAS.Double_Complex_Vector;
               Info   :    out Integer);
            pragma Import (Fortran, zgeev, "zgeev_");
            F_A : BLAS.Double_Complex_Matrix := To_Double_Complex (A);
            F_W : BLAS.Double_Complex_Vector (W'Range);
            F_V_L : BLAS.Double_Complex_Matrix (V_L'Range (1), V_L'Range (2));
            F_V_R : BLAS.Double_Complex_Matrix (V_R'Range (1), V_R'Range (2));
            F_Querying_Work : BLAS.Double_Complex_Vector (1 .. 1);
            F_R_Work : BLAS.Double_Complex_Vector (1 .. 2 * A'Length (1));
         begin
            --  Query the optimum size of the Work vector
            zgeev (Jobv_L, Jobv_R,
                   F_A'Length (1), F_A, F_A'Length (1),
                   F_W,
                   F_V_L, F_V_L'Length (1),
                   F_V_R, F_V_R'Length (1),
                   F_Querying_Work, -1,
                   F_R_Work,
                   Info);
            declare
               F_Local_Work :
                 BLAS.Double_Complex_Vector
                 (1 .. Integer (F_Querying_Work (1).Re));
            begin
               zgeev (Jobv_L, Jobv_R,
                      F_A'Length (1), F_A, F_A'Length (1),
                      F_W,
                      F_V_L, F_V_L'Length (1),
                      F_V_R, F_V_R'Length (1),
                      F_Local_Work, F_Local_Work'Length,
                      F_R_Work,
                      Info);
               W := To_Complex (F_W);
               --  Avoid computing V_L, V_R if zgeev wasn't asked to
               --  compute them (To_Complex might fail if
               --  uninitialized (i.e. invalid) values are present)
               if Jobv_L = 'V' then
                  V_L := To_Complex (F_V_L);
               end if;
               if Jobv_R = 'V' then
                  V_R := To_Complex (F_V_R);
               end if;
            end;
         end;
      end if;
   end Complex_geev;

   procedure Real_geev
     (Jobv_L :        Character;
      Jobv_R :        Character;
      A      : in out Real_Arrays.Real_Matrix;
      W_R    :    out Real_Arrays.Real_Vector;
      W_I    :    out Real_Arrays.Real_Vector;
      V_L    :    out Real_Arrays.Real_Matrix;
      V_R    :    out Real_Arrays.Real_Matrix;
      Info   :    out Integer)
   is
   begin
      if Is_Single then
         declare
            procedure sgeev
              (Jobv_L :        Character;
               Jobv_R :        Character;
               N      :        Positive;
               A      : in out Real_Arrays.Real_Matrix;
               Ld_A   :        Positive;
               W_R    :    out Real_Arrays.Real_Vector;
               W_I    :    out Real_Arrays.Real_Vector;
               V_L    :    out Real_Arrays.Real_Matrix;
               Ld_V_L :        Integer;
               V_R    :    out Real_Arrays.Real_Matrix;
               Ld_V_R :        Integer;
               Work   :    out Real_Arrays.Real_Vector;
               L_Work :        Integer;
               Info   :    out Integer);
            pragma Import (Fortran, sgeev, "sgeev_");
            Querying_Work : Real_Arrays.Real_Vector (1 .. 1);
         begin
            --  Query the optimum size of the Work vector
            sgeev (Jobv_L, Jobv_R,
                   A'Length (1),
                   A, A'Length (1),
                   W_R, W_I,
                   V_L, V_L'Length (1),
                   V_R, V_R'Length (1),
                   Querying_Work, -1,
                   Info);
            declare
               Local_Work : Real_Arrays.Real_Vector
                 (1 .. Integer (Querying_Work (1)));
            begin
               sgeev (Jobv_L, Jobv_R,
                      A'Length (1),
                      A, A'Length (1),
                      W_R, W_I,
                      V_L, V_L'Length (1),
                      V_R, V_R'Length (1),
                      Local_Work, Local_Work'Length,
                      Info);
            end;
         end;
      elsif Is_Double then
         declare
            procedure dgeev
              (Jobv_L :        Character;
               Jobv_R :        Character;
               N      :        Positive;
               A      : in out Real_Arrays.Real_Matrix;
               Ld_A   :        Positive;
               W_R    :    out Real_Arrays.Real_Vector;
               W_I    :    out Real_Arrays.Real_Vector;
               V_L    :    out Real_Arrays.Real_Matrix;
               Ld_V_L :        Integer;
               V_R    :    out Real_Arrays.Real_Matrix;
               Ld_V_R :        Integer;
               Work   :    out Real_Arrays.Real_Vector;
               L_Work :        Integer;
               Info   :    out Integer);
            pragma Import (Fortran, dgeev, "dgeev_");
            Querying_Work : Real_Arrays.Real_Vector (1 .. 1);
         begin
            --  Query the optimum size of the Work vector
            dgeev (Jobv_L, Jobv_R,
                   A'Length (1),
                   A, A'Length (1),
                   W_R, W_I,
                   V_L, V_L'Length (1),
                   V_R, V_R'Length (1),
                   Querying_Work, -1,
                   Info);
            declare
               Local_Work : Real_Arrays.Real_Vector
                 (1 .. Integer (Querying_Work (1)));
            begin
               dgeev (Jobv_L, Jobv_R,
                      A'Length (1),
                      A, A'Length (1),
                      W_R, W_I,
                      V_L, V_L'Length (1),
                      V_R, V_R'Length (1),
                      Local_Work, Local_Work'Length,
                      Info);
            end;
         end;
      else
         declare
            procedure dgeev
              (Jobv_L :        Character;
               Jobv_R :        Character;
               N      :        Positive;
               A      : in out BLAS.Double_Precision_Matrix;
               Ld_A   :        Positive;
               W_R    :    out BLAS.Double_Precision_Vector;
               W_I    :    out BLAS.Double_Precision_Vector;
               V_L    :    out BLAS.Double_Precision_Matrix;
               Ld_V_L :        Integer;
               V_R    :    out BLAS.Double_Precision_Matrix;
               Ld_V_R :        Integer;
               Work   :    out BLAS.Double_Precision_Vector;
               L_Work :        Integer;
               Info   :    out Integer);
            pragma Import (Fortran, dgeev, "dgeev_");
            F_A : BLAS.Double_Precision_Matrix := To_Double_Precision (A);
            F_W_R : BLAS.Double_Precision_Vector (W_R'Range);
            F_W_I : BLAS.Double_Precision_Vector (W_I'Range);
            F_V_L : BLAS.Double_Precision_Matrix (V_L'Range (1),
                                                  V_L'Range (2));
            F_V_R : BLAS.Double_Precision_Matrix (V_R'Range (1),
                                                  V_R'Range (2));
            F_Querying_Work : BLAS.Double_Precision_Vector (1 .. 1);
         begin
            --  Query the optimum size of the Work vector
            dgeev (Jobv_L, Jobv_R,
                   F_A'Length (1), F_A, F_A'Length (1),
                   F_W_R, F_W_I,
                   F_V_L, F_V_L'Length (1),
                   F_V_R, F_V_R'Length (1),
                   F_Querying_Work, -1,
                   Info);
            declare
               F_Local_Work :
                 BLAS.Double_Precision_Vector
                 (1 .. Integer (F_Querying_Work (1)));
            begin
               dgeev (Jobv_L, Jobv_R,
                      F_A'Length (1), F_A, F_A'Length (1),
                      F_W_R, F_W_I,
                      F_V_L, F_V_L'Length (1),
                      F_V_R, F_V_R'Length (1),
                      F_Local_Work, F_Local_Work'Length,
                      Info);
               W_R := To_Real (F_W_R);
               W_I := To_Real (F_W_I);
               --  Avoid computing V_L, V_R if dgeev wasn't asked to
               --  compute them (To_Real might fail if
               --  uninitialized (i.e. invalid) values are present)
               if Jobv_L = 'L' then
                  V_L := To_Real (F_V_L);
               end if;
               if Jobv_R = 'V' then
                  V_R := To_Real (F_V_R);
               end if;
            end;
         end;
      end if;
   end Real_geev;

   procedure Complex_ggev
     (Jobv_L  :        Character;
      Jobv_R  :        Character;
      A       : in out Complex_Arrays.Complex_Matrix;
      B       : in out Complex_Arrays.Complex_Matrix;
      Alpha   :    out Complex_Arrays.Complex_Vector;
      Beta    :    out Complex_Arrays.Complex_Vector;
      V_L     :    out Complex_Arrays.Complex_Matrix;
      V_R     :    out Complex_Arrays.Complex_Matrix;
      Info    :    out Integer)
   is
   begin
      if Is_Single then
         declare
            procedure cggev
              (Jobv_L  :        Character;
               Jobv_R  :        Character;
               N       :        Positive;
               A       : in out Complex_Arrays.Complex_Matrix;
               Ld_A    :        Positive;
               B       : in out Complex_Arrays.Complex_Matrix;
               Ld_B    :        Positive;
               Alpha   :    out Complex_Arrays.Complex_Vector;
               Beta    :    out Complex_Arrays.Complex_Vector;
               V_L     :    out Complex_Arrays.Complex_Matrix;
               Ld_V_L  :        Integer;
               V_R     :    out Complex_Arrays.Complex_Matrix;
               Ld_V_R  :        Integer;
               Work    :    out Complex_Arrays.Complex_Vector;
               L_Work  :        Integer;
               R_Work  :    out Real_Arrays.Real_Vector;
               Info    :    out Integer);
            pragma Import (Fortran, cggev, "cggev_");
            Querying_Work : Complex_Arrays.Complex_Vector (1 .. 1);
            R_Work : Real_Arrays.Real_Vector (1 .. 8 * A'Length (1));
         begin
            --  Query the optimum size of the Work vector
            cggev (Jobv_L, Jobv_R,
                   A'Length (1),
                   A, A'Length (1),
                   B, B'Length (1),
                   Alpha, Beta,
                   V_L, V_L'Length (1),
                   V_R, V_R'Length (1),
                   Querying_Work, -1,
                   R_Work,
                   Info);
            declare
               Local_Work : Complex_Arrays.Complex_Vector
                 (1 .. Integer (Querying_Work (1).Re));
            begin
               cggev (Jobv_L, Jobv_R,
                      A'Length (1),
                      A, A'Length (1),
                      B, B'Length (1),
                      Alpha, Beta,
                      V_L, V_L'Length (1),
                      V_R, V_R'Length (1),
                      Local_Work, Local_Work'Length,
                      R_Work,
                      Info);
            end;
         end;
      elsif Is_Double then
         declare
            procedure zggev
              (Jobv_L  :        Character;
               Jobv_R  :        Character;
               N       :        Positive;
               A       : in out Complex_Arrays.Complex_Matrix;
               Ld_A    :        Positive;
               B       : in out Complex_Arrays.Complex_Matrix;
               Ld_B    :        Positive;
               Alpha   :    out Complex_Arrays.Complex_Vector;
               Beta    :    out Complex_Arrays.Complex_Vector;
               V_L     :    out Complex_Arrays.Complex_Matrix;
               Ld_V_L  :        Integer;
               V_R     :    out Complex_Arrays.Complex_Matrix;
               Ld_V_R  :        Integer;
               Work    :    out Complex_Arrays.Complex_Vector;
               L_Work  :        Integer;
               R_Work  :    out Real_Arrays.Real_Vector;
               Info    :    out Integer);
            pragma Import (Fortran, zggev, "zggev_");
            Querying_Work : Complex_Arrays.Complex_Vector (1 .. 1);
            R_Work : Real_Arrays.Real_Vector (1 .. 8 * A'Length (1));
         begin
            --  Query the optimum size of the Work vector
            zggev (Jobv_L, Jobv_R,
                   A'Length (1),
                   A, A'Length (1),
                   B, B'Length (1),
                   Alpha, Beta,
                   V_L, V_L'Length (1),
                   V_R, V_R'Length (1),
                   Querying_Work, -1,
                   R_Work,
                   Info);
            declare
               Local_Work : Complex_Arrays.Complex_Vector
                 (1 .. Integer (Querying_Work (1).Re));
            begin
               zggev (Jobv_L, Jobv_R,
                      A'Length (1),
                      A, A'Length (1),
                      B, B'Length (1),
                      Alpha, Beta,
                      V_L, V_L'Length (1),
                      V_R, V_R'Length (1),
                      Local_Work, Local_Work'Length,
                      R_Work,
                      Info);
            end;
         end;
      else
         declare
            procedure zggev
              (Jobv_L  :        Character;
               Jobv_R  :        Character;
               N       :        Positive;
               A       : in out BLAS.Double_Complex_Matrix;
               Ld_A    :        Positive;
               B       : in out BLAS.Double_Complex_Matrix;
               Ld_B    :        Positive;
               Alpha   :    out BLAS.Double_Complex_Vector;
               Beta    :    out BLAS.Double_Complex_Vector;
               V_L     :    out BLAS.Double_Complex_Matrix;
               Ld_V_L  :        Integer;
               V_R     :    out BLAS.Double_Complex_Matrix;
               Ld_V_R  :        Integer;
               Work    :    out BLAS.Double_Complex_Vector;
               L_Work  :        Integer;
               R_Work  :    out BLAS.Double_Precision_Vector;
               Info    :    out Integer);
            pragma Import (Fortran, zggev, "zggev_");
            F_A : BLAS.Double_Complex_Matrix := To_Double_Complex (A);
            F_B : BLAS.Double_Complex_Matrix := To_Double_Complex (B);
            F_Alpha : BLAS.Double_Complex_Vector (Alpha'Range);
            F_Beta : BLAS.Double_Complex_Vector (Beta'Range);
            F_V_L : BLAS.Double_Complex_Matrix (V_L'Range (1),
                                               V_L'Range (2));
            F_V_R : BLAS.Double_Complex_Matrix (V_R'Range (1),
                                               V_R'Range (2));
            F_Querying_Work : BLAS.Double_Complex_Vector (1 .. 1);
            R_Work : BLAS.Double_Precision_Vector (1 .. 8 * A'Length (1));
         begin
            --  Query the optimum size of the Work vector
            zggev (Jobv_L, Jobv_R,
                   F_A'Length (1),
                   F_A, F_A'Length (1),
                   F_B, F_B'Length (1),
                   F_Alpha, F_Beta,
                   F_V_L, F_V_L'Length (1),
                   F_V_R, F_V_R'Length (1),
                   F_Querying_Work, -1,
                   R_Work,
                   Info);
            declare
               F_Local_Work :
                 BLAS.Double_Complex_Vector
                 (1 .. Integer (F_Querying_Work (1).Re));
            begin
               zggev (Jobv_L, Jobv_R,
                      F_A'Length (1),
                      F_A, F_A'Length (1),
                      F_B, F_B'Length (1),
                      F_Alpha, F_Beta,
                      F_V_L, F_V_L'Length (1),
                      F_V_R, F_V_R'Length (1),
                      F_Local_Work, F_Local_Work'Length,
                      R_Work,
                      Info);
               Alpha := To_Complex (F_Alpha);
               Beta := To_Complex (F_Beta);
               --  Avoid computing V_L, V_R if zggev wasn't asked to
               --  compute them (To_Complex might fail if
               --  uninitialized (i.e. invalid) values are present)
               if Jobv_L = 'V' then
                  V_L := To_Complex (F_V_L);
               end if;
               if Jobv_R = 'V' then
                  V_R := To_Complex (F_V_R);
               end if;
            end;
         end;
      end if;
   end Complex_ggev;

   procedure Real_ggev
     (Jobv_L  :        Character;
      Jobv_R  :        Character;
      A       : in out Real_Arrays.Real_Matrix;
      B       : in out Real_Arrays.Real_Matrix;
      Alpha_R :    out Real_Arrays.Real_Vector;
      Alpha_I :    out Real_Arrays.Real_Vector;
      Beta    :    out Real_Arrays.Real_Vector;
      V_L     :    out Real_Arrays.Real_Matrix;
      V_R     :    out Real_Arrays.Real_Matrix;
      Info    :    out Integer)
   is
   begin
      if Is_Single then
         declare
            procedure sggev
              (Jobv_L  :        Character;
               Jobv_R  :        Character;
               N       :        Positive;
               A       : in out Real_Arrays.Real_Matrix;
               Ld_A    :        Positive;
               B       : in out Real_Arrays.Real_Matrix;
               Ld_B    :        Positive;
               Alpha_R :    out Real_Arrays.Real_Vector;
               Alpha_I :    out Real_Arrays.Real_Vector;
               Beta    :    out Real_Arrays.Real_Vector;
               V_L     :    out Real_Arrays.Real_Matrix;
               Ld_V_L  :        Integer;
               V_R     :    out Real_Arrays.Real_Matrix;
               Ld_V_R  :        Integer;
               Work    :    out Real_Arrays.Real_Vector;
               L_Work  :        Integer;
               Info    :    out Integer);
            pragma Import (Fortran, sggev, "sggev_");
            Querying_Work : Real_Arrays.Real_Vector (1 .. 1);
         begin
            --  Query the optimum size of the Work vector
            sggev (Jobv_L, Jobv_R,
                   A'Length (1),
                   A, A'Length (1),
                   B, B'Length (1),
                   Alpha_R, Alpha_I,
                   Beta,
                   V_L, V_L'Length (1),
                   V_R, V_R'Length (1),
                   Querying_Work, -1,
                   Info);
            declare
               Local_Work : Real_Arrays.Real_Vector
                 (1 .. Integer (Querying_Work (1)));
            begin
               sggev (Jobv_L, Jobv_R,
                      A'Length (1),
                      A, A'Length (1),
                      B, B'Length (1),
                      Alpha_R, Alpha_I,
                      Beta,
                      V_L, V_L'Length (1),
                      V_R, V_R'Length (1),
                      Local_Work, Local_Work'Length,
                      Info);
            end;
         end;
      elsif Is_Double then
         declare
            procedure dggev
              (Jobv_L  :        Character;
               Jobv_R  :        Character;
               N       :        Positive;
               A       : in out Real_Arrays.Real_Matrix;
               Ld_A    :        Positive;
               B       : in out Real_Arrays.Real_Matrix;
               Ld_B    :        Positive;
               Alpha_R :    out Real_Arrays.Real_Vector;
               Alpha_I :    out Real_Arrays.Real_Vector;
               Beta    :    out Real_Arrays.Real_Vector;
               V_L     :    out Real_Arrays.Real_Matrix;
               Ld_V_L  :        Integer;
               V_R     :    out Real_Arrays.Real_Matrix;
               Ld_V_R  :        Integer;
               Work    :    out Real_Arrays.Real_Vector;
               L_Work  :        Integer;
               Info    :    out Integer);
            pragma Import (Fortran, dggev, "dggev_");
            Querying_Work : Real_Arrays.Real_Vector (1 .. 1);
         begin
            --  Query the optimum size of the Work vector
            dggev (Jobv_L, Jobv_R,
                   A'Length (1),
                   A, A'Length (1),
                   B, B'Length (1),
                   Alpha_R, Alpha_I,
                   Beta,
                   V_L, V_L'Length (1),
                   V_R, V_R'Length (1),
                   Querying_Work, -1,
                   Info);
            declare
               Local_Work : Real_Arrays.Real_Vector
                 (1 .. Integer (Querying_Work (1)));
            begin
               dggev (Jobv_L, Jobv_R,
                      A'Length (1),
                      A, A'Length (1),
                      B, B'Length (1),
                      Alpha_R, Alpha_I,
                      Beta,
                      V_L, V_L'Length (1),
                      V_R, V_R'Length (1),
                      Local_Work, Local_Work'Length,
                      Info);
            end;
         end;
      else
         declare
            procedure dggev
              (Jobv_L  :        Character;
               Jobv_R  :        Character;
               N       :        Positive;
               A       : in out BLAS.Double_Precision_Matrix;
               Ld_A    :        Positive;
               B       : in out BLAS.Double_Precision_Matrix;
               Ld_B    :        Positive;
               Alpha_R :    out BLAS.Double_Precision_Vector;
               Alpha_I :    out BLAS.Double_Precision_Vector;
               Beta    :    out BLAS.Double_Precision_Vector;
               V_L     :    out BLAS.Double_Precision_Matrix;
               Ld_V_L  :        Integer;
               V_R     :    out BLAS.Double_Precision_Matrix;
               Ld_V_R  :        Integer;
               Work    :    out BLAS.Double_Precision_Vector;
               L_Work  :        Integer;
               Info    :    out Integer);
            pragma Import (Fortran, dggev, "dggev_");
            F_A : BLAS.Double_Precision_Matrix := To_Double_Precision (A);
            F_B : BLAS.Double_Precision_Matrix := To_Double_Precision (B);
            F_Alpha_R : BLAS.Double_Precision_Vector (Alpha_R'Range);
            F_Alpha_I : BLAS.Double_Precision_Vector (Alpha_I'Range);
            F_Beta : BLAS.Double_Precision_Vector (Beta'Range);
            F_V_L : BLAS.Double_Precision_Matrix (V_L'Range (1),
                                                 V_L'Range (2));
            F_V_R : BLAS.Double_Precision_Matrix (V_R'Range (1),
                                                 V_R'Range (2));
            F_Querying_Work : BLAS.Double_Precision_Vector (1 .. 1);
         begin
            --  Query the optimum size of the Work vector
            dggev (Jobv_L, Jobv_R,
                   F_A'Length (1),
                   F_A, F_A'Length (1),
                   F_B, F_B'Length (1),
                   F_Alpha_R, F_Alpha_I,
                   F_Beta,
                   F_V_L, F_V_L'Length (1),
                   F_V_R, F_V_R'Length (1),
                   F_Querying_Work, -1,
                   Info);
            declare
               F_Local_Work :
                 BLAS.Double_Precision_Vector
                 (1 .. Integer (F_Querying_Work (1)));
            begin
               dggev (Jobv_L, Jobv_R,
                      F_A'Length (1),
                      F_A, F_A'Length (1),
                      F_B, F_B'Length (1),
                      F_Alpha_R, F_Alpha_I,
                      F_Beta,
                      F_V_L, F_V_L'Length (1),
                      F_V_R, F_V_R'Length (1),
                      F_Local_Work, F_Local_Work'Length,
                      Info);
               Alpha_R := To_Real (F_Alpha_R);
               Alpha_I := To_Real (F_Alpha_I);
               Beta := To_Real (F_Beta);
               --  Avoid computing V_L, V_R if dggev wasn't asked to
               --  compute them (To_Real might fail if
               --  uninitialized (i.e. invalid) values are present)
               if Jobv_L = 'V' then
                  V_L := To_Real (F_V_L);
               end if;
               if Jobv_R = 'V' then
                  V_R := To_Real (F_V_R);
               end if;
            end;
         end;
      end if;
   end Real_ggev;

end Ada_Numerics.Generic_Arrays;
