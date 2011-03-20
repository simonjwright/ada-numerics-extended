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
with Interfaces.Fortran.BLAS;
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

   -----------------------
   --  Instantiations.  --
   -----------------------

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
   --     Result_Vector => Interfaces.Fortran.BLAS.Double_Precision_Vector,
   --     Operation     => To_Double_Precision);

   function To_Real is new
     System.Generic_Array_Operations.Vector_Elementwise_Operation
     (X_Scalar      => Interfaces.Fortran.Double_Precision,
      Result_Scalar => Real_Arrays.Real'Base,
      X_Vector      => Interfaces.Fortran.BLAS.Double_Precision_Vector,
      Result_Vector => Real_Arrays.Real_Vector,
      Operation     => To_Real);

   function To_Double_Precision is new
     System.Generic_Array_Operations.Matrix_Elementwise_Operation
     (X_Scalar      => Real_Arrays.Real'Base,
      Result_Scalar => Interfaces.Fortran.Double_Precision,
      X_Matrix      => Real_Arrays.Real_Matrix,
      Result_Matrix => Interfaces.Fortran.BLAS.Double_Precision_Matrix,
      Operation     => To_Double_Precision);

   function To_Real is new
     System.Generic_Array_Operations.Matrix_Elementwise_Operation
     (X_Scalar      => Interfaces.Fortran.Double_Precision,
      Result_Scalar => Real_Arrays.Real'Base,
      X_Matrix      => Interfaces.Fortran.BLAS.Double_Precision_Matrix,
      Result_Matrix => Real_Arrays.Real_Matrix,
      Operation     => To_Real);

   --  function To_Double_Complex is new
   --    System.Generic_Array_Operations.Vector_Elementwise_Operation
   --    (X_Scalar      => Complex,
   --     Result_Scalar => Interfaces.Fortran.Double_Complex,
   --     X_Vector      => Complex_Vector,
   --     Result_Vector => Interfaces.Fortran.BLAS.Double_Complex_Vector,
   --     Operation     => To_Double_Complex);

   function To_Complex is new
     System.Generic_Array_Operations.Vector_Elementwise_Operation
     (X_Scalar      => Interfaces.Fortran.Double_Complex,
      Result_Scalar => Complex_Arrays.Complex_Types.Complex,
      X_Vector      => Interfaces.Fortran.BLAS.Double_Complex_Vector,
      Result_Vector => Complex_Arrays.Complex_Vector,
      Operation     => To_Complex);

   function To_Double_Complex is new
     System.Generic_Array_Operations.Matrix_Elementwise_Operation
     (X_Scalar      => Complex_Arrays.Complex_Types.Complex,
      Result_Scalar => Interfaces.Fortran.Double_Complex,
      X_Matrix      => Complex_Arrays.Complex_Matrix,
      Result_Matrix => Interfaces.Fortran.BLAS.Double_Complex_Matrix,
      Operation     => To_Double_Complex);

   function To_Complex is new
     System.Generic_Array_Operations.Matrix_Elementwise_Operation
     (X_Scalar      => Interfaces.Fortran.Double_Complex,
      Result_Scalar => Complex_Arrays.Complex_Types.Complex,
      X_Matrix      => Interfaces.Fortran.BLAS.Double_Complex_Matrix,
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
      Dummy_Eigenvectors : Complex_Arrays.Complex_Matrix (1 .. 1, 1 .. 1);
      Info : Integer;

   begin

      if A'Length (1) /= A'Length (2) then
         raise Constraint_Error with "not square";
      end if;

      Transpose (A, Working_A);

      Complex_geev (Jobv_L => 'N', Jobv_R => 'N',
                    A => Working_A,
                    W => Result,
                    V_L => Dummy_Eigenvectors,
                    V_R => Dummy_Eigenvectors,
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

      if A'Length (1) /= A'Length (2) then
         raise Constraint_Error with "A not square";
      end if;

      if Values'Length /= A'Length (1) then
         raise Constraint_Error with "Values has wrong length";
      end if;

      if Values'First /= A'First (1) then
         raise Constraint_Error with "Values has wrong range";
      end if;

      if Vectors'Length (1) /= Vectors'Length (2) then
         raise Constraint_Error with "Vectors not square";
      end if;

      if Vectors'Length (1) /= A'Length (1) then
         raise Constraint_Error with "Vectors has wrong length";
      end if;

      if Vectors'First (1) /= A'First (1)
        or Vectors'First (2) /= A'First (2) then
         raise Constraint_Error with "Vectors has wrong range(s)";
      end if;

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
      Dummy_Eigenvectors : Real_Arrays.Real_Matrix (1 .. 1, 1 .. 1);
      Info : Integer;

   begin

      if A'Length (1) /= A'Length (2) then
         raise Constraint_Error with "not square";
      end if;

      Transpose (A, Working_A);

      Real_geev (Jobv_L => 'N', Jobv_R => 'N',
                 A => Working_A,
                 W_R => W_R,
                 W_I => W_I,
                 V_L => Dummy_Eigenvectors,
                 V_R => Dummy_Eigenvectors,
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

      if A'Length (1) /= A'Length (2) then
         raise Constraint_Error with "not square";
      end if;

      if Values'Length /= A'Length (1) then
         raise Constraint_Error with "Values has wrong length";
      end if;

      if Values'First /= A'First (1) then
         raise Constraint_Error with "Values has wrong range";
      end if;

      if Vectors'Length (1) /= Vectors'Length (2) then
         raise Constraint_Error with "Vectors not square";
      end if;

      if Vectors'Length (1) /= A'Length (1) then
         raise Constraint_Error with "Vectors has wrong length";
      end if;

      if Vectors'First (1) /= A'First (1)
        or Vectors'First (2) /= A'First (2) then
         raise Constraint_Error with "Vectors has wrong range(s)";
      end if;

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
               pragma Assert (W_I (C) = -W_I (C + 1),
                              "eigenvalue pair is not complex conjugate");
               for K in Vectors'Range (1) loop
                  Vectors (K, J) := (Working_R_Eigenvectors (J, K),
                                     Working_R_Eigenvectors (J + 1, K));
                  Vectors (K, J + 1) := (Working_R_Eigenvectors (J, K),
                                         -Working_R_Eigenvectors (J + 1, K));
               end loop;
               C := C + 2;
            end if;

            pragma Assert (C <= Values'Last + 1,
                           "ran off end of eigenvalues");

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

      --  use type Complex_Arrays.Complex;

   begin

      if A'Length (1) /= A'Length (2) then
         raise Constraint_Error with "A not square";
      end if;

      if B'Length (1) /= B'Length (2) then
         raise Constraint_Error with "B not square";
      end if;

      if B'First (1) /= A'First (1) or B'First (2) /= A'First (2) then
         raise Constraint_Error with "A & B have different ranges";
      end if;

      if Values'Length /= A'Length (1) then
         raise Constraint_Error with "Values has wrong length";
      end if;

      if Values'First /= A'First (1) then
         raise Constraint_Error with "Values has wrong range";
      end if;

      if Vectors'Length (1) /= Vectors'Length (2) then
         raise Constraint_Error with "Vectors not square";
      end if;

      if Vectors'Length (1) /= A'Length (1) then
         raise Constraint_Error with "Vectors has wrong length";
      end if;

      if Vectors'First (1) /= A'First (1)
        or Vectors'First (2) /= A'First (2) then
         raise Constraint_Error with "Vectors has wrong range(s)";
      end if;

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
      Vectors : out Real_Arrays.Real_Matrix)
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

      if A'Length (1) /= A'Length (2) then
         raise Constraint_Error with "A not square";
      end if;

      if B'Length (1) /= B'Length (2) then
         raise Constraint_Error with "B not square";
      end if;

      if B'First (1) /= A'First (1) or B'First (2) /= A'First (2) then
         raise Constraint_Error with "A & B have different ranges";
      end if;

      if Values'Length /= A'Length (1) then
         raise Constraint_Error with "Values has wrong length";
      end if;

      if Values'First /= A'First (1) then
         raise Constraint_Error with "Values has wrong range";
      end if;

      if Vectors'Length (1) /= Vectors'Length (2) then
         raise Constraint_Error with "Vectors not square";
      end if;

      if Vectors'Length (1) /= A'Length (1) then
         raise Constraint_Error with "Vectors has wrong length";
      end if;

      if Vectors'First (1) /= A'First (1)
        or Vectors'First (2) /= A'First (2) then
         raise Constraint_Error with "Vectors has wrong range(s)";
      end if;

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

      Transpose (Working_R_Eigenvectors, Vectors);

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
            package IFB renames Interfaces.Fortran.BLAS;
            procedure zgeev
              (Jobv_L :        Character;
               Jobv_R :        Character;
               N      :        Positive;
               A      : in out IFB.Double_Complex_Matrix;
               Ld_A   :        Positive;
               W      :    out IFB.Double_Complex_Vector;
               V_L    :    out IFB.Double_Complex_Matrix;
               Ld_V_L :        Integer;
               V_R    :    out IFB.Double_Complex_Matrix;
               Ld_V_R :        Integer;
               Work   :    out IFB.Double_Complex_Vector;
               L_Work :        Integer;
               R_Work :    out IFB.Double_Complex_Vector;
               Info   :    out Integer);
            pragma Import (Fortran, zgeev, "zgeev_");
            F_A : IFB.Double_Complex_Matrix := To_Double_Complex (A);
            F_W : IFB.Double_Complex_Vector (W'Range);
            F_V_L : IFB.Double_Complex_Matrix (V_L'Range (1), V_L'Range (2));
            F_V_R : IFB.Double_Complex_Matrix (V_R'Range (1), V_R'Range (2));
            F_Querying_Work : IFB.Double_Complex_Vector (1 .. 1);
            F_R_Work : IFB.Double_Complex_Vector (1 .. 2 * A'Length (1));
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
                 IFB.Double_Complex_Vector
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
               V_R := To_Complex (F_V_R);
               V_L := To_Complex (F_V_L);
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
            package IFB renames Interfaces.Fortran.BLAS;
            procedure dgeev
              (Jobv_L :        Character;
               Jobv_R :        Character;
               N      :        Positive;
               A      : in out IFB.Double_Precision_Matrix;
               Ld_A   :        Positive;
               W_R    :    out IFB.Double_Precision_Vector;
               W_I    :    out IFB.Double_Precision_Vector;
               V_L    :    out IFB.Double_Precision_Matrix;
               Ld_V_L :        Integer;
               V_R    :    out IFB.Double_Precision_Matrix;
               Ld_V_R :        Integer;
               Work   :    out IFB.Double_Precision_Vector;
               L_Work :        Integer;
               Info   :    out Integer);
            pragma Import (Fortran, dgeev, "dgeev_");
            F_A : IFB.Double_Precision_Matrix := To_Double_Precision (A);
            F_W_R : IFB.Double_Precision_Vector (W_R'Range);
            F_W_I : IFB.Double_Precision_Vector (W_I'Range);
            F_V_L : IFB.Double_Precision_Matrix (V_L'Range (1), V_L'Range (2));
            F_V_R : IFB.Double_Precision_Matrix (V_R'Range (1), V_R'Range (2));
            F_Querying_Work : IFB.Double_Precision_Vector (1 .. 1);
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
                 IFB.Double_Precision_Vector
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
               V_R := To_Real (F_V_R);
               V_L := To_Real (F_V_L);
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
            package IFB renames Interfaces.Fortran.BLAS;
            procedure zggev
              (Jobv_L  :        Character;
               Jobv_R  :        Character;
               N       :        Positive;
               A       : in out IFB.Double_Complex_Matrix;
               Ld_A    :        Positive;
               B       : in out IFB.Double_Complex_Matrix;
               Ld_B    :        Positive;
               Alpha   :    out IFB.Double_Complex_Vector;
               Beta    :    out IFB.Double_Complex_Vector;
               V_L     :    out IFB.Double_Complex_Matrix;
               Ld_V_L  :        Integer;
               V_R     :    out IFB.Double_Complex_Matrix;
               Ld_V_R  :        Integer;
               Work    :    out IFB.Double_Complex_Vector;
               L_Work  :        Integer;
               R_Work  :    out IFB.Double_Precision_Vector;
               Info    :    out Integer);
            pragma Import (Fortran, zggev, "zggev_");
            F_A : IFB.Double_Complex_Matrix := To_Double_Complex (A);
            F_B : IFB.Double_Complex_Matrix := To_Double_Complex (B);
            F_Alpha : IFB.Double_Complex_Vector (Alpha'Range);
            F_Beta : IFB.Double_Complex_Vector (Beta'Range);
            F_V_L : IFB.Double_Complex_Matrix (V_L'Range (1),
                                               V_L'Range (2));
            F_V_R : IFB.Double_Complex_Matrix (V_R'Range (1),
                                               V_R'Range (2));
            F_Querying_Work : IFB.Double_Complex_Vector (1 .. 1);
            R_Work : IFB.Double_Precision_Vector (1 .. 8 * A'Length (1));
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
                 IFB.Double_Complex_Vector
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
               V_R := To_Complex (F_V_R);
               V_L := To_Complex (F_V_L);
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
            package IFB renames Interfaces.Fortran.BLAS;
            procedure dggev
              (Jobv_L  :        Character;
               Jobv_R  :        Character;
               N       :        Positive;
               A       : in out IFB.Double_Precision_Matrix;
               Ld_A    :        Positive;
               B       : in out IFB.Double_Precision_Matrix;
               Ld_B    :        Positive;
               Alpha_R :    out IFB.Double_Precision_Vector;
               Alpha_I :    out IFB.Double_Precision_Vector;
               Beta    :    out IFB.Double_Precision_Vector;
               V_L     :    out IFB.Double_Precision_Matrix;
               Ld_V_L  :        Integer;
               V_R     :    out IFB.Double_Precision_Matrix;
               Ld_V_R  :        Integer;
               Work    :    out IFB.Double_Precision_Vector;
               L_Work  :        Integer;
               Info    :    out Integer);
            pragma Import (Fortran, dggev, "dggev_");
            F_A : IFB.Double_Precision_Matrix := To_Double_Precision (A);
            F_B : IFB.Double_Precision_Matrix := To_Double_Precision (B);
            F_Alpha_R : IFB.Double_Precision_Vector (Alpha_R'Range);
            F_Alpha_I : IFB.Double_Precision_Vector (Alpha_I'Range);
            F_Beta : IFB.Double_Precision_Vector (Beta'Range);
            F_V_L : IFB.Double_Precision_Matrix (V_L'Range (1),
                                                 V_L'Range (2));
            F_V_R : IFB.Double_Precision_Matrix (V_R'Range (1),
                                                 V_R'Range (2));
            F_Querying_Work : IFB.Double_Precision_Vector (1 .. 1);
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
                 IFB.Double_Precision_Vector
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
               V_R := To_Real (F_V_R);
               V_L := To_Real (F_V_L);
            end;
         end;
      end if;
   end Real_ggev;

end Ada_Numerics.Generic_Arrays;
