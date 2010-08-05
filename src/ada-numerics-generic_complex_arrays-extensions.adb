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

with Interfaces.Fortran.BLAS;
with System.Generic_Array_Operations;

package body Ada.Numerics.Generic_Complex_Arrays.Extensions is

   procedure Transpose
   is new System.Generic_Array_Operations.Transpose (Scalar => Complex,
                                                     Matrix => Complex_Matrix);

   use type Interfaces.Fortran.Real;
   use type Interfaces.Fortran.Double_Precision;

   subtype Real is Generic_Complex_Arrays.Real_Arrays.Real;

   Is_Single : constant Boolean :=
     Real'Machine_Mantissa
       = Interfaces.Fortran.Real'Machine_Mantissa
     and then Interfaces.Fortran.Real (Real'First)
       = Interfaces.Fortran.Real'First
     and then Interfaces.Fortran.Real (Real'Last)
       = Interfaces.Fortran.Real'Last;

   Is_Double : constant Boolean :=
     Real'Machine_Mantissa
       = Interfaces.Fortran.Double_Precision'Machine_Mantissa
     and then Interfaces.Fortran.Double_Precision (Real'First)
       = Interfaces.Fortran.Double_Precision'First
     and then Interfaces.Fortran.Double_Precision (Real'Last)
       = Interfaces.Fortran.Double_Precision'Last;

   --  Local subprograms for handling Real when it isn't directly
   --  supportable by BLAS/LAPACK

   --  function To_Double_Precision
   --    (X : Real)
   --    return Interfaces.Fortran.Double_Precision;
   --  pragma Inline (To_Double_Precision);

   --  function To_Real (X : Interfaces.Fortran.Double_Precision) return Real;
   --  pragma Inline (To_Real);

   function To_Double_Complex
     (X : Complex)
     return Interfaces.Fortran.Double_Complex;
   pragma Inline (To_Double_Complex);

   function To_Complex
     (X : Interfaces.Fortran.Double_Complex) return Complex;
   pragma Inline (To_Complex);

   --  Instantiations

   --  function To_Double_Precision is new
   --    System.Generic_Array_Operations.Vector_Elementwise_Operation
   --    (X_Scalar      => Real'Base,
   --     Result_Scalar => Interfaces.Fortran.Double_Precision,
   --     X_Vector      => Real_Vector,
   --     Result_Vector => Interfaces.Fortran.BLAS.Double_Precision_Vector,
   --     Operation     => To_Double_Precision);

   --  function To_Real is new
   --    System.Generic_Array_Operations.Vector_Elementwise_Operation
   --    (X_Scalar      => Interfaces.Fortran.Double_Precision,
   --     Result_Scalar => Real'Base,
   --     X_Vector      => Interfaces.Fortran.BLAS.Double_Precision_Vector,
   --     Result_Vector => Real_Vector,
   --     Operation     => To_Real);

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
      Result_Scalar => Complex,
      X_Vector      => Interfaces.Fortran.BLAS.Double_Complex_Vector,
      Result_Vector => Complex_Vector,
      Operation     => To_Complex);

   function To_Double_Complex is new
     System.Generic_Array_Operations.Matrix_Elementwise_Operation
     (X_Scalar      => Complex,
      Result_Scalar => Interfaces.Fortran.Double_Complex,
      X_Matrix      => Complex_Matrix,
      Result_Matrix => Interfaces.Fortran.BLAS.Double_Complex_Matrix,
      Operation     => To_Double_Complex);

   function To_Complex is new
     System.Generic_Array_Operations.Matrix_Elementwise_Operation
     (X_Scalar      => Interfaces.Fortran.Double_Complex,
      Result_Scalar => Complex,
      X_Matrix      => Interfaces.Fortran.BLAS.Double_Complex_Matrix,
      Result_Matrix => Complex_Matrix,
      Operation     => To_Complex);

   --  function To_Double_Precision
   --    (X : Real)
   --    return Interfaces.Fortran.Double_Precision
   --  is
   --  begin
   --     return Interfaces.Fortran.Double_Precision (X);
   --  end To_Double_Precision;

   --  function To_Real (X : Interfaces.Fortran.Double_Precision) return Real
   --  is
   --  begin
   --     return Real (X);
   --  end To_Real;

   function To_Double_Complex
     (X : Complex) return Interfaces.Fortran.Double_Complex
   is
   begin
      return (Interfaces.Fortran.Double_Precision (X.Re),
              Interfaces.Fortran.Double_Precision (X.Im));
   end To_Double_Complex;

   function To_Complex
     (X : Interfaces.Fortran.Double_Complex) return Complex
   is
   begin
      return (Real (X.Re), Real (X.Im));
   end To_Complex;

   --  This declaration is an Ada-ised version of the Fortran geev,
   --  without the order/leading dimension parameters necessary for
   --  Fortran; the work areas are internally allocated.
   procedure geev
     (Jobv_L :        Character;
      Jobv_R :        Character;
      A      : in out Complex_Matrix;
      W      :    out Complex_Vector;
      V_L    :    out Complex_Matrix;
      V_R    :    out Complex_Matrix;
      Info   :    out Integer);

   function Eigenvalues (A : Complex_Matrix) return Complex_Vector
   is

      Working_A : Complex_Matrix (A'Range (2), A'Range (1));
      Result : Complex_Vector (1 .. A'Length (1));
      Dummy_Eigenvectors : Complex_Matrix (1 .. 1, 1 .. 1);
      Info : Integer;

   begin

      if A'Length (1) /= A'Length (2) then
         raise Constraint_Error with "not square";
      end if;

      Transpose (A, Working_A);

      geev (Jobv_L => 'N', Jobv_R => 'N',
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

   procedure geev
     (Jobv_L :        Character;
      Jobv_R :        Character;
      A      : in out Complex_Matrix;
      W      :    out Complex_Vector;
      V_L    :    out Complex_Matrix;
      V_R    :    out Complex_Matrix;
      Info   :    out Integer)
   is
   begin
      if Is_Single then
         declare
            procedure cgeev
              (Jobv_L :        Character;
               Jobv_R :        Character;
               N      :        Positive;
               A      : in out Complex_Matrix;
               Ld_A   :        Positive;
               W      :    out Complex_Vector;
               V_L    :    out Complex_Matrix;
               Ld_V_L :        Integer;
               V_R    :    out Complex_Matrix;
               Ld_V_R :        Integer;
               Work   :    out Complex_Vector;
               L_Work :        Integer;
               R_Work :    out Complex_Vector;
               Info   :    out Integer);
            pragma Import (Fortran, cgeev, "cgeev_");
            Querying_Work : Complex_Vector (1 .. 1);
            R_Work : Complex_Vector (1 .. 2 * A'Length (1));
         begin
            --  Query the optimum size of the Work vector
            cgeev (Jobv_L, Jobv_R,
                   A'Length (1), A, A'Length (1),
                   W,
                   V_L, V_L'Length (1),
                   V_R, V_R'Length (1),
                   Querying_Work, -1,
                   R_Work,
                   Info);
            declare
               Local_Work :
                 Complex_Vector (1 .. Integer (Querying_Work (1).Re));
            begin
               cgeev (Jobv_L, Jobv_R,
                      A'Length (1), A, A'Length (1),
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
               A      : in out Complex_Matrix;
               Ld_A   :        Positive;
               W      :    out Complex_Vector;
               V_L    :    out Complex_Matrix;
               Ld_V_L :        Integer;
               V_R    :    out Complex_Matrix;
               Ld_V_R :        Integer;
               Work   :    out Complex_Vector;
               L_Work :        Integer;
               R_Work :    out Complex_Vector;
               Info   :    out Integer);
            pragma Import (Fortran, zgeev, "zgeev_");
            Querying_Work : Complex_Vector (1 .. 1);
            R_Work : Complex_Vector (1 .. 2 * A'Length (1));
         begin
            --  Query the optimum size of the Work vector
            zgeev (Jobv_L, Jobv_R,
                   A'Length (1), A, A'Length (1),
                   W,
                   V_L, V_L'Length (1),
                   V_R, V_R'Length (1),
                   Querying_Work, -1,
                   R_Work,
                   Info);
            declare
               Local_Work :
                 Complex_Vector (1 .. Integer (Querying_Work (1).Re));
            begin
               zgeev (Jobv_L, Jobv_R,
                      A'Length (1), A, A'Length (1),
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
   end geev;

end Ada.Numerics.Generic_Complex_Arrays.Extensions;
