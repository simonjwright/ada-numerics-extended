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

with Ada.Exceptions;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Complex_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Arrays;
with Ada_Numerics.Generic_Arrays;

procedure Demo_Extensions is

   subtype My_Float is Float;
   package My_Float_IO is new Float_IO (My_Float);

   package Real_Arrays
   is new Ada.Numerics.Generic_Real_Arrays (My_Float);
   package Complex_Types
   is new Ada.Numerics.Generic_Complex_Types (My_Float);
   package Complex_Arrays
   is new Ada.Numerics.Generic_Complex_Arrays (Real_Arrays, Complex_Types);
   package Extensions
   is new Ada_Numerics.Generic_Arrays (Complex_Arrays);

   package My_Complex_IO is new Complex_IO (Complex_Types);

   use Real_Arrays;
   use Complex_Types;
   use Complex_Arrays;

   use My_Float_IO;
   use My_Complex_IO;

begin

   Put_Line ("--------------------------------");
   Put_Line ("Values from <143ef70b-7e74-426b-a621-a5fd157849be"
               & "@x21g2000yqa.googlegroups.com>");
   --  (posting by yc on comp.lang.ada)
   New_Line;

   declare
      Input : constant Complex_Matrix
        := (42 => ((8.0, 0.0), (-1.0, 0.0), (-5.0, 0.0)),
            43 => ((-4.0, 0.0), (4.0, 0.0), (-2.0, 0.0)),
            44 => ((18.0, 0.0), (-5.0, 0.0), (-7.0, 0.0)));
      Result : constant Complex_Vector := Extensions.Eigenvalues (Input);
   begin

      for J in Result'Range loop
         Put (J, Width => 2);
         Put (" => ");
         Put (Result (J), Exp => 0);
         New_Line;
      end loop;

      New_Line;

   end;

   Put_Line ("--------------------------------");
   Put_Line ("Values in Test16 of "
               & "http://people.sc.fsu.edu/~jburkardt/f_src/lapack"
               & "/lapack_OSX_prb_output.txt");
   New_Line;

   declare
      Z : constant Complex := (0.0, 0.0);
      A : constant Complex := (2.44949, 0.0);
      B : constant Complex := (3.16228, 0.0);
      C : constant Complex := (3.46410, 0.0);
      Input : constant Complex_Matrix
        := (
            1 => (Z, A, Z, Z, Z, Z, Z),
            2 => (A, Z, B, Z, Z, Z, Z),
            3 => (Z, B, Z, C, Z, Z, Z),
            4 => (Z, Z, C, Z, C, Z, Z),
            5 => (Z, Z, Z, C, Z, B, Z),
            6 => (Z, Z, Z, Z, B, Z, A),
            7 => (Z, Z, Z, Z, Z, A, Z)
           );
   begin

      -- GNAT: Eigenvalues of symmetrix complex matrix are real
      Put_Line ("using Complex_Arrays.Eigenvalues");
      declare
         Result : constant Real_Vector := Complex_Arrays.Eigenvalues (Input);
      begin
         for J in Result'Range loop
            Put (Result (J), Exp => 0);
            New_Line;
         end loop;
      end;
      New_Line;

      --  Extension: Eigenvalues of general complex matrix are complex.
      Put_Line ("using Extensions.Eigenvalues");
      declare
         Result : constant Complex_Vector := Extensions.Eigenvalues (Input);
      begin
         for J in Result'Range loop
            Put (Result (J), Exp => 0);
            New_Line;
         end loop;
      end;
      New_Line;

   end;

   Put_Line ("--------------------------------");
   Put_Line
     ("Values from http://en.wikipedia.org/wiki/Skew-symmetric_matrix");
   New_Line;

   declare
      Input : constant Complex_Matrix
        := (((0.0, 0.0), (2.0, 0.0), (-1.0, 0.0)),
            ((-2.0, 0.0), (0.0, 0.0), (-4.0, 0.0)),
            ((1.0, 0.0), (4.0, 0.0), (0.0, 0.0)));
      Result : Complex_Vector (1 .. Input'Length (1));
   begin

      Result := Extensions.Eigenvalues (Input);

      for J in Result'Range loop
         Put (Result (J), Exp => 0);
         New_Line;
      end loop;
      New_Line;

   end;

   Put_Line ("--------------------------------");
   Put_Line
     ("Results from http://en.wikipedia.org/wiki/Orthogonal_matrix");
   New_Line;

   declare
      Input : constant Complex_Matrix
        := (((0.0, 0.0), (-0.8, 0.0), (-0.6, 0.0)),
            ((0.8, 0.0), (-0.36, 0.0), (0.48, 0.0)),
            ((0.6, 0.0), (0.48, 0.0), (-0.64, 0.0)));
      Result : Complex_Vector (Input'Range (1));
      Values : Complex_Vector (Input'Range (1));
      Vectors : Complex_Matrix (Input'Range (1), Input'Range (2));
   begin

      Result := Extensions.Eigenvalues (Input);

      Put_Line ("Eigenvalues:");

      for J in Result'Range loop
         Put (Result (J), Exp => 0);
         New_Line;
      end loop;
      New_Line;

      Extensions.Eigensystem (Input, Values, Vectors);

      Put_Line ("Eignesystem Values:");
      for J in Values'Range loop
         Put (Values (J), Exp => 0);
         New_Line;
      end loop;
      New_Line;

      Put_Line ("Eigensystem Vectors:");
      for J in Vectors'Range (1) loop
         for K in Vectors'Range loop
            Put (Vectors (J, K), Exp => 0);
            Put (" ");
         end loop;
         New_Line;
      end loop;
      New_Line;

   end;

   Put_Line ("--------------------------------");
   Put_Line ("Generalized eigensystem of real non-symmetric matrix.");
   Put_Line ("The solutions are such that beta*a - alpha*b is singular, ie");
   Put_Line ("its determinant is zero. We'll show that it's small for");
   Put_Line ("a selection of randomly-generated matrices.");
   New_Line;

   declare
      Gen : Ada.Numerics.Float_Random.Generator;

      A, B : Real_Arrays.Real_Matrix (1 .. 6, 1 .. 6);
      Vectors : Complex_Arrays.Complex_Matrix (1 .. 6, 1 .. 6);

      Values : Extensions.Generalized_Eigenvalue_Vector (A'Range (1));

      A_Times_Beta, B_Times_Alpha, Difference :
        Complex_Arrays.Complex_Matrix (A'Range (1), A'Range (2));
   begin
      Ada.Numerics.Float_Random.Reset (Gen, 1);

      for T in 1 .. 6 loop

         for J in A'Range (1) loop
            for K in A'Range (2) loop
               A (J, K) := My_Float (Ada.Numerics.Float_Random.Random (Gen));
            end loop;
         end loop;

         for J in B'Range (1) loop
            for K in B'Range (2) loop
               B (J, K) := My_Float (Ada.Numerics.Float_Random.Random (Gen));
            end loop;
         end loop;

         Extensions.Eigensystem (A, B, Values, Vectors);

         for J in Values'Range loop
            begin
               A_Times_Beta := Compose_From_Cartesian (A) * Values (J).Beta;
               B_Times_Alpha := Compose_From_Cartesian (B) * Values (J).Alpha;
               Difference := A_Times_Beta - B_Times_Alpha;
               Put ("j:" & J'Img);
               Put (" determinant:");
               Put (Modulus (Determinant (Difference)));
            exception
               when E : others =>
                  Put (" =====> " & Ada.Exceptions.Exception_Message (E));
            end;
            New_Line;
         end loop;

         New_Line;

      end loop;
   end;

   Put_Line ("--------------------------------");
   Put_Line ("ZGGEV example at http://www.nag.co.uk/lapack-ex/node122.html");
   New_Line;

   declare
      A : constant Complex_Arrays.Complex_Matrix (1 .. 4, 1 .. 4) :=
        (((-21.10,-22.50), ( 53.50,-50.50), (-34.50,127.50), (  7.50,  0.50)),
         (( -0.46, -7.78), ( -3.50,-37.50), (-15.50, 58.50), (-10.50, -1.50)),
         ((  4.30, -5.50), ( 39.70,-17.10), (-68.50, 12.50), ( -7.50, -3.50)),
         ((  5.50,  4.40), ( 14.40, 43.30), (-32.50,-46.00), (-19.00,-32.50)));
      B : constant Complex_Arrays.Complex_Matrix (1 .. 4, 1 .. 4) :=
        (((  1.00, -5.00), (  1.60,  1.20), ( -3.00,  0.00), (  0.00, -1.00)),
         ((  0.80, -0.60), (  3.00, -5.00), ( -4.00,  3.00), ( -2.40, -3.20)),
         ((  1.00,  0.00), (  2.40,  1.80), ( -4.00, -5.00), (  0.00, -3.00)),
         ((  0.00,  1.00), ( -1.80,  2.40), (  0.00, -4.00), (  4.00, -5.00)));
      Values : Extensions.Generalized_Eigenvalue_Vector (1 .. 4);
      Vectors : Complex_Arrays.Complex_Matrix (1 .. 4, 1 .. 4);
   begin
      Extensions.Eigensystem (A, B, Values, Vectors);
      for J in 1 .. 4 loop
         Put ("Eigenvalue(" & J'Img & ") = ");
         Put (Values (J).Alpha / Values (J).Beta);
         New_Line;
         Put ("Eigenvector(" & J'Img & ") = ");
         for K in 1 .. 4 loop
            Put (Vectors (K, J));
         end loop;
         New_Line;
      end loop;
   end;

end Demo_Extensions;
