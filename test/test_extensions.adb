--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version.  It is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING3 respectively.  If
--  not, see <http://www.gnu.org/licenses/>.
--
--  Copyright Simon Wright <simon@pushface.org>

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Arrays.Extensions;

procedure Test_Extensions is

   subtype My_Float is Float;

   package Real_Arrays
   is new Ada.Numerics.Generic_Real_Arrays (My_Float);
   package Complex_Types
   is new Ada.Numerics.Generic_Complex_Types (My_Float);
   package Complex_Arrays
   is new Ada.Numerics.Generic_Complex_Arrays (Real_Arrays, Complex_Types);
   package Extensions
   is new Complex_Arrays.Extensions;

   use Real_Arrays;
   use Complex_Types;
   use Complex_Arrays;

begin

   declare
   --  Values in yc's example
      Input : constant Complex_Matrix
        := (((8.0, 0.0), (-1.0, 0.0), (-5.0, 0.0)),
            ((-4.0, 0.0), (4.0, 0.0), (-2.0, 0.0)),
            ((18.0, 0.0), (-5.0, 0.0), (-7.0, 0.0)));
      Result : Complex_Vector (1 .. Input'Length (1));
   begin
      Result := Extensions.Eigenvalues (Input);

      for J in Result'Range loop
         Put_Line (My_Float'Base'Image (Result (J).Re)
                     & " "
                     & My_Float'Base'Image (Result (J).Im));
      end loop;
      New_Line;
   end;

   declare
   --  Values in Test 16 of
   --  http://people.sc.fsu.edu/~jburkardt/f_src/lapack/lapack_OSX_prb_output.txt
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
      declare
         Result : constant Real_Vector := Complex_Arrays.Eigenvalues (Input);
      begin
         for J in Result'Range loop
            Put_Line (My_Float'Base'Image (Result (J)));
         end loop;
      end;
      New_Line;
      declare
         Result : constant Complex_Vector := Extensions.Eigenvalues (Input);
      begin
         for J in Result'Range loop
            Put_Line (My_Float'Base'Image (Result (J).Re)
                        & " "
                        & My_Float'Base'Image (Result (J).Im));
         end loop;
      end;
      New_Line;
   end;

end Test_Extensions;
