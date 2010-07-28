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

   package Real_Arrays
   is new Ada.Numerics.Generic_Real_Arrays (Long_Float);
   package Complex_Types
   is new Ada.Numerics.Generic_Complex_Types (Long_Float);
   package Complex_Arrays
   is new Ada.Numerics.Generic_Complex_Arrays (Real_Arrays, Complex_Types);
   package Extensions
   is new Complex_Arrays.Extensions;

   use Complex_Arrays;

   Input : Complex_Matrix (1 .. 3, 1 .. 3);
   Result : Complex_Vector (1 .. Input'Length (1));

begin

   --  Values in yc's example
   Input := (((8.0, 0.0), (-1.0, 0.0), (-5.0, 0.0)),
             ((-4.0, 0.0), (4.0, 0.0), (-2.0, 0.0)),
             ((18.0, 0.0), (-5.0, 0.0), (-7.0, 0.0)));

   Result := Extensions.Eigenvalues (Input);

   for J in Result'Range loop
      Put_Line (Long_Float'Image (Result (J).Re)
                  & " "
                  & Long_Float'Image (Result (J).Im));
   end loop;

end Test_Extensions;
