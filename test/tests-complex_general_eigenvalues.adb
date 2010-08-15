--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version.  It is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING3.  If not, see
--  <http://www.gnu.org/licenses/>.
--
--  Copyright Simon Wright <simon@pushface.org>

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Arrays;
with Ada.Numerics.Generic_Arrays;

with Ada.Text_IO.Complex_IO; use Ada.Text_IO;
--  May not be referenced for released versions
pragma Warnings (Off, Ada.Text_IO);
pragma Warnings (Off, Ada.Text_IO.Complex_IO);

with Whatever;

package body Tests.Complex_General_Eigenvalues is

   generic
      type Real is digits <>;
      Type_Name : String;
   package Tests_G is
      function Suite return AUnit.Test_Suites.Access_Test_Suite;
   end Tests_G;

   package body Tests_G is

      procedure Eigenvalues_Alone (C : in out Test_Case'Class);

      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Warnings (Off, C);
      begin
         return new String'(Type_Name & ": Complex_General_Eigenvalues");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Eigenvalues_Alone'Unrestricted_Access,
            "Eigenvalues_Alone");
      end Register_Tests;

      function Suite return AUnit.Test_Suites.Access_Test_Suite
      is
         Result : constant AUnit.Test_Suites.Access_Test_Suite
           := new AUnit.Test_Suites.Test_Suite;
      begin
         AUnit.Test_Suites.Add_Test (Result, new Case_1);
         return Result;
      end Suite;

      package Real_Arrays
      is new Ada.Numerics.Generic_Real_Arrays (Real);
      package Complex_Types
      is new Ada.Numerics.Generic_Complex_Types (Real);
      package Complex_Arrays
      is new Ada.Numerics.Generic_Complex_Arrays (Real_Arrays, Complex_Types);
      package Extensions
      is new Ada.Numerics.Generic_Arrays (Complex_Arrays);

      package Real_IO is new Float_IO (Real);
      package My_Complex_IO is new Complex_IO (Complex_Types);

      use Real_Arrays;
      use Complex_Types;
      use Complex_Arrays;

      use Real_IO;
      use My_Complex_IO;

      function Close_Enough (L, R : Complex_Vector) return Boolean;

      --  The values in Input, Eigenvalues were derived from a run of
      --  cgeev_generator.

      Input : constant Complex_Matrix :=
        (3 => (( 0.99755955    , 0.56682467    ),
               ( 0.96591532    , 0.74792767    ),
               ( 0.36739087    , 0.48063689    ),
               ( 7.37542510E-02, 5.35517931E-03),
               ( 0.34708124    , 0.34224379    ),
               ( 0.21795171    , 0.13316035    )),
         4 => (( 0.90052450    , 0.38676596    ),
               ( 0.44548225    , 0.66193217    ),
               ( 1.61082745E-02, 0.65085483    ),
               ( 0.64640880    , 0.32298726    ),
               ( 0.85569239    , 0.40128690    ),
               ( 0.20687431    , 0.96853942    )),
         5 => (( 0.59839952    , 0.67298073    ),
               ( 0.45688230    , 0.33001512    ),
               ( 0.10038292    , 0.75545329    ),
               ( 0.60569322    , 0.71904790    ),
               ( 0.89733458    , 0.65822911    ),
               ( 0.15071678    , 0.61231488    )),
         6 => (( 0.97866023    , 0.99914223    ),
               ( 0.25679797    , 0.55086535    ),
               ( 0.65904748    , 0.55400509    ),
               ( 0.97776008    , 0.90192330    ),
               ( 0.65792465    , 0.72885847    ),
               ( 0.40245521    , 0.92862761    )),
         7 => (( 0.14783514    , 0.67452925    ),
               ( 0.76961428    , 0.33932251    ),
               ( 0.11581880    , 0.61436915    ),
               ( 0.82061714    , 0.94709462    ),
               ( 0.73112863    , 0.49760389    ),
               ( 0.37480170    , 0.42150581    )),
         8 => (( 0.55290300    , 0.99791926    ),
               ( 0.99039471    , 0.74630964    ),
               ( 0.95375901    , 9.32746530E-02),
               ( 0.73402363    , 0.75176162    ),
               ( 0.94684845    , 0.70617634    ),
               ( 0.81380963    , 0.55859447    )));

      Eigenvalues : Complex_Vector :=
         ((  3.3980660    ,  3.5673485    ),
          (  1.0080026    , 5.52535392E-02),
          (-0.62701023    ,-0.18942726    ),
          ( 0.20622893    ,-0.50571519    ),
          ( 0.18741301    , 0.50275004    ),
          (-0.10657825    , 0.51211989    ));

      procedure Eigenvalues_Alone (C : in out Test_Case'Class)
      is
         Result : constant Complex_Vector := Extensions.Eigenvalues (Input);
      begin
         Assert (Result'First = Input'First (1)
                   and Result'Last = Input'Last (1),
                 "result'range not same as input'range (1)");
         Assert (Close_Enough (Result, Eigenvalues),
                 "incorrect result");
      end Eigenvalues_Alone;

      function Close_Enough (L, R : Complex_Vector) return Boolean
      is
         --  This limit may seem a tad on the high side, but all we
         --  really need to know is whether the binding to the LAPACK
         --  subprogram is successful. Experiment shows that putting
         --  the numbers derived from the COMPLEX*16 set into the
         --  COMPLEX*8 subprogram gives differences of this size.
         Lim : constant Real := Float'Model_Epsilon * 30.0;
      begin
         if L'Length /= R'Length then
             raise Constraint_Error with "Close_Enough: different lengths";
         end if;
         for J in L'Range loop
            if abs (L (J).Re - R (J - L'First + R'First).Re) > Lim
            or abs (L (J).Im - R (J - L'First + R'First).Im) > Lim then
               return False;
            end if;
         end loop;
         return True;
      end Close_Enough;

   end Tests_G;

   package Single_Tests is new Tests_G (Float, "Float");
   package Double_Tests is new Tests_G (Long_Float, "Long_Float");
   package Extended_Tests is new Tests_G (Long_Long_Float, "Long_Long_Float");

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, Single_Tests.Suite);
      AUnit.Test_Suites.Add_Test (Result, Double_Tests.Suite);
      AUnit.Test_Suites.Add_Test (Result, Extended_Tests.Suite);
      return Result;
   end Suite;

end Tests.Complex_General_Eigenvalues;
