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
with Ada.Numerics.Generic_Complex_Arrays.Extensions;

with Ada.Text_IO.Complex_IO; use Ada.Text_IO;
--  May not be referenced for released versions
pragma Warnings (Off, Ada.Text_IO);
pragma Warnings (Off, Ada.Text_IO.Complex_IO);

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
      is new Complex_Arrays.Extensions;

      package Real_IO is new Float_IO (Real);
      package My_Complex_IO is new Complex_IO (Complex_Types);

      use Real_Arrays;
      use Complex_Types;
      use Complex_Arrays;

      use Real_IO;
      use My_Complex_IO;

      function Close_Enough (L, R : Complex_Vector) return Boolean;

      --  The values in Input, Eigenvalues were derived from a run of
      --  zgeev_generator.
      Input : constant Complex_Matrix :=
        (3 => (( 0.99755960702896118     , 0.56682473421096802     ),
               ( 0.96591538190841675     , 0.74792766571044922     ),
               ( 0.36739090085029602     , 0.48063689470291138     ),
               ( 7.37542659044265747E-002, 5.35522913560271263E-003),
               ( 0.34708127379417419     , 0.34224382042884827     ),
               ( 0.21795172989368439     , 0.13316041231155396     )),
         4 => (( 0.90052449703216553     , 0.38676601648330688     ),
               ( 0.44548228383064270     , 0.66193217039108276     ),
               ( 1.61083005368709564E-002, 0.65085482597351074     ),
               ( 0.64640879631042480     , 0.32298728823661804     ),
               ( 0.85569238662719727     , 0.40128692984580994     ),
               ( 0.20687432587146759     , 0.96853947639465332     )),
         5 => (( 0.59839951992034912     , 0.67298072576522827     ),
               ( 0.45688229799270630     , 0.33001512289047241     ),
               ( 0.10038292407989502     , 0.75545328855514526     ),
               ( 0.60569328069686890     , 0.71904790401458740     ),
               ( 0.89733457565307617     , 0.65822911262512207     ),
               ( 0.15071684122085571     , 0.61231487989425659     )),
         6 => (( 0.97866022586822510     , 0.99914228916168213     ),
               ( 0.25679799914360046     , 0.55086541175842285     ),
               ( 0.65904754400253296     , 0.55400514602661133     ),
               ( 0.97776007652282715     , 0.90192329883575439     ),
               ( 0.65792471170425415     , 0.72885853052139282     ),
               ( 0.40245527029037476     , 0.92862766981124878     )),
         7 => (( 0.14783519506454468     , 0.67452931404113770     ),
               ( 0.76961433887481689     , 0.33932256698608398     ),
               ( 0.11581885814666748     , 0.61436921358108521     ),
               ( 0.82061713933944702     , 0.94709467887878418     ),
               ( 0.73112863302230835     , 0.49760389328002930     ),
               ( 0.37480175495147705     , 0.42150586843490601     )),
         8 => (( 0.55290305614471436     , 0.99791926145553589     ),
               ( 0.99039477109909058     , 0.74630963802337646     ),
               ( 0.95375907421112061     , 9.32746902108192444E-002),
               ( 0.73402369022369385     , 0.75176161527633667     ),
               ( 0.94684851169586182     , 0.70617634057998657     ),
               ( 0.81380969285964966     , 0.55859452486038208     )));

      Eigenvalues : Complex_Vector :=
        ((  3.3980669045124534     ,  3.5673510676968099     ),
         (  1.0080025235206473     , 5.52534645505778910E-002),
         (-0.62700993355392609     ,-0.18942703058527749     ),
         ( 0.20622879636121216     ,-0.50571525284129615     ),
         ( 0.18741315261406835     , 0.50274987810127159     ),
         (-0.10657822611016853     , 0.51211978321127938     ));

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
         Lim : constant Real := Float'Model_Epsilon * 100.0;
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
