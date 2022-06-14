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
with Ada_Numerics.Generic_Arrays;

with Ada.Assertions;

with Ada.Text_IO.Complex_IO; use Ada.Text_IO;
--  May not be referenced for released versions
pragma Warnings (Off, Ada.Text_IO);
pragma Warnings (Off, Ada.Text_IO.Complex_IO);

with LAPACK_Version;

package body Tests.Real_Generalized_Eigenvalues is

   generic
      type Real is digits <>;
      Type_Name : String;
   package Tests_G is
      function Suite return AUnit.Test_Suites.Access_Test_Suite;
   end Tests_G;

   package body Tests_G is

      procedure Eigensystem_Constraints (C : in out Test_Case'Class);
      procedure Eigensystem (C : in out Test_Case'Class);

      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Warnings (Off, C);
      begin
         return new String'(Type_Name & ": Real_Generalized_Eigenvalues");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Eigensystem_Constraints'Unrestricted_Access,
            "Eigensystem_Constraints");
         Registration.Register_Routine
           (C,
            Eigensystem'Unrestricted_Access,
            "Eigensystem");
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
      is new Ada_Numerics.Generic_Arrays (Complex_Arrays);

      subtype Generalized_Eigenvalue_Vector
        is Extensions.Generalized_Eigenvalue_Vector;

      use Real_Arrays;
      use Complex_Types;
      use Complex_Arrays;

      --  This limit may seem a tad on the high side, but all we
      --  really need to know is whether the binding to the LAPACK
      --  subprogram is successful. Experiment shows that putting the
      --  numbers derived from the COMPLEX*16 set into the COMPLEX*8
      --  subprogram gives differences of this size.
      Lim : constant Real := Float'Model_Epsilon * 30.0;

      function Close_Enough (L, R : Complex_Vector) return Boolean
      with Pre => L'Length = R'Length;

      function Column (V : Complex_Matrix; C : Integer) return Complex_Vector;

      --  The values in Input_A, Input_B, Expected_Alphas,
      --  Expected_Betas, Expected_Eigenvectors were derived from a
      --  run of sggev_generator.
      --
      --  Note that the expected results changed at LAPACK 3.5.0!

      Input_A : constant Real_Matrix (3 .. 8, 13 .. 18) :=
        ((  0.99755955     ,
         0.96591532     ,
         0.36739087     ,
         7.37542510E-02 ,
         0.34708124     ,
         0.21795171     ),
         (  0.90052450     ,
          0.44548225     ,
          1.61082745E-02 ,
          0.64640880     ,
          0.85569239     ,
          0.20687431     ),
         (  0.59839952     ,
          0.45688230     ,
          0.10038292     ,
          0.60569322     ,
          0.89733458     ,
          0.15071678     ),
         (  0.97866023     ,
          0.25679797     ,
          0.65904748     ,
          0.97776008     ,
          0.65792465     ,
          0.40245521     ),
         (  0.14783514     ,
          0.76961428     ,
          0.11581880     ,
          0.82061714     ,
          0.73112863     ,
          0.37480170     ),
         (  0.55290300     ,
          0.99039471     ,
          0.95375901     ,
          0.73402363     ,
          0.94684845     ,
          0.81380963     ));

      Input_B : constant Real_Matrix (Input_A'Range (1), Input_A'Range (2)) :=
        ((  0.56682467     ,
         0.74792767     ,
         0.48063689     ,
         5.35517931E-03 ,
         0.34224379     ,
         0.13316035     ),
         (  0.38676596     ,
          0.66193217     ,
          0.65085483     ,
          0.32298726     ,
          0.40128690     ,
          0.96853942     ),
         (  0.67298073     ,
          0.33001512     ,
          0.75545329     ,
          0.71904790     ,
          0.65822911     ,
          0.61231488     ),
         (  0.99914223     ,
          0.55086535     ,
          0.55400509     ,
          0.90192330     ,
          0.72885847     ,
          0.92862761     ),
         (  0.67452925     ,
          0.33932251     ,
          0.61436915     ,
          0.94709462     ,
          0.49760389     ,
          0.42150581     ),
         (  0.99791926     ,
          0.74630964     ,
          9.32746530E-02 ,
          0.75176162     ,
          0.70617634     ,
          0.55859447     ));

      --  Expected values are computed at package elaboration.

      Expected_Alphas : Complex_Vector (Input_A'Range (1));

      Expected_Betas : Real_Vector (Input_A'Range (1));

      Expected_Eigenvectors : Complex_Matrix (Input_A'Range (1),
                                              Input_B'Range (2));

      procedure Eigensystem_Constraints (C : in out Test_Case'Class)
      is
         pragma Unreferenced (C);
         Good_Values : Generalized_Eigenvalue_Vector (Input_A'Range (1));
         Good_Vectors : Complex_Matrix (Input_A'Range (1), Input_A'Range (2));
      begin
         declare
            Bad_Input : constant Real_Matrix (1 .. 2, 1 .. 3)
              := (others => (others => 0.0));
         begin
            Extensions.Eigensystem
              (A => Bad_Input,
               B => Input_B,
               Values => Good_Values,
               Vectors => Good_Vectors);
            Assert (False, "should have raised Assertion_Error (1)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
         declare
            Bad_Input : constant Real_Matrix (1 .. 2, 1 .. 3)
              := (others => (others => 0.0));
         begin
            Extensions.Eigensystem
              (A => Input_A,
               B => Bad_Input,
               Values => Good_Values,
               Vectors => Good_Vectors);
            Assert (False, "should have raised Assertion_Error (2)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
         declare
            Bad_Input : constant Real_Matrix (1 .. Input_A'Length (1),
                                              1 .. Input_A'Length (2))
              := (others => (others => 0.0));
         begin
            Extensions.Eigensystem
              (A => Input_A,
               B => Bad_Input,
               Values => Good_Values,
               Vectors => Good_Vectors);
            Assert (False, "should have raised Assertion_Error (3)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
         declare
            Bad_Input : constant Real_Matrix
              (Input_A'First (1) .. Input_A'Last (1) - 1,
               Input_A'First (2) .. Input_A'Last (2) - 1)
                := (others => (others => 0.0));
         begin
            Extensions.Eigensystem
              (A => Input_A,
               B => Bad_Input,
               Values => Good_Values,
               Vectors => Good_Vectors);
            Assert (False, "should have raised Assertion_Error (4)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
         declare
            Bad_Values :
            Generalized_Eigenvalue_Vector (1 .. Input_A'Length (1));
         begin
            Extensions.Eigensystem
              (A => Input_A,
               B => Input_B,
               Values => Bad_Values,
               Vectors => Good_Vectors);
            Assert (False, "should have raised Assertion_Error (5)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
         declare
            Bad_Values :
            Generalized_Eigenvalue_Vector
              (Input_A'First (1) .. Input_A'Last (1) - 1);
         begin
            Extensions.Eigensystem
              (A => Input_A,
               B => Input_B,
               Values => Bad_Values,
               Vectors => Good_Vectors);
            Assert (False, "should have raised Assertion_Error (6)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
         declare
            Bad_Vectors : Complex_Matrix (1 .. 2, 1 .. 3);
         begin
            Extensions.Eigensystem
              (A => Input_A,
               B => Input_B,
               Values => Good_Values,
               Vectors => Bad_Vectors);
            Assert (False, "should have raised Assertion_Error (7)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
         declare
            Bad_Vectors : Complex_Matrix (1 .. Input_A'Length (1),
                                          1 .. Input_A'Length (2));
         begin
            Extensions.Eigensystem
              (A => Input_A,
               B => Input_B,
               Values => Good_Values,
               Vectors => Bad_Vectors);
            Assert (False, "should have raised Assertion_Error (8)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
      end Eigensystem_Constraints;

      procedure Eigensystem (C : in out Test_Case'Class)
      is
         pragma Unreferenced (C);
         Values : Generalized_Eigenvalue_Vector (Input_A'Range (1));
         Vectors : Complex_Matrix (Input_A'Range (1), Input_A'Range (2));
      begin

         Extensions.Eigensystem (A => Input_A,
                                 B => Input_B,
                                 Values => Values,
                                 Vectors => Vectors);

         for J in Values'Range loop
            Assert (Modulus (Values (J).Alpha - Expected_Alphas (J)) <= Lim,
                    "incorrect Values.Alpha");
            Assert (abs (Values (J).Beta.Re - Expected_Betas (J)) <= Lim,
                    "incorrect Values.Beta");
         end loop;

         --  At this point you might have expected to see
         --
         --  Assert (Close_Enough (Vectors, Expected_Eigenvectors),
         --          "incorrect vectors");
         --
         --  However, this fails (often) because LAPACK can negate a
         --  particular eigenvector. This has been seen with single-
         --  vs double-precision, and on different releases of
         --  LAPACK.
         declare
            J : Integer := Vectors'First (2);
            K : Integer := Expected_Eigenvectors'First (2);
         begin
            loop
               declare
                  Actual : constant Complex_Vector
                    := Column (Vectors, J);
                  Expected : constant Complex_Vector
                    := Column (Expected_Eigenvectors, K);
               begin
                  Assert (Close_Enough (Actual, Expected)
                          or else
                          Close_Enough (Actual, -Expected),
                          "incorrect vector " & K'Img);
                  exit when J = Vectors'Last (2);
                  J := J + 1;
                  K := K + 1;
               end;
            end loop;
         end;

      end Eigensystem;

      function Close_Enough (L, R : Complex_Vector) return Boolean
      is
      begin
         if L'Length /= R'Length then
            raise Constraint_Error with "Close_Enough: different lengths";
         end if;
         for J in L'Range loop
            if abs (L (J).Re - R (J - L'First + R'First).Re) > Lim
              or abs (L (J).Im - R (J - L'First + R'First).Im) > Lim
            then
               return False;
            end if;
         end loop;
         return True;
      end Close_Enough;

      function Column (V : Complex_Matrix; C : Integer) return Complex_Vector
      is
         Result : Complex_Vector (V'Range (1));
      begin
         for J in V'Range (1) loop
            Result (J) := V (J, C);
         end loop;
         return Result;
      end Column;

   begin
      if LAPACK_Version < 30500 then
         Expected_Alphas :=
           ((   1.00140500     ,   0.00000000     ),
            ( -0.822484076     ,   0.00000000     ),
            (  0.308495104     ,  0.264515132     ),
            (  0.235172346     , -0.201645479     ),
            (   1.41685843     ,   0.00000000     ),
            (  0.432987601     ,   0.00000000     ));

         Expected_Betas :=
           ((  0.221702680     ),
            (  0.738268852     ),
            (  0.778071642     ),
            (  0.593140483     ),
            (   1.09822965     ),
            (  0.239632174     ));

         Expected_Eigenvectors :=
           (((-1.00000000, 0.0),
             (-0.496568143, 0.0),
             (0.200492918, -5.11028878E-02),
             (0.200492918, 5.11028878E-02),
             (-3.50932032E-02, 0.0),
             (0.556000769, 0.0)),
            ((0.488186270, 0.0),
             (-5.67027591E-02, 0.0),
             (-0.370432556, -6.61318526E-02),
             (-0.370432556, 6.61318526E-02),
             (-1.00000000, 0.0),
             (-0.519566178, 0.0)),
            ((-0.125129744, 0.0),
             (1.00000000, 0.0),
             (-0.27180528, 0.152554125),
             (-0.27180528, -0.152554125),
             (-0.229509324, 0.0),
             (5.55794686E-02, 0.0)),
            ((0.676800549, 0.0),
             (-0.201009586, 0.0),
             (-8.27412680E-02, 0.176907450),
             (-8.27412680E-02, -0.176907450),
             (-0.528019786, 0.0),
             (-1.00000000, 0.0)),
            ((0.628045917, 0.0),
             (-8.34041759E-02, 0.0),
             (8.45926031E-02, 0.129796341),
             (8.45926031E-02, -0.129796341),
             (0.412864298, 0.0),
             (0.322295547, 0.0)),
            ((-0.283281505, 0.0),
             (0.231684163, 0.0),
             (0.763694108, -0.236305878),
             (0.763694108, 0.236305878),
             (0.577049077, 0.0),
             (0.270170867, 0.0)));
      else
         Expected_Alphas :=
           ((   1.00140440     ,   0.00000000     ),
            ( -0.822484553     ,   0.00000000     ),
            (  0.308494955     ,  0.264514446     ),
            (  0.235172167     , -0.201644912     ),
            (  0.284790188     ,   0.00000000     ),
            (   2.15415454     ,   0.00000000     ));

         Expected_Betas :=
           ((  0.221702456     ),
            (  0.738268673     ),
            (  0.778071404     ),
            (  0.593140125     ),
            (  0.157614022     ),
            (   1.66972101     ));

         Expected_Eigenvectors :=
           (((-1.00000000, 0.0),
             (-0.496568412, 0.0),
             (0.200492606, -5.11028580E-02),
             (0.200492606, 5.11028580E-02),
             (0.556000769, 0.0),
             (3.50940190E-02, 0.0)),
            ((0.488185912, 0.0),
             (-5.67027628E-02, 0.0),
             (-0.370432585, -6.61318600E-02),
             (-0.370432585, 6.61318600E-02),
             (-0.519566774, 0.0),
             (1.00000000, 0.0)),
            ((-0.125129789, 0.0),
             (1.00000000, 0.0),
             (-0.271805733, 0.152553663),
             (-0.271805733, -0.152553663),
             (5.55791818E-02, 0.0),
             (0.229510009, 0.0)),
            ((0.676800013, 0.0),
             (-0.201009721, 0.0),
             (-8.27407986E-02, 0.176907077),
             (-8.27407986E-02, -0.176907077),
             (-1.00000000, 0.0),
             (0.528018117, 0.0)),
            ((0.628045678, 0.0),
             (-8.34040344E-02, 0.0),
             (8.45923871E-02, 0.129796267),
             (8.45923871E-02, -0.129796267),
             (0.322295755, 0.0),
             (-0.412863880, 0.0)),
            ((-0.283280969, 0.0),
             (0.231684491, 0.0),
             (0.763694644, -0.236305326),
             (0.763694644, 0.236305326),
             (0.270171165, 0.0),
             (-0.577049255, 0.0)));
        end if;
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

end Tests.Real_Generalized_Eigenvalues;
