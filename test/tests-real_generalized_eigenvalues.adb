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

with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Numerics.Generic_Real_Arrays;
with Ada.Numerics.Generic_Complex_Types;
with Ada.Numerics.Generic_Complex_Arrays;
with Ada_Numerics.Generic_Arrays;

with Ada.Text_IO.Complex_IO; use Ada.Text_IO;
--  May not be referenced for released versions
pragma Warnings (Off, Ada.Text_IO);
pragma Warnings (Off, Ada.Text_IO.Complex_IO);

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

      package Real_IO is new Float_IO (Real);
      package My_Complex_IO is new Complex_IO (Complex_Types);

      use Real_Arrays;
      use Complex_Types;
      use Complex_Arrays;

      use Real_IO;
      use My_Complex_IO;

      --  This limit may seem a tad on the high side, but all we
      --  really need to know is whether the binding to the LAPACK
      --  subprogram is successful. Experiment shows that putting the
      --  numbers derived from the COMPLEX*16 set into the COMPLEX*8
      --  subprogram gives differences of this size.
      Lim : constant Real := Float'Model_Epsilon * 30.0;

      function Close_Enough (L, R : Real_Vector) return Boolean;
      function Column (V : Real_Matrix; C : Integer) return Real_Vector;

      --  The values in Input_A, Input_B, Expected_Alphas,
      --  Expected_Betas, Expected_Eigenvectors were derived from a
      --  run of sggev_generator.

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

      Expected_Alphas : constant Complex_Vector (Input_A'Range (1)) :=
        ((   1.0014051     ,   0.0000000     ),
         ( -0.82248443     ,   0.0000000     ),
         (  0.30849504     ,  0.26451489     ),
         (  0.23517229     , -0.20164527     ),
         (   1.4168590     ,   0.0000000     ),
         (  0.43298730     ,   0.0000000     ));

      Expected_Betas : constant Real_Vector (Input_A'Range (1)) :=
        ((  0.22170286     ),
         (  0.73826897     ),
         (  0.77807158     ),
         (  0.59314036     ),
         (   1.0982305     ),
         (  0.23963201     ));

      Expected_Eigenvectors : constant Real_Matrix (Input_A'Range (1),
                                                    Input_B'Range (2)) :=
        (( -1.0000000      ,
           -0.49656805     ,
            0.20049269     ,
           -5.11029027E-02 ,
           -3.50931920E-02 ,
            0.55600142     ),
         (  0.48818636     ,
           -5.67028634E-02 ,
           -0.37043244     ,
           -6.61318377E-02 ,
           -1.0000000      ,
           -0.51956600     ),
         ( -0.12512936     ,
            1.0000000      ,
           -0.27180523     ,
            0.15255395     ,
           -0.22950920     ,
            5.55792861E-02 ),
         (  0.67680043     ,
           -0.20100972     ,
           -8.27410221E-02 ,
            0.17690741     ,
           -0.52802014     ,
           -1.0000000     ),
         (  0.62804544     ,
           -8.34040195E-02 ,
            8.45926255E-02 ,
            0.12979619     ,
            0.41286322     ,
            0.32229346     ),
         ( -0.28328142     ,
            0.23168407     ,
            0.76369429     ,
           -0.23630577     ,
            0.57704848     ,
            0.27017081     ));

      procedure Eigensystem_Constraints (C : in out Test_Case'Class)
      is
         Good_Values : Generalized_Eigenvalue_Vector (Input_A'Range (1));
         Good_Vectors : Real_Matrix (Input_A'Range (1), Input_A'Range (2));
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
            Assert (C, False, "should have raised Constraint_Error (1)");
         exception
            when Constraint_Error => null;
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
            Assert (C, False, "should have raised Constraint_Error (2)");
         exception
            when Constraint_Error => null;
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
            Assert (C, False, "should have raised Constraint_Error (3)");
         exception
            when Constraint_Error => null;
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
            Assert (C, False, "should have raised Constraint_Error (4)");
         exception
            when Constraint_Error => null;
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
            Assert (C, False, "should have raised Constraint_Error (5)");
         exception
            when Constraint_Error => null;
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
            Assert (C, False, "should have raised Constraint_Error (6)");
         exception
            when Constraint_Error => null;
         end;
         declare
            Bad_Vectors : Real_Matrix (1 .. 2, 1 .. 3);
         begin
            Extensions.Eigensystem
              (A => Input_A,
               B => Input_B,
               Values => Good_Values,
               Vectors => Bad_Vectors);
            Assert (C, False, "should have raised Constraint_Error (7)");
         exception
            when Constraint_Error => null;
         end;
         declare
            Bad_Vectors : Real_Matrix (1 .. Input_A'Length (1),
                                          1 .. Input_A'Length (2));
         begin
            Extensions.Eigensystem
              (A => Input_A,
               B => Input_B,
               Values => Good_Values,
               Vectors => Bad_Vectors);
            Assert (C, False, "should have raised Constraint_Error (8)");
         exception
            when Constraint_Error => null;
         end;
      end Eigensystem_Constraints;

      procedure Eigensystem (C : in out Test_Case'Class)
      is
         Values : Generalized_Eigenvalue_Vector (Input_A'Range (1));
         Vectors : Real_Matrix (Input_A'Range (1), Input_A'Range (2));
      begin

         Extensions.Eigensystem (A => Input_A,
                                 B => Input_B,
                                 Values => Values,
                                 Vectors => Vectors);

         for J in Values'Range loop
            Assert (C,
                    Modulus (Values (J).Alpha - Expected_Alphas (J)) <= Lim,
                    "incorrect Values.Alpha");
            Assert (C,
                    abs (Values (J).Beta.Re - Expected_Betas (J)) <= Lim,
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
               Assert (C,
                       Close_Enough
                         (Column (Vectors, J),
                          Column (Expected_Eigenvectors, K))
                         or else
                         Close_Enough
                         (Column (Vectors, J),
                          -Column (Expected_Eigenvectors, K)),
                       "incorrect vector " & K'Img);
               exit when J = Vectors'Last (2);
               J := J + 1;
               K := K + 1;
            end loop;
         end;

      end Eigensystem;

      function Close_Enough (L, R : Real_Vector) return Boolean
      is
      begin
         if L'Length /= R'Length then
             raise Constraint_Error with "Close_Enough: different lengths";
         end if;
         for J in L'Range loop
            if abs (L (J) - R (J - L'First + R'First)) > Lim then
               return False;
            end if;
         end loop;
         return True;
      end Close_Enough;

      function Column (V : Real_Matrix; C : Integer) return Real_Vector
      is
         Result : Real_Vector (V'Range (1));
      begin
         for J in V'Range (1) loop
            Result (J) := V (J, C);
         end loop;
         return Result;
      end Column;

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
