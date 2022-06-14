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

package body Tests.Real_General_Eigenvalues is

   generic
      type Real is digits <>;
      Type_Name : String;
   package Tests_G is
      function Suite return AUnit.Test_Suites.Access_Test_Suite;
   end Tests_G;

   package body Tests_G is

      procedure Eigenvalues_Constraints (C : in out Test_Case'Class);
      procedure Eigenvalues (C : in out Test_Case'Class);
      procedure Eigensystem_Constraints (C : in out Test_Case'Class);
      procedure Eigensystem (C : in out Test_Case'Class);

      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Warnings (Off, C);
      begin
         return new String'(Type_Name & ": Real_General_Eigenvalues");
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Eigenvalues_Constraints'Unrestricted_Access,
            "Eigenvalues_Constraints");
         Registration.Register_Routine
           (C,
            Eigenvalues'Unrestricted_Access,
            "Eigenvalues");
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

      use Real_Arrays;
      use Complex_Types;
      use Complex_Arrays;

      function Close_Enough (L, R : Complex_Vector) return Boolean
      with Pre => L'Length = R'Length;

      function Column (V : Complex_Matrix; C : Integer) return Complex_Vector;

      --  The values in Input, Expected_Eigenvalues,
      --  Expected_Eigenvectors were derived from a run of
      --  sgeev_generator.

      Input : constant Real_Matrix (3 .. 8, 13 .. 18) :=
        ((  0.99755955     ,
            0.56682467     ,
            0.96591532     ,
            0.74792767     ,
            0.36739087     ,
            0.48063689     ),
         (  7.37542510E-02 ,
            5.35517931E-03 ,
            0.34708124     ,
            0.34224379     ,
            0.21795171     ,
            0.13316035     ),
         (  0.90052450     ,
            0.38676596     ,
            0.44548225     ,
            0.66193217     ,
            1.61082745E-02 ,
            0.65085483     ),
         (  0.64640880     ,
            0.32298726     ,
            0.85569239     ,
            0.40128690     ,
            0.20687431     ,
            0.96853942     ),
         (  0.59839952     ,
            0.67298073     ,
            0.45688230     ,
            0.33001512     ,
            0.10038292     ,
            0.75545329     ),
         (  0.60569322     ,
            0.71904790     ,
            0.89733458     ,
            0.65822911     ,
            0.15071678     ,
            0.61231488     ));

      Expected_Eigenvalues : constant Complex_Vector (Input'Range (1)) :=
        ((   3.1848500     ,   0.0000000     ),
         (  0.22021073     ,  8.75075459E-02 ),
         (  0.22021073     , -8.75075459E-02 ),
         ( -0.42940903     ,   0.0000000     ),
         ( -0.31673914     ,  0.25383112     ),
         ( -0.31673914     , -0.25383112     ));

      Expected_Eigenvectors : constant Complex_Matrix (Input'Range (1),
                                                       Input'Range (2)) :=
        (((  0.53144681     ,  0.0),
          (  0.27645430     ,  0.35389864     ),
          (  0.27645430     , -0.35389864     ),
          ( -0.16669641     ,  0.0),
          ( -0.13764811     , -6.27622157E-02 ),
          ( -0.13764811     ,  6.27622157E-02 )),
         ((  0.14857270     ,  0.0),
          ( -0.31161579     , -9.87667590E-02 ),
          ( -0.31161579     ,  9.87667590E-02 ),
          ( -0.27106366     ,  0.0),
          ( -5.54268509E-02 , -0.46847814     ),
          ( -5.54268509E-02 ,  0.46847814     )),
         ((  0.41218984     ,  0.0),
          (  0.35458034     ,  4.45076749E-02 ),
          (  0.35458034     , -4.45076749E-02 ),
          (  0.72193635     ,  0.0),
          ( -0.19077766     ,  3.68054360E-02 ),
          ( -0.19077766     , -3.68054360E-02 )),
         ((  0.44885054     ,  0.0),
          ( -8.65924060E-02 , -0.17865179     ),
          ( -8.65924060E-02 ,  0.17865179     ),
          ( -0.52945685     ,  0.0),
          (  0.63458711     ,  0.0000000     ),
          (  0.63458711     ,  0.0000000     )),
         ((  0.35383540     ,  0.0),
          ( -0.65427136     ,  0.0000000     ),
          ( -0.65427136     ,  0.0000000     ),
          (  0.30810347     ,  0.0),
          (  0.11322861     ,  0.44579652    ),
          (  0.11322861     , -0.44579652    )),
         ((  0.44600874     ,  0.0),
          ( -0.22177826     , -0.21700253     ),
          ( -0.22177826     ,  0.21700253     ),
          ( -4.78787459E-02 ,  0.0),
          ( -0.21573524     ,  0.23668885     ),
          ( -0.21573524     , -0.23668885     )));

      procedure Eigenvalues_Constraints (C : in out Test_Case'Class)
      is
         pragma Unreferenced (C);
         Unsquare : constant Real_Matrix (1 .. 2, 1 .. 3)
           := (others => (others => 0.0));
      begin
         declare
            Result : constant Complex_Vector
              := Extensions.Eigenvalues (Unsquare);
            pragma Unreferenced (Result);
         begin
            Assert (False, "should have raised Assertion_Error");
         end;
      exception
         when Ada.Assertions.Assertion_Error => null;
      end Eigenvalues_Constraints;

      procedure Eigenvalues (C : in out Test_Case'Class)
      is
         pragma Unreferenced (C);
         Result : constant Complex_Vector := Extensions.Eigenvalues (Input);
      begin
         Assert (Result'First = Input'First (1)
                   and Result'Last = Input'Last (1),
                 "result'range not same as input'range (1)");
         Assert (Close_Enough (Result, Expected_Eigenvalues),
                 "incorrect result");
      end Eigenvalues;

      procedure Eigensystem_Constraints (C : in out Test_Case'Class)
      is
         pragma Unreferenced (C);
         Good_Values : Complex_Vector (Input'Range (1));
         Good_Vectors : Complex_Matrix (Input'Range (1), Input'Range (2));
      begin
         declare
            Bad_Input : constant Real_Matrix (1 .. 2, 1 .. 3)
              := (others => (others => 0.0));
         begin
            Extensions.Eigensystem (A => Bad_Input,
                                    Values => Good_Values,
                                    Vectors => Good_Vectors);
            Assert (False, "should have raised Assertion_Error (1)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
         declare
            Bad_Values : Complex_Vector (1 .. Input'Length (1));
         begin
            Extensions.Eigensystem (A => Input,
                                    Values => Bad_Values,
                                    Vectors => Good_Vectors);
            Assert (False, "should have raised Assertion_Error (2)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
         declare
            Bad_Values : Complex_Vector (Input'First (1) .. Input'Last (1) - 1);
         begin
            Extensions.Eigensystem (A => Input,
                                    Values => Bad_Values,
                                    Vectors => Good_Vectors);
            Assert (False, "should have raised Assertion_Error (3)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
         declare
            Bad_Vectors : Complex_Matrix (1 .. 2, 1 .. 3);
         begin
            Extensions.Eigensystem (A => Input,
                                    Values => Good_Values,
                                    Vectors => Bad_Vectors);
            Assert (False, "should have raised Assertion_Error (4)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
         declare
            Bad_Vectors : Complex_Matrix (1 .. Input'Length (1),
                                          1 .. Input'Length (2));
         begin
            Extensions.Eigensystem (A => Input,
                                    Values => Good_Values,
                                    Vectors => Bad_Vectors);
            Assert (False, "should have raised Assertion_Error (5)");
         exception
            when Ada.Assertions.Assertion_Error => null;
         end;
      end Eigensystem_Constraints;

      procedure Eigensystem (C : in out Test_Case'Class)
      is
         pragma Unreferenced (C);
         Values : Complex_Vector (Input'Range (1));
         Vectors : Complex_Matrix (Input'Range (1), Input'Range (2));
      begin
         Extensions.Eigensystem (A => Input,
                                 Values => Values,
                                 Vectors => Vectors);
         Assert (Close_Enough (Values, Expected_Eigenvalues),
                 "incorrect values");

         --  At this point you might have expected to see
         --
         --  Assert (Close_Enough (Vectors, Expected_Eigenvectors),
         --          "incorrect vectors");
         --
         --  However, this fails (often) because LAPACK can negate a
         --  particular eigenvector. This has been seen with single-
         --  vs double-precision, and on different releases of
         --  LAPACK. Most strangely, the 4th vector returned by
         --  sgeev_generator was returned negated by the Ada binding
         --  on the same platform!
         declare
            J : Integer := Vectors'First (2);
            K : Integer := Expected_Eigenvectors'First (2);
         begin
            loop
               Assert (Close_Enough
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

         --  The arrangement of indexes is sufficiently mind-bending
         --  that we should check that the eigenvalues/vectors do in
         --  fact match.

         for J in Values'Range loop
            declare
               K : constant Integer := J - Values'First + Vectors'First (2);
            begin
               Assert (Close_Enough (Input * Column (Vectors, K),
                                     Values (J) * Column (Vectors, K)),
                       "incorrect vector " & J'Img);
            end;
         end loop;

      end Eigensystem;

      --  This limit may seem a tad on the high side, but all we
      --  really need to know is whether the binding to the LAPACK
      --  subprogram is successful. Experiment shows that putting the
      --  numbers derived from the COMPLEX*16 set into the COMPLEX*8
      --  subprogram gives differences of this size.
      Lim : constant Real := Float'Model_Epsilon * 30.0;

      function Close_Enough (L, R : Complex_Vector) return Boolean
      is
      begin
         for J in L'Range loop
            if abs (L (J).Re - R (J - L'First + R'First).Re) > Lim
            or abs (L (J).Im - R (J - L'First + R'First).Im) > Lim then
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

end Tests.Real_General_Eigenvalues;
