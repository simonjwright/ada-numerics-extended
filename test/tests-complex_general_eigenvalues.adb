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

package body Tests.Complex_General_Eigenvalues is

   generic
      type Real is digits <>;
      Type_Name : String;
      Debug_Output : Boolean := False;
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
         return new String'(Type_Name & ": Complex_General_Eigenvalues");
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

      package My_Complex_IO is new Complex_IO (Complex_Types);

      use Complex_Types;
      use Complex_Arrays;

      use My_Complex_IO;

      function Close_Enough (L, R : Complex_Vector) return Boolean
      with Pre => L'Length = R'Length;
      function Close_Enough (L, R : Complex_Matrix) return Boolean
      with
        Pre =>
          L'Length (1) = R'Length (1)
          and then L'Length (2) = R'Length (2);

      --  The values in Input, Expected_Eigenvalues,
      --  Expected_Eigenvectors were derived from a run of
      --  cgeev_generator.

      Input : constant Complex_Matrix (3 .. 8, 13 .. 18) :=
        ((( 0.99755955    , 0.56682467    ),
          ( 0.96591532    , 0.74792767    ),
          ( 0.36739087    , 0.48063689    ),
          ( 7.37542510E-02, 5.35517931E-03),
          ( 0.34708124    , 0.34224379    ),
          ( 0.21795171    , 0.13316035    )),
         (( 0.90052450    , 0.38676596    ),
          ( 0.44548225    , 0.66193217    ),
          ( 1.61082745E-02, 0.65085483    ),
          ( 0.64640880    , 0.32298726    ),
          ( 0.85569239    , 0.40128690    ),
          ( 0.20687431    , 0.96853942    )),
         (( 0.59839952    , 0.67298073    ),
          ( 0.45688230    , 0.33001512    ),
          ( 0.10038292    , 0.75545329    ),
          ( 0.60569322    , 0.71904790    ),
          ( 0.89733458    , 0.65822911    ),
          ( 0.15071678    , 0.61231488    )),
         (( 0.97866023    , 0.99914223    ),
          ( 0.25679797    , 0.55086535    ),
          ( 0.65904748    , 0.55400509    ),
          ( 0.97776008    , 0.90192330    ),
          ( 0.65792465    , 0.72885847    ),
          ( 0.40245521    , 0.92862761    )),
         (( 0.14783514    , 0.67452925    ),
          ( 0.76961428    , 0.33932251    ),
          ( 0.11581880    , 0.61436915    ),
          ( 0.82061714    , 0.94709462    ),
          ( 0.73112863    , 0.49760389    ),
          ( 0.37480170    , 0.42150581    )),
         (( 0.55290300    , 0.99791926    ),
          ( 0.99039471    , 0.74630964    ),
          ( 0.95375901    , 9.32746530E-02),
          ( 0.73402363    , 0.75176162    ),
          ( 0.94684845    , 0.70617634    ),
          ( 0.81380963    , 0.55859447    )));

      Expected_Eigenvalues : constant Complex_Vector (Input'Range (1)) :=
        ((  3.3980660    ,  3.5673485    ),
         (  1.0080026    , 5.52535392E-02),
         (-0.62701023    ,-0.18942726    ),
         ( 0.20622893    ,-0.50571519    ),
         ( 0.18741301    , 0.50275004    ),
         (-0.10657825    , 0.51211989    ));

      Expected_Eigenvectors : constant Complex_Matrix (Input'Range (1),
                                                       Input'Range (2)) :=
        ((( 0.26146498    , 5.53096458E-03),
          ( 0.69007766    ,  0.0000000    ),
          (-0.33993864    ,-0.12087621    ),
          (-0.10242952    , 0.13319522    ),
          (-0.22170562    , 0.47241959    ),
          (-0.34357452    ,-0.11636004    )),
         (( 0.36076644    , 8.12109038E-02),
          ( 0.11586758    ,-0.25473198    ),
          ( 0.57077682    ,  0.0000000    ),
          (-0.10233923    ,-0.30746639    ),
          (-6.63828179E-02,-0.35655662    ),
          (-2.05811113E-04,-7.92269558E-02)),
         (( 0.35763562    , 0.11501306    ),
          (-0.12223477    ,-8.74236524E-02),
          ( 0.31982315    , 0.13336638    ),
          ( 8.52892101E-02, 0.10423380    ),
          ( 0.20212649    , 0.13214748    ),
          ( 0.30959696    ,-0.27752465    )),
         (( 0.48191616    , 0.11476357    ),
          (-3.46576311E-02, 0.10269910    ),
          ( 0.29120964    , 0.14564611    ),
          (-0.30022317    ,-0.31935146    ),
          ( 0.55859631    ,  0.0000000    ),
          (-7.58073777E-02, 0.45906258    )),
         (( 0.37139055    , 8.32461789E-02),
          (-0.39627567    , 7.07832426E-02),
          (-0.30821502    ,-0.15343353    ),
          ( 0.58892167    ,  0.0000000    ),
          (-3.68152745E-04,-0.10933587    ),
          ( 0.52521509    ,  0.0000000    )),
         (( 0.51327550    ,  0.0000000    ),
          (-0.42405269    , 0.26321325    ),
          (-0.42857879    , 0.12544817    ),
          ( 7.74565339E-03, 0.55642736    ),
          (-0.44734421    ,-0.11707998    ),
          (-0.43265030    , 9.87282395E-02)));

      procedure Eigenvalues_Constraints (C : in out Test_Case'Class)
      is
         pragma Unreferenced (C);
         Unsquare : constant Complex_Matrix (1 .. 2, 1 .. 3)
           := (others => (others => (0.0, 0.0)));
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
            Bad_Input : constant Complex_Matrix (1 .. 2, 1 .. 3)
              := (others => (others => (0.0, 0.0)));
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
         Assert (Close_Enough (Vectors, Expected_Eigenvectors),
                 "incorrect vectors");
      end Eigensystem;

      --  This limit may seem a tad on the high side, but all we
      --  really need to know is whether the binding to the LAPACK
      --  subprogram is successful. Experiment shows that putting
      --  the numbers derived from the COMPLEX*16 set into the
      --  COMPLEX*8 subprogram gives differences of this size.
      Lim : constant Real := Float'Model_Epsilon * 60.0;

      function Close_Enough (L, R : Complex_Vector) return Boolean
      is
      begin
         for J in L'Range loop
            declare
               Left : Complex renames L (J);
               Right : Complex renames R (J - L'First + R'First);
            begin
               if abs (Left.Re - Right.Re) > Lim
                 or abs (Left.Im - Right.Im) > Lim then
                  if Debug_Output then
                     Put ("Close_Enough(Complex_Vector): failure:"
                            & " j:" & J'Img
                            & " l:");
                     Put (Left);
                     Put (" r:");
                     Put (Right);
                     Put (" diff:");
                     Put (Left - Right);
                     Put (" lim:");
                     Put (Lim'Img);
                     New_Line;
                  end if;
                  return False;
               end if;
            end;
         end loop;
         return True;
      end Close_Enough;

      function Close_Enough (L, R : Complex_Matrix) return Boolean
      is
      begin
         for J in L'Range (1) loop
            for K in L'Range (2) loop
               declare
                  Left : Complex renames L (J, K);
                  Right : Complex renames R (J - L'First (1) + R'First (1),
                                             K - L'First (2) + R'First (2));
               begin
                  if abs (Left.Re - Right.Re) > Lim
                    or abs (Left.Im - Right.Im) > Lim then
                     if Debug_Output then
                        Put ("Close_Enough(Complex_Matrix): failure:"
                               & " j:" & J'Img
                               & " k:" & K'Img
                               & " l:");
                        Put (Left);
                        Put (" r:");
                        Put (Right);
                        Put (" diff:");
                        Put (Left - Right);
                        Put (" lim:");
                        Put (Lim'Img);
                        New_Line;
                     end if;
                     return False;
                  end if;
               end;
            end loop;
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
