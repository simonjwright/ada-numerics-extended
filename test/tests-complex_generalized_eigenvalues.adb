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
pragma Warnings (Off);
with System.Generic_Array_Operations;
pragma Warnings (On);

with Ada.Assertions;

with Ada.Text_IO.Complex_IO; use Ada.Text_IO;
--  May not be referenced for released versions
pragma Warnings (Off, Ada.Text_IO);
pragma Warnings (Off, Ada.Text_IO.Complex_IO);

package body Tests.Complex_Generalized_Eigenvalues is

   --  This test suite is written as a two-level generic, because it
   --  turns out that the same input gives wildly different results
   --  depending on the precision (unlike other algorithms).

   --  The outer generic instantiates the required types, the inner
   --  one supplies the appropriate inputs and outputs depending on
   --  the precision.

   generic
      type Real is digits <>;
      Type_Name : String;
      Debug_Output : Boolean := False;
   package Tests_G is

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

      --  The actual tests.
      --  If Expected_Betas has a null range, Expected_Alphas is the ratio.
      generic
         Input_A               : Complex_Arrays.Complex_Matrix;
         Input_B               : Complex_Arrays.Complex_Matrix;
         Expected_Alphas       : Complex_Arrays.Complex_Vector;
         Expected_Betas        : Complex_Arrays.Complex_Vector;
         Expected_Eigenvectors : Complex_Arrays.Complex_Matrix;
         Limit                 : Real;
         Additional_Naming     : String := "";
      package Impl is
         function Suite return AUnit.Test_Suites.Access_Test_Suite;
      end Impl;

      function Transpose (M : Complex_Arrays.Complex_Matrix)
                         return Complex_Arrays.Complex_Matrix;
      --  Useful for constructing eigenvector matrices, with their
      --  Fortran-based organization by column.

   end Tests_G;

   package body Tests_G is

      use Complex_Types;
      use Complex_Arrays;

      package My_Complex_IO is new Complex_IO (Complex_Types);

      use My_Complex_IO;

      function Close_Enough
        (L, R : Complex_Vector; Limit : Real) return Boolean;
      function Column (V : Complex_Matrix; C : Integer) return Complex_Vector;

      package body Impl is

         procedure Eigensystem_Constraints (C : in out Test_Case'Class);
         procedure Eigensystem_Results (C : in out Test_Case'Class);

         type Case_1 is new Test_Case with null record;
         function Name (C : Case_1) return AUnit.Message_String;
         procedure Register_Tests (C : in out Case_1);

         function Name (C : Case_1) return AUnit.Message_String is
            pragma Warnings (Off, C);
         begin
            if Additional_Naming = "" then
               return new String'(Type_Name
                                    & ": Complex_Generalized_Eigenvalues");
            else
               return new String'(Type_Name
                                    & " ("
                                    & Additional_Naming
                                    & "): Complex_Generalized_Eigenvalues");
            end if;
         end Name;

         procedure Register_Tests (C : in out Case_1) is
         begin
            Registration.Register_Routine
              (C,
               Eigensystem_Constraints'Unrestricted_Access,
               "Eigensystem_Constraints");
            Registration.Register_Routine
              (C,
               Eigensystem_Results'Unrestricted_Access,
               "Eigensystem_Results");
         end Register_Tests;

         function Suite return AUnit.Test_Suites.Access_Test_Suite
         is
            Result : constant AUnit.Test_Suites.Access_Test_Suite
              := new AUnit.Test_Suites.Test_Suite;
         begin
            AUnit.Test_Suites.Add_Test (Result, new Case_1);
            return Result;
         end Suite;

         procedure Eigensystem_Constraints (C : in out Test_Case'Class)
         is
            pragma Unreferenced (C);
            Good_Values : Generalized_Eigenvalue_Vector (Input_A'Range (1));
            Good_Vectors : Complex_Matrix (Input_A'Range (1),
                                           Input_A'Range (2));
         begin
            declare
               Bad_Input : constant Complex_Matrix (1 .. 2, 1 .. 3)
                 := (others => (others => (0.0, 0.0)));
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
               Bad_Input : constant Complex_Matrix (1 .. 2, 1 .. 3)
                 := (others => (others => (0.0, 0.0)));
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
               Bad_Input : constant Complex_Matrix (1 .. Input_A'Length (1),
                                                    1 .. Input_A'Length (2))
                 := (others => (others => (0.0, 0.0)));
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
               Bad_Input : constant Complex_Matrix
                 (Input_A'First (1) .. Input_A'Last (1) - 1,
                  Input_A'First (2) .. Input_A'Last (2) - 1)
                 := (others => (others => (0.0, 0.0)));
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

         procedure Eigensystem_Results (C : in out Test_Case'Class)
         is
            pragma Unreferenced (C);
            Values : Generalized_Eigenvalue_Vector (Input_A'Range (1));
            Vectors : Complex_Matrix (Input_A'Range (1), Input_A'Range (2));
         begin

            Extensions.Eigensystem (A => Input_A,
                                    B => Input_B,
                                    Values => Values,
                                    Vectors => Vectors);

            declare
               Alphas : Complex_Vector (Values'Range);
               Betas : Complex_Vector (Values'Range);
            begin
               for J in Values'Range loop
                  Alphas (J) := Values (J).Alpha;
                  Betas (J) := Values (J).Beta;
                  if Expected_Betas'Length = 0 then
                     Alphas (J) := Alphas (J) / Betas (J);
                  end if;
               end loop;
               Assert (Close_Enough (Alphas, Expected_Alphas, Limit),
                       "incorrect Values.Alpha");
               if Expected_Betas'Length /= 0 then
                  Assert (Close_Enough (Betas, Expected_Betas, Limit),
                          "incorrect Values.Beta");
               end if;
            end;

            declare
               Test_OK : Boolean := True;
            begin
               for J in Vectors'Range (2) loop
                  if not (Close_Enough (Column (Vectors, J),
                                        Column (Expected_Eigenvectors, J),
                                        Limit)
                            or else
                            Close_Enough (-Column (Vectors, J),
                                          Column (Expected_Eigenvectors, J),
                                          Limit))
                  then
                     Put_Line (".. column:" & J'Img);
                     Test_OK := False;
                  end if;
               end loop;
               Assert (Test_OK, "incorrect vectors");
            end;

         end Eigensystem_Results;

      end Impl;

      function Transpose (M : Complex_Matrix) return Complex_Matrix
      is
         procedure Transpose
         is new System.Generic_Array_Operations.Transpose
           (Scalar => Complex,
            Matrix => Complex_Matrix);
      begin
         return Result : Complex_Matrix (M'Range (2), M'Range (1)) do
           Transpose (M, Result);
         end return;
      end Transpose;

      function Close_Enough
        (L, R : Complex_Vector; Limit : Real) return Boolean
      is
         Result : Boolean := True;
      begin
         if L'Length /= R'Length then
            raise Constraint_Error
              with "Close_Enough(Complex_Vector): different lengths";
         end if;
         for J in L'Range loop
            declare
               Left : Complex renames L (J);
               Right : Complex renames R (J - L'First + R'First);
            begin
               if abs (Left.Re - Right.Re) > Limit
                 or abs (Left.Im - Right.Im) > Limit then
                  if Debug_Output then
                     Put ("Close_Enough(Complex_Vector): failure:"
                            & " j:" & J'Img
                            & " l:");
                     Put (Left);
                     Put (" r:");
                     Put (Right);
                     Put (" diff:");
                     Put (Left - Right);
                     Put (" limit:");
                     Put (Limit'Img);
                     New_Line;
                  end if;
                  Result := False;
               end if;
            end;
         end loop;
         return Result;
      end Close_Enough;

      function Column
        (V : Complex_Matrix; C : Integer) return Complex_Vector
      is
      begin
         return Result : Complex_Vector (V'Range (1)) do
           for J in V'Range (1) loop
              Result (J) := V (J, C);
           end loop;
         end return;
      end Column;

   end Tests_G;

   package Single_Tests is new Tests_G (Float, "Float");
   package Double_Tests is new Tests_G (Long_Float, "Long_Float");
   package Extended_Tests is new Tests_G (Long_Long_Float, "Long_Long_Float");

   --  The data is derived from a run of sggev_generator.
   Single_Input_A :
     constant Single_Tests.Complex_Arrays.Complex_Matrix (3 .. 8,
                                                          13 .. 18) :=
     ((( 0.99755955    , 0.56682467    ),
       ( 0.36739087    , 0.48063689    ),
       ( 0.34708124    , 0.34224379    ),
       ( 0.90052450    , 0.38676596    ),
       ( 1.61082745E-02, 0.65085483    ),
       ( 0.85569239    , 0.40128690    )),
      (( 0.59839952    , 0.67298073    ),
       ( 0.10038292    , 0.75545329    ),
       ( 0.89733458    , 0.65822911    ),
       ( 0.97866023    , 0.99914223    ),
       ( 0.65904748    , 0.55400509    ),
       ( 0.65792465    , 0.72885847    )),
      (( 0.14783514    , 0.67452925    ),
       ( 0.11581880    , 0.61436915    ),
       ( 0.73112863    , 0.49760389    ),
       ( 0.55290300    , 0.99791926    ),
       ( 0.95375901    , 9.32746530E-02),
       ( 0.94684845    , 0.70617634    )),
      (( 6.17055297E-02, 0.48038077    ),
       ( 0.58739519    , 0.51996821    ),
       ( 0.66965729    , 0.66494006    ),
       ( 7.65594840E-02, 0.10124964    ),
       ( 1.51494741E-02, 0.79291540    ),
       ( 0.95358074    , 0.11424434    )),
      (( 4.81528640E-02, 0.11420578    ),
       ( 7.33417869E-02, 0.24686170    ),
       ( 0.56699830    , 2.43123770E-02),
       ( 0.97658539    , 0.69260502    ),
       ( 4.67772484E-02, 0.83977771    ),
       ( 0.73352587    , 0.11604273    )),
      (( 0.74653637    , 0.84320086    ),
       ( 0.73073655    , 0.41060424    ),
       ( 0.47131765    , 0.46262538    ),
       ( 0.25796580    , 0.93770498    ),
       ( 0.90884805    , 0.69487661    ),
       ( 0.74439669    , 0.30111301    )));

   Single_Input_B :
     constant Single_Tests.Complex_Arrays.Complex_Matrix
     (Single_Input_A'Range (1),
      Single_Input_A'Range (2)) :=
     ((( 0.96591532    , 0.74792767    ),
       ( 7.37542510E-02, 5.35517931E-03),
       ( 0.21795171    , 0.13316035    ),
       ( 0.44548225    , 0.66193217    ),
       ( 0.64640880    , 0.32298726    ),
       ( 0.20687431    , 0.96853942    )),
      (( 0.45688230    , 0.33001512    ),
       ( 0.60569322    , 0.71904790    ),
       ( 0.15071678    , 0.61231488    ),
       ( 0.25679797    , 0.55086535    ),
       ( 0.97776008    , 0.90192330    ),
       ( 0.40245521    , 0.92862761    )),
      (( 0.76961428    , 0.33932251    ),
       ( 0.82061714    , 0.94709462    ),
       ( 0.37480170    , 0.42150581    ),
       ( 0.99039471    , 0.74630964    ),
       ( 0.73402363    , 0.75176162    ),
       ( 0.81380963    , 0.55859447    )),
      (( 0.59768975    , 0.13753188    ),
       ( 0.88587832    , 0.30381012    ),
       ( 0.50367689    , 0.26157510    ),
       ( 0.54926568    , 0.37558490    ),
       ( 0.62087750    , 0.77360356    ),
       ( 0.31846261    , 0.59681982    )),
      (( 0.21596491    , 0.10057336    ),
       ( 0.44338423    , 0.20836753    ),
       ( 0.42029053    , 0.39785302    ),
       ( 4.94331121E-03, 0.12992102    ),
       ( 0.67848879    , 0.58195078    ),
       ( 0.84029961    , 0.83499593    )),
      (( 0.52883899    , 0.66548461    ),
       ( 0.35572159    , 0.73537701    ),
       ( 0.75969166    , 0.70245939    ),
       ( 0.45610356    , 0.80848926    ),
       ( 0.21948850    , 0.85495454    ),
       ( 0.67196852    , 0.61871403    )));

   Single_Expected_Alphas :
     constant Single_Tests.Complex_Arrays.Complex_Vector
     (Single_Input_A'Range (1)) :=
     ((-0.72462112    , 0.66605300    ),
      ( 0.38089162    , 0.88627946    ),
      ( 0.61585903    ,-0.64815724    ),
      (-0.64635521    ,-0.12128220    ),
      (  2.2019682    ,-0.74377418    ),
      ( 0.52759075    ,-9.17982757E-02));

   Single_Expected_Betas :
     constant Single_Tests.Complex_Arrays.Complex_Vector
     (Single_Input_A'Range (1)) :=
     (( 0.39223254    ,  0.0000000    ),
      ( 0.47756130    ,  0.0000000    ),
      ( 0.51605523    ,  0.0000000    ),
      ( 0.97219819    ,  0.0000000    ),
      (  2.2389016    ,  0.0000000    ),
      ( 0.94190317    ,  0.0000000    ));

   Single_Expected_Eigenvectors :
     constant Single_Tests.Complex_Arrays.Complex_Matrix
     (Single_Input_A'Range (1),
      Single_Input_B'Range (2)) :=
     (((-3.41690592E-02, 0.34121007    ),
       (-9.06911045E-02,-0.50845200    ),
       ( 1.89757571E-02,-0.29191309    ),
       (-0.19284678    , 0.18012540    ),
       ( 0.29880726    ,-0.13542424    ),
       ( 0.54871058    , 0.17579372    )),
      ((-0.10953330    , 0.64291245    ),
       ( 0.77856314    ,-0.22143684    ),
       ( 0.35502124    ,-0.49379399    ),
       ( 0.70806199    , 0.26557785    ),
       ( 0.12665448    , 0.52514285    ),
       (-0.89201754    ,-0.10798247    )),
      (( 6.70455918E-02,-0.59220606    ),
       (-0.61433345    , 5.71437217E-02),
       ( 0.40359023    ,-7.00188801E-02),
       (-0.89382565    ,-0.10617443    ),
       (-0.11373845    , 0.10914997    ),
       (-0.28644159    , 9.93528366E-02)),
      (( 0.44573134    ,-0.55426866    ),
       (-0.10562851    ,-1.88317858E-02),
       ( 4.39306535E-02, 0.34227726    ),
       ( 0.22148877    ,-0.13514845    ),
       (-0.18817410    ,-7.23348409E-02),
       (-0.28263345    , 3.14227380E-02)),
      ((-0.21147224    ,-0.15393445    ),
       (-0.30253553    , 0.42683592    ),
       (-1.83209926E-02, 0.81524885    ),
       ( 0.25485277    ,-9.91754308E-02),
       (-0.54370803    ,-0.45629200    ),
       ( 0.17893469    ,-0.29209048    )),
      ((-0.19641124    , 0.26479438    ),
       ( 0.32989562    , 9.17730927E-02),
       (-0.73561502    ,-0.26438498    ),
       (-9.23215821E-02,-0.22580478    ),
       ( 0.15508713    ,-0.69760889    ),
       ( 0.32355800    , 0.24944758    )));

   package Single_Impl is new Single_Tests.Impl
     (Input_A => Single_Input_A,
      Input_B => Single_Input_B,
      Expected_Alphas => Single_Expected_Alphas,
      Expected_Betas => Single_Expected_Betas,
      Expected_Eigenvectors => Single_Expected_Eigenvectors,
      Limit => 1.0e-5);

   --  The data is derived from a run of zggev_generator.
   Double_Input_A :
     constant Double_Tests.Complex_Arrays.Complex_Matrix (3 .. 8,
                                                          13 .. 18) :=
     ((( 0.99755960702896118     , 0.56682473421096802     ),
       ( 0.36739090085029602     , 0.48063689470291138     ),
       ( 0.34708127379417419     , 0.34224382042884827     ),
       ( 0.90052449703216553     , 0.38676601648330688     ),
       ( 1.61083005368709564E-002, 0.65085482597351074     ),
       ( 0.85569238662719727     , 0.40128692984580994     )),
      (( 0.59839951992034912     , 0.67298072576522827     ),
       ( 0.10038292407989502     , 0.75545328855514526     ),
       ( 0.89733457565307617     , 0.65822911262512207     ),
       ( 0.97866022586822510     , 0.99914228916168213     ),
       ( 0.65904754400253296     , 0.55400514602661133     ),
       ( 0.65792471170425415     , 0.72885853052139282     )),
      (( 0.14783519506454468     , 0.67452931404113770     ),
       ( 0.11581885814666748     , 0.61436921358108521     ),
       ( 0.73112863302230835     , 0.49760389328002930     ),
       ( 0.55290305614471436     , 0.99791926145553589     ),
       ( 0.95375907421112061     , 9.32746902108192444E-002),
       ( 0.94684851169586182     , 0.70617634057998657     )),
      (( 6.17055781185626984E-002, 0.48038077354431152     ),
       ( 0.58739519119262695     , 0.51996827125549316     ),
       ( 0.66965728998184204     , 0.66494011878967285     ),
       ( 7.65594989061355591E-002, 0.10124966502189636     ),
       ( 1.51495030149817467E-002, 0.79291546344757080     ),
       ( 0.95358073711395264     , 0.11424437165260315     )),
      (( 4.81529012322425842E-002, 0.11420577764511108     ),
       ( 7.33418017625808716E-002, 0.24686174094676971     ),
       ( 0.56699836254119873     , 2.43123993277549744E-002),
       ( 0.97658544778823853     , 0.69260501861572266     ),
       ( 4.67772595584392548E-002, 0.83977776765823364     ),
       ( 0.73352593183517456     , 0.11604274809360504     )),
      (( 0.74653637409210205     , 0.84320092201232910     ),
       ( 0.73073655366897583     , 0.41060426831245422     ),
       ( 0.47131767868995667     , 0.46262544393539429     ),
       ( 0.25796580314636230     , 0.93770503997802734     ),
       ( 0.90884804725646973     , 0.69487667083740234     ),
       ( 0.74439668655395508     , 0.30111306905746460     )));

   Double_Input_B :
     constant Double_Tests.Complex_Arrays.Complex_Matrix
     (Double_Input_A'Range (1),
      Double_Input_A'Range (2)) :=
     ((( 0.96591538190841675     , 0.74792766571044922     ),
       ( 7.37542659044265747E-002, 5.35522913560271263E-003),
       ( 0.21795172989368439     , 0.13316041231155396     ),
       ( 0.44548228383064270     , 0.66193217039108276     ),
       ( 0.64640879631042480     , 0.32298728823661804     ),
       ( 0.20687432587146759     , 0.96853947639465332     )),
      (( 0.45688229799270630     , 0.33001512289047241     ),
       ( 0.60569328069686890     , 0.71904790401458740     ),
       ( 0.15071684122085571     , 0.61231487989425659     ),
       ( 0.25679799914360046     , 0.55086541175842285     ),
       ( 0.97776007652282715     , 0.90192329883575439     ),
       ( 0.40245527029037476     , 0.92862766981124878     )),
      (( 0.76961433887481689     , 0.33932256698608398     ),
       ( 0.82061713933944702     , 0.94709467887878418     ),
       ( 0.37480175495147705     , 0.42150586843490601     ),
       ( 0.99039477109909058     , 0.74630963802337646     ),
       ( 0.73402369022369385     , 0.75176161527633667     ),
       ( 0.81380969285964966     , 0.55859452486038208     )),
      (( 0.59768974781036377     , 0.13753192126750946     ),
       ( 0.88587832450866699     , 0.30381017923355103     ),
       ( 0.50367689132690430     , 0.26157513260841370     ),
       ( 0.54926574230194092     , 0.37558495998382568     ),
       ( 0.62087756395339966     , 0.77360355854034424     ),
       ( 0.31846264004707336     , 0.59681981801986694     )),
      (( 0.21596491336822510     , 0.10057339072227478     ),
       ( 0.44338425993919373     , 0.20836757123470306     ),
       ( 0.42029058933258057     , 0.39785301685333252     ),
       ( 4.94336755946278572E-003, 0.12992103397846222     ),
       ( 0.67848885059356689     , 0.58195084333419800     ),
       ( 0.84029966592788696     , 0.83499598503112793     )),
      (( 0.52883899211883545     , 0.66548466682434082     ),
       ( 0.35572159290313721     , 0.73537701368331909     ),
       ( 0.75969171524047852     , 0.70245939493179321     ),
       ( 0.45610359311103821     , 0.80848932266235352     ),
       ( 0.21948850154876709     , 0.85495454072952271     ),
       ( 0.67196851968765259     , 0.61871403455734253     )));

   Double_Expected_Alphas :
     constant Double_Tests.Complex_Arrays.Complex_Vector
     (Double_Input_A'Range (1)) :=
     ((-0.72462137727084108     , 0.66605284400204401     ),
      ( 0.38089150123582910     , 0.88627961630480201     ),
      (-0.64196175825401258     ,-0.12045771725621814     ),
      ( 0.62007403908784364     ,-0.65259336565559700     ),
      (  2.2019693761831594     ,-0.74377395504137078     ),
      ( 0.52759066348006678     ,-9.17980559652656625E-002));

   Double_Expected_Betas :
     constant Double_Tests.Complex_Arrays.Complex_Vector
     (Double_Input_A'Range (1)) :=
     (( 0.39223242731531105     ,  0.0000000000000000     ),
      ( 0.47756126471998211     ,  0.0000000000000000     ),
      ( 0.96558994968501355     ,  0.0000000000000000     ),
      ( 0.51958743124576134     ,  0.0000000000000000     ),
      (  2.2389017108697922     ,  0.0000000000000000     ),
      ( 0.94190294494813787     ,  0.0000000000000000     ));

   Double_Expected_Eigenvectors :
     constant Double_Tests.Complex_Arrays.Complex_Matrix
     (Double_Input_A'Range (1),
      Double_Input_B'Range (2)) :=
     (((-8.40656466581337150E-003, 0.34664542621724620     ),
       ( 6.21121712215207053E-002,-0.63323784986162979     ),
       (-0.21450330305538842     ,-7.66602177307265098E-002),
       ( 0.25260679374520156     , 9.00310139007541449E-002),
       ( 0.29881527806205832     ,-0.13540040921933222     ),
       ( 0.54871069909424020     , 0.17579324594467974     )),
      ((-6.13655573904763121E-002, 0.65659973009420114     ),
       ( 0.99719193664281780     ,-2.80806335718215014E-003),
       ( 7.64720452724648864E-002, 0.64829780145503546     ),
       ( 0.34621743159109913     , 0.43700577424819742     ),
       ( 0.12661385107114020     , 0.52514882689654929     ),
       (-0.89201833333728031     ,-0.10798166666271966     )),
      (( 2.23962116559053882E-002,-0.60222986928648559     ),
       (-0.74760925130497535     ,-0.13722683664302515     ),
       (-0.27227505359572124     ,-0.72772494640427876     ),
       (-3.96092536052228827E-002, 0.37341646201720402     ),
       (-0.11374640573517417     , 0.10914045817685453     ),
       (-0.28644158750620391     , 9.93527614856379859E-002)),
      (( 0.40711535463760518     ,-0.59288464536239494     ),
       (-0.11898190806251309     ,-5.75796267653002625E-002),
       ( 0.19133370587153931     , 0.11643262199988723     ),
       (-0.31280778181100533     ,-4.72146440539824297E-002),
       (-0.18816748922853238     ,-7.23488877591436791E-002),
       (-0.28263331715255124     , 3.14230051572193039E-002)),
      ((-0.22497442766594572     ,-0.13906737903009458     ),
       (-0.50120681073010565     , 0.40523516996093001     ),
       ( 0.17694021812530289     , 0.15626453249333030     ),
       (-0.71418177605252076     ,-0.22086519171187932     ),
       (-0.54366994717920081     ,-0.45633005282079914     ),
       ( 0.17893481474433970     ,-0.29209063829692694     )),
      ((-0.17782586443391793     , 0.28198049558708305     ),
       ( 0.36060141962977393     , 0.21891531827625882     ),
       ( 0.13669243124775099     ,-0.16018673864998911     ),
       ( 0.41781662983049417     ,-0.58218337016950583     ),
       ( 0.15513868689302518     ,-0.69759200329309068     ),
       ( 0.32355854452626087     , 0.24944776982410577     )));

   package Double_Impl is new Double_Tests.Impl
     (Input_A => Double_Input_A,
      Input_B => Double_Input_B,
      Expected_Alphas => Double_Expected_Alphas,
      Expected_Betas => Double_Expected_Betas,
      Expected_Eigenvectors => Double_Expected_Eigenvectors,
      Limit => 1.0e-10);

   --  The data is from the ZGGEV example at
   --  http://www.nag.co.uk/lapack-ex/node122.html, implemented here
   --  with extended output precision in nag-zggef.f.
   package Double_Impl_NAG is new Double_Tests.Impl
     (Input_A =>
        (((-21.10,-22.50), ( 53.50,-50.50), (-34.50,127.50), (  7.50,  0.50)),
         (( -0.46, -7.78), ( -3.50,-37.50), (-15.50, 58.50), (-10.50, -1.50)),
         ((  4.30, -5.50), ( 39.70,-17.10), (-68.50, 12.50), ( -7.50, -3.50)),
         ((  5.50,  4.40), ( 14.40, 43.30), (-32.50,-46.00), (-19.00,-32.50))),
      Input_B =>
        (((  1.00, -5.00), (  1.60,  1.20), ( -3.00,  0.00), (  0.00, -1.00)),
         ((  0.80, -0.60), (  3.00, -5.00), ( -4.00,  3.00), ( -2.40, -3.20)),
         ((  1.00,  0.00), (  2.40,  1.80), ( -4.00, -5.00), (  0.00, -3.00)),
         ((  0.00,  1.00), ( -1.80,  2.40), (  0.00, -4.00), (  4.00, -5.00))),
      Expected_Alphas =>
        ((  2.999999999999997    , -9.000000000000002    ),
         (  2.000000000000001    , -5.000000000000001    ),
         (  3.000000000000001    , -9.999999999999971E-01),
         (  4.000000000000000    , -4.999999999999999    )),
      Expected_Betas => (1 .. 0 => (0.0, 0.0)),
      Expected_Eigenvectors =>
        (Double_Tests.Transpose
           ((((  8.237684355586408E-01,  1.762315644413593E-01),
              (  1.529507374223460E-01, -7.065516195641947E-02),
              (  7.065516195641951E-02,  1.529507374223459E-01),
              ( -1.529507374223459E-01,  7.065516195641937E-02)),
             (( -6.397414100896659E-01, -3.602585899103342E-01),
              ( -4.159704468673991E-03,  5.465027092886775E-04),
              ( -4.021231720563600E-02, -2.264482565150679E-02),
              (  2.264482565150676E-02, -4.021231720563594E-02)),
             ((  9.775354973150105E-01,  2.246450268498953E-02),
              (  1.591014198926005E-01, -1.137099392482031E-01),
              (  1.208985801073995E-01, -1.537099392482032E-01),
              (  1.537099392482029E-01, 1.208985801073995E-01)),
             ((  9.062337812121569E-01, -9.376621878784308E-02),
              (  7.430303263300258E-03, -6.875036041750603E-03),
              ( -3.020779270707200E-02,  3.125540626261550E-03),
              (  1.458585625588661E-02,  1.409696992996683E-01))))),
      Limit => 1.0e-10,
      Additional_Naming => "NAG ZGGEV example");

   --  The data is derived from a run of zggev_generator.
   Extended_Input_A :
     constant Extended_Tests.Complex_Arrays.Complex_Matrix (3 .. 8,
                                                            13 .. 18) :=
     ((( 0.99755960702896118     , 0.56682473421096802     ),
       ( 0.36739090085029602     , 0.48063689470291138     ),
       ( 0.34708127379417419     , 0.34224382042884827     ),
       ( 0.90052449703216553     , 0.38676601648330688     ),
       ( 1.61083005368709564E-002, 0.65085482597351074     ),
       ( 0.85569238662719727     , 0.40128692984580994     )),
      (( 0.59839951992034912     , 0.67298072576522827     ),
       ( 0.10038292407989502     , 0.75545328855514526     ),
       ( 0.89733457565307617     , 0.65822911262512207     ),
       ( 0.97866022586822510     , 0.99914228916168213     ),
       ( 0.65904754400253296     , 0.55400514602661133     ),
       ( 0.65792471170425415     , 0.72885853052139282     )),
      (( 0.14783519506454468     , 0.67452931404113770     ),
       ( 0.11581885814666748     , 0.61436921358108521     ),
       ( 0.73112863302230835     , 0.49760389328002930     ),
       ( 0.55290305614471436     , 0.99791926145553589     ),
       ( 0.95375907421112061     , 9.32746902108192444E-002),
       ( 0.94684851169586182     , 0.70617634057998657     )),
      (( 6.17055781185626984E-002, 0.48038077354431152     ),
       ( 0.58739519119262695     , 0.51996827125549316     ),
       ( 0.66965728998184204     , 0.66494011878967285     ),
       ( 7.65594989061355591E-002, 0.10124966502189636     ),
       ( 1.51495030149817467E-002, 0.79291546344757080     ),
       ( 0.95358073711395264     , 0.11424437165260315     )),
      (( 4.81529012322425842E-002, 0.11420577764511108     ),
       ( 7.33418017625808716E-002, 0.24686174094676971     ),
       ( 0.56699836254119873     , 2.43123993277549744E-002),
       ( 0.97658544778823853     , 0.69260501861572266     ),
       ( 4.67772595584392548E-002, 0.83977776765823364     ),
       ( 0.73352593183517456     , 0.11604274809360504     )),
      (( 0.74653637409210205     , 0.84320092201232910     ),
       ( 0.73073655366897583     , 0.41060426831245422     ),
       ( 0.47131767868995667     , 0.46262544393539429     ),
       ( 0.25796580314636230     , 0.93770503997802734     ),
       ( 0.90884804725646973     , 0.69487667083740234     ),
       ( 0.74439668655395508     , 0.30111306905746460     )));

   Extended_Input_B :
     constant Extended_Tests.Complex_Arrays.Complex_Matrix
     (Extended_Input_A'Range (1),
      Extended_Input_A'Range (2)) :=
     ((( 0.96591538190841675     , 0.74792766571044922     ),
       ( 7.37542659044265747E-002, 5.35522913560271263E-003),
       ( 0.21795172989368439     , 0.13316041231155396     ),
       ( 0.44548228383064270     , 0.66193217039108276     ),
       ( 0.64640879631042480     , 0.32298728823661804     ),
       ( 0.20687432587146759     , 0.96853947639465332     )),
      (( 0.45688229799270630     , 0.33001512289047241     ),
       ( 0.60569328069686890     , 0.71904790401458740     ),
       ( 0.15071684122085571     , 0.61231487989425659     ),
       ( 0.25679799914360046     , 0.55086541175842285     ),
       ( 0.97776007652282715     , 0.90192329883575439     ),
       ( 0.40245527029037476     , 0.92862766981124878     )),
      (( 0.76961433887481689     , 0.33932256698608398     ),
       ( 0.82061713933944702     , 0.94709467887878418     ),
       ( 0.37480175495147705     , 0.42150586843490601     ),
       ( 0.99039477109909058     , 0.74630963802337646     ),
       ( 0.73402369022369385     , 0.75176161527633667     ),
       ( 0.81380969285964966     , 0.55859452486038208     )),
      (( 0.59768974781036377     , 0.13753192126750946     ),
       ( 0.88587832450866699     , 0.30381017923355103     ),
       ( 0.50367689132690430     , 0.26157513260841370     ),
       ( 0.54926574230194092     , 0.37558495998382568     ),
       ( 0.62087756395339966     , 0.77360355854034424     ),
       ( 0.31846264004707336     , 0.59681981801986694     )),
      (( 0.21596491336822510     , 0.10057339072227478     ),
       ( 0.44338425993919373     , 0.20836757123470306     ),
       ( 0.42029058933258057     , 0.39785301685333252     ),
       ( 4.94336755946278572E-003, 0.12992103397846222     ),
       ( 0.67848885059356689     , 0.58195084333419800     ),
       ( 0.84029966592788696     , 0.83499598503112793     )),
      (( 0.52883899211883545     , 0.66548466682434082     ),
       ( 0.35572159290313721     , 0.73537701368331909     ),
       ( 0.75969171524047852     , 0.70245939493179321     ),
       ( 0.45610359311103821     , 0.80848932266235352     ),
       ( 0.21948850154876709     , 0.85495454072952271     ),
       ( 0.67196851968765259     , 0.61871403455734253     )));

   Extended_Expected_Alphas :
     constant Extended_Tests.Complex_Arrays.Complex_Vector
     (Extended_Input_A'Range (1)) :=
     ((-0.72462137727084108     , 0.66605284400204401     ),
      ( 0.38089150123582910     , 0.88627961630480201     ),
      (-0.64196175825401258     ,-0.12045771725621814     ),
      ( 0.62007403908784364     ,-0.65259336565559700     ),
      (  2.2019693761831594     ,-0.74377395504137078     ),
      ( 0.52759066348006678     ,-9.17980559652656625E-002));

   Extended_Expected_Betas :
     constant Extended_Tests.Complex_Arrays.Complex_Vector
     (Extended_Input_A'Range (1)) :=
     (( 0.39223242731531105     ,  0.0000000000000000     ),
      ( 0.47756126471998211     ,  0.0000000000000000     ),
      ( 0.96558994968501355     ,  0.0000000000000000     ),
      ( 0.51958743124576134     ,  0.0000000000000000     ),
      (  2.2389017108697922     ,  0.0000000000000000     ),
      ( 0.94190294494813787     ,  0.0000000000000000     ));

   Extended_Expected_Eigenvectors :
     constant Extended_Tests.Complex_Arrays.Complex_Matrix
     (Extended_Input_A'Range (1),
      Extended_Input_B'Range (2)) :=
     (((-8.40656466581337150E-003, 0.34664542621724620     ),
       ( 6.21121712215207053E-002,-0.63323784986162979     ),
       (-0.21450330305538842     ,-7.66602177307265098E-002),
       ( 0.25260679374520156     , 9.00310139007541449E-002),
       ( 0.29881527806205832     ,-0.13540040921933222     ),
       ( 0.54871069909424020     , 0.17579324594467974     )),
      ((-6.13655573904763121E-002, 0.65659973009420114     ),
       ( 0.99719193664281780     ,-2.80806335718215014E-003),
       ( 7.64720452724648864E-002, 0.64829780145503546     ),
       ( 0.34621743159109913     , 0.43700577424819742     ),
       ( 0.12661385107114020     , 0.52514882689654929     ),
       (-0.89201833333728031     ,-0.10798166666271966     )),
      (( 2.23962116559053882E-002,-0.60222986928648559     ),
       (-0.74760925130497535     ,-0.13722683664302515     ),
       (-0.27227505359572124     ,-0.72772494640427876     ),
       (-3.96092536052228827E-002, 0.37341646201720402     ),
       (-0.11374640573517417     , 0.10914045817685453     ),
       (-0.28644158750620391     , 9.93527614856379859E-002)),
      (( 0.40711535463760518     ,-0.59288464536239494     ),
       (-0.11898190806251309     ,-5.75796267653002625E-002),
       ( 0.19133370587153931     , 0.11643262199988723     ),
       (-0.31280778181100533     ,-4.72146440539824297E-002),
       (-0.18816748922853238     ,-7.23488877591436791E-002),
       (-0.28263331715255124     , 3.14230051572193039E-002)),
      ((-0.22497442766594572     ,-0.13906737903009458     ),
       (-0.50120681073010565     , 0.40523516996093001     ),
       ( 0.17694021812530289     , 0.15626453249333030     ),
       (-0.71418177605252076     ,-0.22086519171187932     ),
       (-0.54366994717920081     ,-0.45633005282079914     ),
       ( 0.17893481474433970     ,-0.29209063829692694     )),
      ((-0.17782586443391793     , 0.28198049558708305     ),
       ( 0.36060141962977393     , 0.21891531827625882     ),
       ( 0.13669243124775099     ,-0.16018673864998911     ),
       ( 0.41781662983049417     ,-0.58218337016950583     ),
       ( 0.15513868689302518     ,-0.69759200329309068     ),
       ( 0.32355854452626087     , 0.24944776982410577     )));

   package Extended_Impl is new Extended_Tests.Impl
     (Input_A => Extended_Input_A,
      Input_B => Extended_Input_B,
      Expected_Alphas => Extended_Expected_Alphas,
      Expected_Betas => Extended_Expected_Betas,
      Expected_Eigenvectors => Extended_Expected_Eigenvectors,
      Limit => 1.0e-10);

   --  The data is from the ZGGEV example at
   --  http://www.nag.co.uk/lapack-ex/node122.html, implemented here
   --  with extended output precision in nag-zggef.f.
   package Extended_Impl_NAG is new Extended_Tests.Impl
     (Input_A =>
        (((-21.10,-22.50), ( 53.50,-50.50), (-34.50,127.50), (  7.50,  0.50)),
         (( -0.46, -7.78), ( -3.50,-37.50), (-15.50, 58.50), (-10.50, -1.50)),
         ((  4.30, -5.50), ( 39.70,-17.10), (-68.50, 12.50), ( -7.50, -3.50)),
         ((  5.50,  4.40), ( 14.40, 43.30), (-32.50,-46.00), (-19.00,-32.50))),
      Input_B =>
        (((  1.00, -5.00), (  1.60,  1.20), ( -3.00,  0.00), (  0.00, -1.00)),
         ((  0.80, -0.60), (  3.00, -5.00), ( -4.00,  3.00), ( -2.40, -3.20)),
         ((  1.00,  0.00), (  2.40,  1.80), ( -4.00, -5.00), (  0.00, -3.00)),
         ((  0.00,  1.00), ( -1.80,  2.40), (  0.00, -4.00), (  4.00, -5.00))),
      Expected_Alphas =>
        ((  2.999999999999997    , -9.000000000000002    ),
         (  2.000000000000001    , -5.000000000000001    ),
         (  3.000000000000001    , -9.999999999999971E-01),
         (  4.000000000000000    , -4.999999999999999    )),
      Expected_Betas => (1 .. 0 => (0.0, 0.0)),
      Expected_Eigenvectors =>
        (Extended_Tests.Transpose
           ((((  8.237684355586408E-01,  1.762315644413593E-01),
              (  1.529507374223460E-01, -7.065516195641947E-02),
              (  7.065516195641951E-02,  1.529507374223459E-01),
              ( -1.529507374223459E-01,  7.065516195641937E-02)),
             (( -6.397414100896659E-01, -3.602585899103342E-01),
              ( -4.159704468673991E-03,  5.465027092886775E-04),
              ( -4.021231720563600E-02, -2.264482565150679E-02),
              (  2.264482565150676E-02, -4.021231720563594E-02)),
             ((  9.775354973150105E-01,  2.246450268498953E-02),
              (  1.591014198926005E-01, -1.137099392482031E-01),
              (  1.208985801073995E-01, -1.537099392482032E-01),
              (  1.537099392482029E-01, 1.208985801073995E-01)),
             ((  9.062337812121569E-01, -9.376621878784308E-02),
              (  7.430303263300258E-03, -6.875036041750603E-03),
              ( -3.020779270707200E-02,  3.125540626261550E-03),
              (  1.458585625588661E-02,  1.409696992996683E-01))))),
      Limit => 1.0e-10,
      Additional_Naming => "NAG ZGGEV example");

   function Suite return AUnit.Test_Suites.Access_Test_Suite
   is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, Single_Impl.Suite);
      AUnit.Test_Suites.Add_Test (Result, Double_Impl.Suite);
      AUnit.Test_Suites.Add_Test (Result, Double_Impl_NAG.Suite);
      AUnit.Test_Suites.Add_Test (Result, Extended_Impl.Suite);
      AUnit.Test_Suites.Add_Test (Result, Extended_Impl_NAG.Suite);
      return Result;
   end Suite;

end Tests.Complex_Generalized_Eigenvalues;
