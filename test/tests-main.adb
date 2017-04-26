
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

with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;

with Tests.Complex_General_Eigenvalues;
with Tests.Real_General_Eigenvalues;
with Tests.Complex_Generalized_Eigenvalues;
with Tests.Real_Generalized_Eigenvalues;

with LAPACK_Version;
with Ada.Text_IO;

procedure Tests.Main is

   function Suites return AUnit.Test_Suites.Access_Test_Suite;
   procedure Run is new AUnit.Run.Test_Runner (Suites);

   function Suites return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result,
                                  Complex_General_Eigenvalues.Suite);
      AUnit.Test_Suites.Add_Test (Result,
                                  Real_General_Eigenvalues.Suite);
      AUnit.Test_Suites.Add_Test (Result,
                                  Complex_Generalized_Eigenvalues.Suite);
      AUnit.Test_Suites.Add_Test (Result,
                                  Real_Generalized_Eigenvalues.Suite);
      return Result;
   end Suites;

   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Ada.Text_IO.Put_Line ("LAPACK version is" & LAPACK_Version'Img);
   Run (Reporter);
end Tests.Main;
