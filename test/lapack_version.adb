with Interfaces.Fortran; use Interfaces.Fortran;
function LAPACK_Version return Natural
is
   Major, Minor, Patch : Fortran_Integer;
   procedure Ilaver (Major : out Fortran_Integer;
                     Minor : out Fortran_Integer;
                     Patch : out Fortran_Integer)
     with
       Import,
       Convention => Fortran,
       External_Name => "ilaver_";
begin
   Ilaver (Major, Minor, Patch);
   return Natural (Major * 10000 + Minor * 100 + Patch);
end LAPACK_Version;
