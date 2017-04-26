--  Encodes the LAPACK version as an integer: m.n.p is encoded as
--  m*10_000 + n*100 + p, so 3.2.1 comes out as 30201.
function LAPACK_Version return Natural;
