*     test_extensions_generator.f

*     generates test data sets for the Ada 2005 Math Extensions project
*     by creating a random single-precision complex matrix, calling
*     cgeev, and outputting the inputs and the results.

      program test_extensions_generator

      external cgeev

      integer n
      parameter (n = 3)

      integer info

      complex*8 h(n, n), w(n), l(n, n), r(n, n), work(128), rwork(n*2)
      real*4 v(2)

*     Orthogonal matrix
      h(1, 1) = cmplx(0.0, 0.0)
      h(1, 2) = cmplx(-0.8, 0.0)
      h(1, 3) = cmplx(-0.6, 0.0)
      h(2, 1) = cmplx(0.8, 0.0)
      h(2, 2) = cmplx(-0.36, 0.0)
      h(2, 3) = cmplx(0.48, 0.0)
      h(3, 1) = cmplx(0.6, 0.0)
      h(3, 2) = cmplx(0.48, 0.0)
      h(3, 3) = cmplx(-0.64, 0.0)

      print *, 'input matrix, size ', n
      do 30 j=1,n
         print *, (h(j, k), k=1,n)
 30   continue

      call cgeev('V', 'V', n, h, n, w, l, n, r, n,
     $     work, 128, rwork, info)

      print *, 'info (should be 0): ', info

      print *, 'eigenvalues'
      do 40 j=1,n
         print *, w(j)
 40   continue

      print *, 'left eigenvectors'
      do 50, j=1,n
         print *, (l(j, k), k=1,n)
 50   continue

      print *, 'right eigenvectors'
      do 60, j=1,n
         print *, (r(j, k), k=1,n)
 60   continue

      end
