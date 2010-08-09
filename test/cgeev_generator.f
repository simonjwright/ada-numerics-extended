*     cgeev_generator.f

*     generates test data sets for the Ada 2005 Math Extensions project
*     by creating a random single-precision complex matrix, calling
*     cgeev, and outputting the inputs and the results.

      program cgeev_generator

      external cgeev

      integer n
      parameter (n = 6)

      integer info

      complex*8 h(n, n), w(n), l(n, n), r(n, n), work(128), rwork(n*2)
      real*4 v(2)

      do 10 j=1,n
         do 20 k=1,n
            call random_number(v)
            h(j, k) = cmplx(v(1), v(2))
 20      continue
 10   continue

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
