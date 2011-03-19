*     zggev_generator.f

*     generates test data sets for the Ada 2005 Math Extensions project
*     by creating two random single-precision complex matrices, calling
*     zggev, and outputting the inputs and the results.

      program zggev_generator

      external zggev

      integer n
      parameter (n = 6)

      integer info

      complex*16 a(n, n), b(n, n), alpha(n), beta(n), l(n, n), r(n, n)
      real*8 work(1024), rwork(8*n)
      real*8 v(2)

      do 10 j=1,n
         do 20 k=1,n
            call random_number(v)
            a(j, k) = cmplx(v(1), v(2))
            call random_number(v)
            b(j, k) = cmplx(v(1), v(2))
 20      continue
 10   continue

      print *, 'input matrix a, size ', n
      do 30 j=1,n
         print *, '(', (a(j, k), ',', k=1,n), '),'
 30   continue
      print *, 'input matrix b, size ', n
      do 35 j=1,n
         print *, '(', (b(j, k), ',', k=1,n), '),'
 35   continue

      call zggev('V', 'V', n, a, n, b, n, alpha, beta, l, n, r, n, work,
     $     1024, rwork, info)

      print *, 'info (should be 0): ', info

      print *, 'eigenvalues - alpha'
      do 40 j=1,n
         print *, alpha(j), ','
 40   continue

      print *, 'eigenvalues - beta'
      do 45 j=1,n
         print *, beta(j), ','
 45   continue

      print *, 'left eigenvectors'
      do 50, j=1,n
         print *, '(', (l(j, k), ',', k=1,n), '),'
 50   continue

      print *, 'right eigenvectors'
      do 60, j=1,n
         print *, '(', (r(j, k), ',', k=1,n), '),'
 60   continue

      end
