*     sggev_generator.f

*     generates test data sets for the Ada 2005 Math Extensions project
*     by creating two random single-precision matrices, calling sggev,
*     and outputting the inputs and the results.

      program sggev_generator

      external sggev

      integer n
      parameter (n = 6)

      integer info

      real*4 a(n, n), b(n, n), ar(n), ai(n), be(n), l(n, n), r(n, n),
     $     work(1024)
      real*4 v(1)

      do 10 j=1,n
         do 20 k=1,n
            call random_number(v)
            a(j, k) = v(1)
            call random_number(v)
            b(j, k) = v(1)
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

      call sggev('V', 'V', n, a, n, b, n, ar, ai, be, l, n, r, n, work,
     $     1024,info)

      print *, 'info (should be 0): ', info

      print *, 'eigenvalues - alpha'
      do 40 j=1,n
         print *, '(', ar(j), ',', ai(j), '),'
 40   continue

      print *, 'eigenvalues - beta'
      do 45 j=1,n
         print *, '(', be(j), '),'
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
