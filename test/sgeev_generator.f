*     sgeev_generator.f

*     generates test data sets for the Ada 2005 Math Extensions project
*     by creating a random single-precision matrix, calling sgeev, and
*     outputting the inputs and the results.

      program sgeev_generator

      external sgeev

      integer n
      parameter (n = 6)

      integer info

      real*4 h(n, n), wr(n), wi(n), l(n, n), r(n, n), work(1024),
     $     rwork(n*2)
      real*4 v(1)

      do 10 j=1,n
         do 20 k=1,n
            call random_number(v)
            h(j, k) = v(1)
 20      continue
 10   continue

      print *, 'input matrix, size ', n
      do 30 j=1,n
         print *, '(', (h(j, k), ',', k=1,n), '),'
 30   continue

      call sgeev('V', 'V', n, h, n, wr, wi, l, n, r, n,
     $     work, 1024, info)

      print *, 'info (should be 0): ', info

      print *, 'eigenvalues'
      do 40 j=1,n
         print *, '(', wr(j), ',', wi(j), '),'
 40   continue

      print *, 'left eigenvectors'
      do 50, j=1,n
         print *, '(', (l(j, k), ',', k=1,n), '),'
 50   continue

      print *, 'right eigenvectors'
      do 60, j=1,n
         print *, '(', (r(j, k), ',', k=1,n), '),'
 60   continue

      end
