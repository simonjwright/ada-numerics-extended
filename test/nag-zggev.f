*     ZGGEV Example Program Text
*     NAG Copyright 2005.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NB, NMAX
      PARAMETER        (NB=64,NMAX=10)
      INTEGER          LDA, LDB, LDVR, LWORK
      PARAMETER        (LDA=NMAX,LDB=NMAX,LDVR=NMAX,LWORK=NMAX+NMAX*NB)
*     .. Local Scalars ..
      DOUBLE PRECISION SMALL
      INTEGER          I, INFO, J, LWKOPT, N
*     .. Local Arrays ..
      COMPLEX *16      A(LDA,NMAX), ALPHA(NMAX), B(LDB,NMAX),
     +                 BETA(NMAX), DUMMY(1,1), VR(LDVR,NMAX),
     +                 WORK(LWORK)
      DOUBLE PRECISION RWORK(8*NMAX)
*     .. External Functions ..
      DOUBLE PRECISION DLAMCH
      EXTERNAL         DLAMCH
*     .. External Subroutines ..
      EXTERNAL         ZGGEV
*     .. Intrinsic Functions ..
      INTRINSIC        ABS
*     .. Executable Statements ..
      WRITE (NOUT,*) 'ZGGEV Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read in the matrices A and B
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
         READ (NIN,*) ((B(I,J),J=1,N),I=1,N)
*
*        Solve the generalized eigenvalue problem
*
         CALL ZGGEV('No left vectors','Vectors (right)',N,A,LDA,B,LDB,
     +              ALPHA,BETA,DUMMY,1,VR,LDVR,WORK,LWORK,RWORK,INFO)
*
         IF (INFO.GT.0) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99999) 'Failure in ZGGEV. INFO =', INFO
         ELSE
            SMALL = DLAMCH('Sfmin')
            DO 20 J = 1, N
               WRITE (NOUT,*)
               IF ((ABS(ALPHA(J)))*SMALL.GE.ABS(BETA(J))) THEN
                  WRITE (NOUT,99998) 'Eigenvalue(', J, ')',
     +              ' is numerically infinite or undetermined',
     +              'ALPHA(', J, ') = ', ALPHA(J), ', BETA(', J, ') = ',
     +              BETA(J)
               ELSE
                  WRITE (NOUT,99997) 'Eigenvalue(', J, ') = ',
     +              ALPHA(J)/BETA(J)
               END IF
               WRITE (NOUT,*)
               WRITE (NOUT,99996) 'Eigenvector(', J, ')',
     +           (VR(I,J),I=1,N)
   20       CONTINUE
*
            LWKOPT = WORK(1)
            IF (LWORK.LT.LWKOPT) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,99995) 'Optimum workspace required = ',
     +           LWKOPT, 'Workspace provided         = ', LWORK
            END IF
         END IF
      ELSE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'NMAX too small'
      END IF
      STOP
*
99999 FORMAT (1X,A,I4)
99998 FORMAT (1X,A,I2,2A,/1X,2(A,I2,A,'(',1P,E23.15,',',1P,E23.15,')'))
99997 FORMAT (1X,A,I2,A,'(',1P,E23.15,',',1P,E23.15,')')
99996 FORMAT (1X,A,I2,A,/3(1X,'(',1P,E23.15,',',1P,E23.15,')',:))
99995 FORMAT (1X,A,I5,/1X,A,I5)
      END
