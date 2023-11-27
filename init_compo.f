!                    *********************
                     SUBROUTINE INIT_COMPO
!                    *********************
! 27/11/2023 modified by e.roome to allow for a spatially hetrgenous sediemnt distribution in SISYPHE.
! The modified subroutine reads 3 sediment % variables named: FRAC1, FRAC2, FRAC3 from the geometry file.
! These varibles represent the proportion of a particular grain size at each node.
! The sum of the three varibles must be 1 at every node.
! The D50 of each fraction is defined in the SISYPHE stering file as usual.
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE   V7P2
!***********************************************************************
!| NCOUCHES       |-->| NUMBER OF LAYER FOR EACH POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,IERR
      DOUBLE PRECISION EPAI
!      INTEGER, PARAMETER :: NPOIN = 324863
!      DOUBLE PRECISION, DIMENSION(NPOIN, NCOUCHES, NSICLA) :: AVAIL
!      DOUBLE PRECISION, DIMENSION(NPOIN, NCOUCHES) :: ES
      DOUBLE PRECISION :: FRAC_C(NPOIN)
      CHARACTER*16 VAR_NAME
!
!-----------------------------------------------------------------------
!
!  TO BE FILLED BY THE USER

      WRITE(LU,*) 'start reading sediment fractions'
      WRITE(LU,*) 'from file: ',SIS_FILES(SISGEO)%NAME    
      DO I=1,NSICLA        
         WRITE(VAR_NAME,'(a4,I1,a11)')'FRAC',I,'           '
         WRITE(LU,*)'variable name:',VAR_NAME
         CALL FIND_VARIABLE('SERAFIN ',SIS_FILES(SISGEO)%LU,
     &                   VAR_NAME,FRAC_C, NPOIN,
     &                   IERR,0d0)
         IF (IERR.eq.0) THEN
            WRITE(LU,*) 'found fraction FRAC',I
            WRITE(LU,*) 'range=',minval(FRAC_C),maxval(FRAC_C)
         ELSE
            WRITE(LU,*) 'fraction NOT found for FRAC',I
         ENDIF
         DO J=1,NPOIN
            NCOUCHES(J) = 1
            AVAIL(J,1,I) = FRAC_C(J)
            DO K=1,NCOUCHES(J)
               ES(J,K)=(ZF%R(J)-ZR%R(J))/NCOUCHES(J)
            ENDDO
         ENDDO
      ENDDO

      ! USER FUNCTION
      CALL USER_INIT_COMPO(NCOUCHES)

      RETURN
      END

