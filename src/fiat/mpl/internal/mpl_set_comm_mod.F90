! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_SET_COMM_MOD

!**** MPL_SET_COMM Set the MPL communicator

!     Purpose.
!     --------
!     Set new communicator

!**   Interface.
!     ----------
!        CALL MPL_SET_COMM(KCOMM)

!        Input required arguments :
!        -------------------------
!           KCOMM    -  New communicator

!        Input optional arguments :
!        -------------------------

!        Output required arguments :
!        -------------------------

!        Output optional arguments :
!        -------------------------

!     Author.
!     -------
!        Sam Hatfield        

!     Modifications.
!     --------------
!        Original: 2022-03-10

!     ------------------------------------------------------------------

! TODO: Establish how this subroutine differs from MPL_SETDFLT_COMM, if at all

USE EC_PARKIND     , ONLY : JPIM
USE MPL_DATA_MODULE, ONLY : MPL_COMM, MPL_NUMPROC, MPL_IDS, MPL_RANK, MPL_COMM_OML
USE MPL_MPIF

IMPLICIT NONE

PUBLIC MPL_SET_COMM

PRIVATE

CONTAINS

SUBROUTINE MPL_SET_COMM(KCOMM)

INTEGER(KIND=JPIM), INTENT(IN) :: KCOMM

INTEGER(KIND=JPIM) :: IERROR, IP, IRANK

! Set default communicator
MPL_COMM = KCOMM

! Calculate size of new communicator
CALL MPI_COMM_SIZE(MPL_COMM, MPL_NUMPROC, IERROR)

! Define MPL_IDS, deallocating it if already defined
IF (ALLOCATED(MPL_IDS)) THEN
    DEALLOCATE(MPL_IDS)
ENDIF
ALLOCATE(MPL_IDS(MPL_NUMPROC))
DO IP = 1, MPL_NUMPROC
  MPL_IDS(IP) = IP
ENDDO

! Calculate this rank's 1-based rank in new communicator
CALL MPI_COMM_RANK(MPL_COMM, IRANK, IERROR)
MPL_RANK = IRANK+1

! Also set the first element of MPL_COMM_OML to new communicator
! TODO: make this less messy
IF (ALLOCATED(MPL_COMM_OML)) THEN
  MPL_COMM_OML(1) = MPL_COMM
ENDIF

END SUBROUTINE MPL_SET_COMM

END MODULE MPL_SET_COMM_MOD