! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

SUBROUTINE GSTATS(KNUM,KSWITCH)

!**** *GSTATS*  - Gather timing statistics

!     PURPOSE.
!     --------
!       To gather timings for subsequent output by routine STATS_OUTPUT 


!**   INTERFACE.
!     ----------
!       *CALL* *GSTATS(KNUM,KSWITCH)

!        EXPLICIT ARGUMENTS
!        --------------------
!        KNUM - timing event number (for list of already defined events
!               see routine STATS_OUTPUT)
!        KSWITCH  - KSWITCH=0 - switch on timer
!                   KSWITCH=1 - switch off timer
!                   KSWITCH=2 - suspend timer
!                   KSWITCH=3 - resume  timer

!        IMPLICIT ARGUMENTS
!        --------------------
!        Module YOMSTATS

!     METHOD.
!     -------

USE EC_PARKIND  ,ONLY : JPIM

USE YOMGSTATS
USE ROCTX, ONLY: ROCTXSTARTRANGE, ROCTXENDRANGE

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN) :: KNUM
INTEGER(KIND=JPIM),INTENT(IN) :: KSWITCH

IF (KSWITCH == 0) THEN
  CALL ROCTXSTARTRANGE(DESCRIPTIONS(KNUM+1))
ELSEIF (KSWITCH == 1) THEN
  CALL ROCTXENDRANGE()
ENDIF

CALL GSTATS_INNER(KNUM, KSWITCH)

END SUBROUTINE GSTATS
    