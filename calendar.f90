PROGRAM calendar
! Get new dates based on user specified 1) number of time steps and 2) time
! step, 3) Start date

IMPLICIT NONE

INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,37)   ! single precision
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12,307) ! double precision
  !
INTEGER, PARAMETER :: wp = sp

INTEGER*8, DIMENSION(12) , PARAMETER :: &
    days_in_month = (/31,28,31,30,31,30,31,31,30,31,30,31/)

INTEGER*8 syear, smonth, sday, dts, tsteps
REAL(wp)  shour
INTEGER*8 cyear, cmonth, cday
REAL(wp)  chour, model_time, mtime

INTEGER i, daysmonth

! User Settings
 dts   = 1800    ![s]
 shour = 0._wp   ![hr]
 sday  = 11    
 smonth = 10
 syear = 2000 
 tsteps = 8003

!
 model_time = tsteps*dts/3600._wp  ![hrs]

 PRINT*, "Start Date      :", syear,smonth,sday
 PRINT*, "Time Steps [-]  :", tsteps
 PRINT*, "Model Time [hr] :", model_time
 PRINT*, "Time Step  [s]  :", dts

!BIG LOOP
 chour  = shour
 cday   = sday
 cmonth = smonth
 cyear  = syear
 do i = 1, tsteps
  mtime = i*dts/3600._wp
  chour = chour + dts/3600._wp
  if (chour.eq.24._wp) then
    chour = 0
    cday = cday + 1
    daysmonth = days_in_month(cmonth)
    !Check for Leap Year
    if (cmonth.eq.2) then
    if (mod(cyear,100)/=0 .AND. mod(cyear,4)==0) then
      daysmonth = days_in_month(cmonth) + 1 
    elseif (mod(cyear,400)==0)then
      daysmonth = days_in_month(cmonth) + 1
    endif
    end if  !Leap Year Check
    !
    if (cday.gt.daysmonth) then
      cday = 1
      cmonth = cmonth + 1
      if (cmonth.gt.12) then
        cmonth = 1
        cyear = cyear + 1
      end if
   end if
  end if
  if (mtime.eq.model_time) then
   PRINT*, "Curr. Date      :", cyear,cmonth, cday, chour
   exit 
  end if
 end do 

END PROGRAM calendar
