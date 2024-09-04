PROGRAM solarzenithangle
! Based on NOAA solar calculation, this program computes solar zenith angle
! Inputs: year, month, day, hour, time shift, lat, lon
! Author: Prabhakar Shrestha

IMPLICIT NONE

INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,37)   ! single precision
INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12,307) ! double precision
  !
INTEGER, PARAMETER :: wp = sp 

REAL(wp) pi,pirad
INTEGER*8  year, month, day
REAL(wp)  lat, lon, hour, tz
INTEGER*8 juld
REAL(dp)  gregjuld
REAL(dp)  G2, I2, J2, K2, L2, M2, N2, O2, P2, Q2, R2, S2, T2, U2, V2
REAL(dp)  AB_val, AB2, AC2
REAL(wp)  szen

!User Inputs
!------------------------------------------------------------------------
 year = 2016
 month = 10 
 day = 11 
 hour = 0._wp 
 tz = -7._wp
 lat = 39.1035805_wp
 lon = -107.883820_wp

!------------------------------------------------------------------------

 pi = 4._wp*atan(1._wp)
 pirad = pi/180._wp

 !Julian Day
 juld  = day - 32075 + 1461*(year + 4800 + (month - 14)/12)/4 + &
             367*(month - 2 - ((month - 14)/12)*12)/12 -       &
             3*((year + 4900 + (month - 14)/12)/100)/4
 gregjuld = DBLE(juld) + DBLE(hour-12)/24._dp - DBLE(tz)

 !Julian Century
 G2  = (gregjuld-2451545._dp)/36525._dp

 !Geom. Mean Lon Sun [degrees]
 I2  = mod(280.46646_dp+G2*(36000.76983_dp + G2*0.0003032_dp),360._dp)

 !Geom. Mean Anom. Sun
 J2  = 357.52911_dp+G2*(35999.05029_dp - 0.0001537_dp*G2)

 !Eccent. Earth Orbit
 K2  = 0.016708634_dp-G2*(0.000042037_dp+0.0000001267_dp*G2)

 !Sun Eq. of Ctr
 L2 = sin(pirad*J2)*(1.914602_dp-G2*(0.004817_dp+0.000014_dp*G2))+ &
      sin(pirad*2._dp*J2)*(0.019993_dp-0.000101_dp*G2)+sin(pirad*3._dp*J2)*0.000289_dp

 !Sun True Long [deg]
 M2  = I2 + L2

 !Sun True Anom [deg]
 N2 = J2 + L2

 !Sun Rad Vector [AUs]
 O2 = (1.000001018_dp*(1._dp-K2*K2))/(1._dp+K2*cos(pirad*N2))
 
 !Sun App Long [deg]
 P2 = M2-0.00569_dp-0.00478_dp*sin(pirad*(125.04_dp-1934.136_dp*G2))

 !Mean Obliq Ecliptic [deg]
 Q2 = 23._dp+(26._dp+((21.448_dp-G2*(46.815_dp+G2*(0.00059_dp-G2*0.001813_dp))))/60._dp)/60._dp

 !Obliq Corr. [deg]
 R2 = Q2+0.00256_dp*cos(pirad*(125.04_dp-1934.136_dp*G2))

 !Sun Rt Ascen [deg]
 S2 = (atan2(cos(pirad*R2)*sin(pirad*P2),cos(pirad*P2)))/pirad

 !Sun Decl [deg]
 T2 = (asin(sin(pirad*R2)*sin(pirad*P2)))/pirad

 !var y
 U2 =tan(pirad*R2/2._dp)*tan(pirad*R2/2._dp)

 !Eq. of time [mins]
 V2 = 4._dp*(U2*sin(2._dp*pirad*I2)-2._dp*K2*sin(pirad*J2)+4._dp*K2*U2*sin(pirad*J2)*cos(2._dp*pirad*I2) &
       -0.5_dp*U2*U2*sin(4._dp*pirad*I2)-1.25_dp*K2*K2*sin(2._dp*pirad*J2))/pirad

 !True Solar Time [min]
 AB_val = (hour/24._dp)*1440._dp+V2+4._dp*lon-60._dp*tz

 if (AB_val .lt. 0) then
    AB2 = AB_val + 1440._dp
 else
    AB2 = mod(AB_val,1440._dp)
 end if

 !Hour angle
 if (AB2/4._dp .lt. 0) then
    AC2 = AB2/4._dp + 180._dp
 else
    AC2 = AB2/4._dp - 180._dp
 end if

 !Solar Zenith angle
 szen = (acos(sin(pirad*lat)*sin(pirad*T2) +    &
          cos(pirad*lat)*cos(pirad*T2)*cos(pirad*AC2))/pirad)

 PRINT*, lat,lon,P2,R2, T2, AB_val, AB2, AC2, szen 

END PROGRAM solarzenithangle
