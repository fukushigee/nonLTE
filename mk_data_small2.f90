program main
!integer, parameter :: ng = 256, ngz = 128 
integer, parameter :: ng = 128, ngz = 64 
integer, parameter :: ixmax = ng-1, iymax = ng -1, izmax = ngz-1 
integer, parameter :: ixmax2 = ng/2-1, iymax2 = ng/2 -1, izmax2 = ngz/2-1 
integer, parameter :: ixm1 = ng/2+1, iym1 = ng/2+1, izm1 = ngz/2+1 
integer :: i,j,k
real :: ran1
real ::   tem( 0:IXMAX,0:IYMAX,0:IZMAX)  ! temperature
real ::   rho( 0:IXMAX,0:IYMAX,0:IZMAX)  ! density
real ::   tem2( 0:IXMAX2,0:IYMAX2,0:IZMAX2)  ! temperature
real ::   rho2( 0:IXMAX2,0:IYMAX2,0:IZMAX2)  ! density
real, dimension(0:ixmax, 0:iymax, 0:izmax) :: u,v,w 
real*8, dimension(0:ixm1, 0:iym1, 0:izm1) :: vx,vy,vz   
integer, parameter :: idum = -100
real :: xcent, ycent, zcent,rr, rmax,dx0, v0
real,parameter :: pc=3.0857e18
real :: time, vlimit, onesolarpc3, tlimit, tmin, rholimit

      rmax=1*pc
      dx0=rmax/(ng/2)
      v0=1d5/3.0857e18*dx0  ! due to normalization this is necessary


onesolarpc3 = 33.6  ! solar mass/pc^3 --> cm^-3
kms = 1e5 ! km/s --> cm/s

vlimit = 500. 
tlimit = 1000. 
tmin = 20. 
rholimit = 1.e-8


open(1,file='jo0120_small.data', form='unformatted',status='old')
read(1) time 
read(1) u,v,w , rho, tem
close(1)

print*,'time = ', time

print*,'max, min of rho', maxval(rho), minval(rho)
print*,'max, min of tem', maxval(tem), minval(tem)
print*,'max, min of u', maxval(u), minval(v)
print*,'max, min of v', maxval(v), minval(v)
print*,'max, min of w', maxval(w), minval(w)

rho = rho *  onesolarpc3
do k = 0, izmax2-1 
do j = 0, iymax2-1 
do i = 0, ixmax2-1 
  rho2(i, j, k) = &
       ( rho(2*i, 2*j, 2*k) + rho(2*(i+1), 2*(j+1), 2*(k+1)) &
       + rho(2*i, 2*(j+1), 2*(k+1)) + rho(2*(i+1), 2*j, 2*(k+1)) &
       + rho(2*(i+1), 2*(j+1), 2*k)  + rho(2*i, 2*j, 2*(k+1)) &
       + rho(2*i, 2*(j+1), 2*k) + rho(2*(i+1), 2*j, 2*k))/8. 

  tem2(i, j, k) = (tem(2*i, 2*j, 2*k) + tem(2*(i+1), 2*(j+1), 2*(k+1)) &
       +tem(2*i, 2*(j+1), 2*(k+1)) + tem(2*(i+1), 2*j, 2*(k+1)) &
       +tem(2*(i+1), 2*(j+1), 2*k)  + tem(2*i, 2*j, 2*(k+1)) &
       +tem(2*i, 2*(j+1), 2*k) + tem(2*(i+1), 2*j, 2*k))/8. 

 if( tem2(i,j,k) > tlimit)then
      rho2(i,j,k) = rholimit 
      tem2(i,j,k) = tlimit
 endif
 if( tem2(i,j,k) < tmin)then
      tem2(i,j,k) = tmin
 endif
 if( rho2(i,j,k) < rholimit)then
   rho2(i,j,k) = rholimit 
 endif

  vx(i, j, k) = dble((u(2*i, 2*j, 2*k) + u(2*(i+1), 2*(j+1), 2*(k+1)) &
                  +u(2*i, 2*(j+1), 2*(k+1)) + u(2*(i+1), 2*j, 2*(k+1)) &
		  +u(2*(i+1), 2*(j+1), 2*k)  + u(2*i, 2*j, 2*(k+1)) &
		  +u(2*i, 2*(j+1), 2*k) + u(2*(i+1), 2*j, 2*k))/8.) 
  vy(i, j, k) = dble((v(2*i, 2*j, 2*k) + v(2*(i+1), 2*(j+1), 2*(k+1)) &
                  +v(2*i, 2*(j+1), 2*(k+1)) + v(2*(i+1), 2*j, 2*(k+1)) &
		  +v(2*(i+1), 2*(j+1), 2*k)  + v(2*i, 2*j, 2*(k+1)) &
		  +v(2*i, 2*(j+1), 2*k) + v(2*(i+1), 2*j, 2*k))/8.) 
  vz(i, j, k) = dble((w(2*i, 2*j, 2*k) + w(2*(i+1), 2*(j+1), 2*(k+1)) &
                  +w(2*i, 2*(j+1), 2*(k+1)) + w(2*(i+1), 2*j, 2*(k+1)) &
		  +w(2*(i+1), 2*(j+1), 2*k)  + w(2*i, 2*j, 2*(k+1)) &
		  +w(2*i, 2*(j+1), 2*k) + w(2*(i+1), 2*j, 2*k))/8.) 
	
enddo
enddo
enddo

do k = 0, izmax2
do j = 0, iymax2
 rho2(ixmax2, j,k) = rholimit 
 tem2(ixmax2, j,k) = tmin 
enddo
enddo
do k = 0, izmax2
do i = 0, ixmax2
 rho2(i, iymax2,k) = rholimit 
 tem2(i, iymax2,k) = tmin
enddo
enddo
do j = 0, iymax2
do i = 0, ixmax2
 rho2(i, j,izmax2) = rholimit
 tem2(i, j,izmax2) = tmin
enddo
enddo



do k = 0, izm1
do j = 0, iym1
do i = 0, ixm1

 if( vx(i,j,k) > vlimit)then
     vx(i,j,k) = vlimit 
 endif

 if( vy(i,j,k) > vlimit)then
     vy(i,j,k) = vlimit 
 endif 

 if( vz(i,j,k) > vlimit)then
     vz(i,j,k) = vlimit 
 endif 

enddo
enddo
enddo

print*, ''
print*,'max, min of rho2', maxval(rho2), minval(rho2)
print*,'max, min of tem2', maxval(tem2), minval(tem2)
print*,'max, min of vx', maxval(vx), minval(vx)
print*,'max, min of vy', maxval(vy), minval(vy)
print*,'max, min of vz', maxval(vz), minval(vz)

vx = vx * kms 
vy = vy * kms 
vz = vz * kms 

open(1,file='agn_input3.data', form='unformatted',status='unknown')
write(1) ng/2 
write(1) rho2, tem2, (vx),(vy),(vz)
close(1)
print*, ' after output'
print*,'max, min of rho2', maxval(rho2), minval(rho2)
print*,'max, min of tem2', maxval(tem2), minval(tem2)
print*,'max, min of vx', maxval(vx), minval(vx)
print*,'max, min of vy', maxval(vy), minval(vy)
print*,'max, min of vz', maxval(vz), minval(vz)

end program main


      FUNCTION RAN1(IDUM)
      DIMENSION R(97)
        real ran1
      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=1./M1)
      PARAMETER (M2=134456,IA2=8121,IC2=28411,RM2=1./M2)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      DATA IFF/0/
      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN
        IFF=1
        IX1=MOD(IC1-IDUM,M1)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX2=MOD(IX1,M2)
        IX1=MOD(IA1*IX1+IC1,M1)
        IX3=MOD(IX1,M3)
        DO 11 J=1,97
          IX1=MOD(IA1*IX1+IC1,M1)
          IX2=MOD(IA2*IX2+IC2,M2)
          R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
  11    CONTINUE
        IDUM=1
      END IF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      J=1+(97*IX3)/M3
      IF(J.GT.97.OR.J.LT.1)then
         print*,'<ran1> J.GT.97.OR.J.LT.1'
         stop
      ENDIF
      RAN1=R(J)
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
      RETURN 
      END    

