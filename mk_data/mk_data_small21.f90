program main
!integer, parameter :: ng = 512, ngz = 256 
 integer, parameter :: ng = 256, ngz = 256 
!integer, parameter :: ng = 128, ngz = 128 
integer, parameter :: ixmax = ng-1, iymax = ng -1, izmax = ngz-1 
integer, parameter :: ixmax2 = ng/2-1, iymax2 = ng/2 -1, izmax2 = ngz/2-1 
integer, parameter :: ixm1 = ng/2+1, iym1 = ng/2+1, izm1 = ngz/2+1 
integer :: nx, ny, nz
integer :: i,j,k
real :: ran1
real, dimension(0:ixmax, 0:iymax, 0:izmax) :: u,v,w,rho,h2, tem, tem_dust
real, dimension( 0:IXMAX2,0:IYMAX2,0:IZMAX2)  :: rho2, h22, u2,v2,w2, tem2, tem_dust2
real*8, dimension( 0:IXMAX2,0:IYMAX2,0:IZMAX2)  :: vx2, vy2, vz2 
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

vlimit = 1000. 
tlimit = 300 
tmin = 20. 
rholimit = 1.e-8

!!!!!!!! input data !!!!!!!!!!!!!!!!
!open(2,file='/work/wadaki/bf0390_128.data', form='unformatted',status='old')
!open(2,file='/work/wadaki/result/in0199', form='unformatted',status='old')
!open(2,file='bf0270', form='unformatted',status='old')
open(2,file='pma0129', form='unformatted',status='old')
!open(2,file='/work/wadaki/bf0390', form='unformatted',status='old')
   read(2) nx, ny,nz
   read(2) time 
   !read(2) u,v,w , rho, tem, tem_dust, h2
   read(2) u,v,w , rho, tem, h2
close(2)

print*,'nx, ny, nz = ', nx, ny, nz 
print*,'time = ', time

if(nx .ne. ng  .or. nz .ne. ngz)then
 print*,'ERROR:   nx is different from ng', ng, ngz
 stop
endif

print*,'max, min of rho', maxval(rho), minval(rho)
print*,'max, min of tem', maxval(tem), minval(tem)
print*,'max, min of h2 ', maxval(h2), minval(h2)
print*,'max, min of u', maxval(u), minval(v)
print*,'max, min of v', maxval(v), minval(v)
print*,'max, min of w', maxval(w), minval(w)


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

  h22(i, j, k) = (h2(2*i, 2*j, 2*k) + h2(2*(i+1), 2*(j+1), 2*(k+1)) &
       +h2(2*i, 2*(j+1), 2*(k+1)) + h2(2*(i+1), 2*j, 2*(k+1)) &
       +h2(2*(i+1), 2*(j+1), 2*k)  + h2(2*i, 2*j, 2*(k+1)) &
       +h2(2*i, 2*(j+1), 2*k) + h2(2*(i+1), 2*j, 2*k))/8. 

  u2(i, j, k) = (u(2*i, 2*j, 2*k) + u(2*(i+1), 2*(j+1), 2*(k+1)) &
       +u(2*i, 2*(j+1), 2*(k+1)) + u(2*(i+1), 2*j, 2*(k+1)) &
       +u(2*(i+1), 2*(j+1), 2*k)  + u(2*i, 2*j, 2*(k+1)) &
       +u(2*i, 2*(j+1), 2*k) + u(2*(i+1), 2*j, 2*k))/8. 

  v2(i, j, k) = (v(2*i, 2*j, 2*k) + v(2*(i+1), 2*(j+1), 2*(k+1)) &
       +v(2*i, 2*(j+1), 2*(k+1)) + v(2*(i+1), 2*j, 2*(k+1)) &
       +v(2*(i+1), 2*(j+1), 2*k)  + v(2*i, 2*j, 2*(k+1)) &
       +v(2*i, 2*(j+1), 2*k) + v(2*(i+1), 2*j, 2*k))/8. 

  w2(i, j, k) = (w(2*i, 2*j, 2*k) + w(2*(i+1), 2*(j+1), 2*(k+1)) &
       +w(2*i, 2*(j+1), 2*(k+1)) + w(2*(i+1), 2*j, 2*(k+1)) &
       +w(2*(i+1), 2*(j+1), 2*k)  + w(2*i, 2*j, 2*(k+1)) &
       +w(2*i, 2*(j+1), 2*k) + w(2*(i+1), 2*j, 2*k))/8. 

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
 if( vx(i,j,k) < -vlimit)then
     vx(i,j,k) = -vlimit 
 endif

 if( vy(i,j,k) > vlimit)then
     vy(i,j,k) = vlimit 
 endif 
 if( vy(i,j,k) < -vlimit)then
     vy(i,j,k) = -vlimit 
 endif 

 if( vz(i,j,k) > vlimit)then
     vz(i,j,k) = vlimit 
 endif 
 if( vz(i,j,k) < -vlimit)then
     vz(i,j,k) = -vlimit 
 endif 

enddo
enddo
enddo

print*, ''
print*,'max, min of rho2', maxval(rho2), minval(rho2)
print*,'max, min of tem2', maxval(tem2), minval(tem2)
!print*,'max, min of tem_dust2', maxval(tem_dust2), minval(tem_dust2)
print*,'max, min of h22', maxval(h22), minval(h22)
print*,'max, min of vx', maxval(vx), minval(vx)
print*,'max, min of vy', maxval(vy), minval(vy)
print*,'max, min of vz', maxval(vz), minval(vz)

!--- for farther reducing data size ---------
!--- output -------------------
!open(1,file='/work/wadaki/bf0390_256.data', form='unformatted',status='unknown')
!open(1,file='bf0270_256.data', form='unformatted',status='unknown')
 open(1,file='pma0129_128.data', form='unformatted',status='unknown')
!open(1,file='/work/wadaki/bf0390_64.data', form='unformatted',status='unknown')
write(1) nx/2, ny/2,nz/2
write(1) time 
!write(1) u2,v2,w2, rho2, tem2, tem_dust2, h22
write(1) u2,v2,w2, rho2, tem2
!write(1) u2,v2,w2, rho2, tem2, h22
close(1)


!--- for nonLTE code ---------
vx2 = u2 * kms 
vy2 = v2 * kms 
vz2 = w2 * kms 
rho2 = rho2 *  onesolarpc3 
h22 = h22 *  onesolarpc3

print*,'shape, size of rho,tem, vx,...'
print*, shape(rho2), shape(tem2), shape(vx2)
print*, size(rho2), size(tem2), size(vx2)

 open(1,file='pma0129_nonlte128.data', form='unformatted',status='unknown')
 write(1) ng/2 
 write(1) rho2, tem2, vx2, vy2, vz2 
 close(1)
 print*, ' after output (unit of velocity: cm/s'
 print*, 'ng/2', ng/2
 print*,'max, min of rho2', maxval(rho2), minval(rho2)
 print*,'max, min of tem2', maxval(tem2), minval(tem2)
!print*,'max, min of tem_dust2', maxval(tem_dust2), minval(tem_dust2)
!print*,'max, min of h22', maxval(h22), minval(h22)
 print*,'max, min of u2', maxval(vx2), minval(vx2)
 print*,'max, min of v2', maxval(vy2), minval(vy2)
 print*,'max, min of w2', maxval(vz2), minval(vz2)

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

