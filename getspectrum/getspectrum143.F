! this is non-LTE calculation
! ver. 1.4   nz is different from x or ny 
!      1.4.2  binary data output nlev.b
!      1.4.2.2.  for octa (7/7/2005)
!      1.4.2.3   for peteragi (5/5/2010)  file: intensity -> data3
!  for an (CfCA)   2015.4.5
!      1.4.3  module, implicit none, bug fixed in
!      find_boundary_from_outside  KW 10/23/2016  
!
!#ifdef VPP
!#define NG  64 
!#define NGZ 32 
!#define NRAY 100 
!#define LEVMAX 10 
!#define VIEWANGLE 45
!#define LEVD '010nlev.d'
!#define SPECT 'sp'
!#define LEVD 'data1/030nlev.d'
!#define SPECT 'data1/sp'
!#define LEVD 'data2/001nlev.d'
!#define SPECT 'data2/sp'
!#endif
#ifdef ALPHA
#define NG 128
#define NGZ 128 
#define NRAY 20
#define LEVMAX 5 
#define VIEWANGLE 45 
#define NCHANNEL 50
#define DELTAV 5.0d5
! #define LEVD '010nlev.d'
!#define LEVD 'pma0129_data2/010nlev.d'
!#define SPECT 'pma0129_data2/sp'
!#define INTENSITY 'pma0129_data2/intensity'
#define LEVD '005nlev.d'
!#define LEVD '006nlev_hcn.d'
#define SPECT 'sp'
#define INTENSITY 'intensity'
#endif

      module grid_size 
      integer,parameter :: ng=NG, ngz = NGZ, ixm=ng,iym=ng,izm=ngz
      integer,parameter :: levmax=LEVMAX
      integer,parameter:: mvelmax=NCHANNEL  !number of levels

        integer,parameter :: Nray0=1000,
     *          nsp=ng, ixm1=ixm+1,iym1=iym+1,izm1=izm+1,
     *          ixm2=ixm+2,iym2=iym+2,izm2=izm+2


      real*8 :: xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0

      real :: TBIvel(1:ng, 1:ng, 1:levmax)
      real :: TBcube(1:ng, 1:ng, 1:mvelmax, 1:levmax)
      real :: radial_velocity(1:mvelmax)
      real*8 :: cT2I(levmax)
      real*8 BG(1:levmax)
      real*8 :: alpha(levmax,ixm,iym,izm),S(levmax,ixm,iym,izm)


      real*8, parameter :: dv=DELTAV   ! 2km/s spectral resolution !debug
      real*8 :: sigma
      real*8 vx(0:ixm1,0:iym1,0:izm1),vy(0:ixm1,0:iym1,0:izm1),
     *       vz(0:ixm1,0:iym1,0:izm1)
      real*8 rad_v(mvelmax), rad_vc(mvelmax)
      real*8 :: nx, ny, nz
!     common /spectrum/ vx,vy,vz,sigma,rad_v,rad_vc

!     common /density/ n
      real*8 ::  n(0:levmax,ixm,iym,izm)

      real*8 ::      xg1(nsp), yg1(nsp), zg1(nsp),
     *               xg2(nsp), yg2(nsp), zg2(nsp) 

      end module grid_size 

      module phys_const
                real*8, parameter :: H    = 6.626184D-27 !Plank Constant [erg s]
                real*8, parameter :: lc   =2.99792458d10    !Speed of Light [cm/s] 
           real*8, parameter :: bc=1.380658d-16     !Boltzmann's Constant [erg/K]
           real*8, parameter :: bb=57.6356D9  ! CO Rotation Constant (Robinson 1974) in GHz
           real*8, parameter :: mu=0.112D-18  ! CO electric dipole moment 
           real*8, parameter :: pi= 3.141592

!     dv=2d3   ! 20m/s spectral resolution
       end module phys_const


      program main
!
!    1.0
!
!     implicit real*8 (a-h,o-z) 
      use grid_size 
      use phys_const
      implicit none
!     integer,parameter :: ng=NG, ngz = NGZ, ixm=ng,iym=ng,izm=ngz
!     integer,parameter :: levmax=LEVMAX

!      include 'mpif.h'
!      common /M_param/ Nprocs, Myrank, ista, iend

!      call mpi_init(ierr)
!      call mpi_comm_size(mpi_comm_world, Nprocs, ierr)
!      call mpi_comm_rank(mpi_comm_world, Myrank, ierr)

!      call para_range(1,izm,Nprocs,Myrank,ista,iend)

      call initia
      call integ

!      call mpi_finalize(ierr)
      end program main



!
!     initial setup
!
      subroutine initia
      use grid_size 
      use phys_const
!     implicit real*8(a-h,o-z)
      implicit none
!     integer,parameter :: ng=NG
!     integer,parameter :: ngz=NGZ
!      integer,parameter :: Nray0=1000,
!     *          ixm=ng,iym=ng,izm=ngz,nsp=ng,
!     *          ixm1=ixm+1,iym1=iym+1,izm1=izm+1,
!     *          ixm2=ixm+2,iym2=iym+2,izm2=izm+2 
!      real*8,parameter :: pi=3.1415926535d0, pc=3.0857d18 
        integer :: i,j,k, i2
        integer :: ng2
        real*8 :: average_s, th, phi
        real*8 :: x0, y0, z0, dx1, dy1, dz1, dx2, dy2, dz2
        integer :: mvel, lev
        integer ::  ngw,levmaxw,Nrayw

!     common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0

!     real*8 nx,ny,nz
!     common /grid2/ xg1(nsp), yg1(nsp), zg1(nsp),
!    *               xg2(nsp), yg2(nsp), zg2(nsp),
!    *               nx, ny, nz

!     common /spectrum/ vx,vy,vz,sigma,rad_v,rad_vc
!     real*8 vx(0:ixm1,0:iym1,0:izm1),vy(0:ixm1,0:iym1,0:izm1),
!    *       vz(0:ixm1,0:iym1,0:izm1)
!     real*8 rad_v(mvelmax), rad_vc(mvelmax)

!     common /density/ n
!     real*8  n(0:levmax,ixm,iym,izm)

!     Alpha and S
!     real*8 alpha(levmax,ixm,iym,izm),S(levmax,ixm,iym,izm)
!     common /alphas/ alpha,S


!     common /CBR/BG(1:levmax)
!     real*8 BG

        real  random

      integer,parameter:: final=1  ! read(1)
      character(40) :: input_levd


      integer, parameter :: Nray=NRAY


      print*,'velocity resolution dv: ', dv/1.e5, 'km/s'
!      dv=5d3   ! 50m/s spectral resolution
      do mvel=1,mvelmax
        rad_v(mvel)=dv*(mvel-(mvelmax+1)/2)  !velocity
        rad_vc(mvel)=rad_v(mvel)/lc
      end do

      do lev=1,levmax
        cT2I(lev)=2d0*(2*bb*lev)**2*bc/lc**2       ! 2*bb*J=nu(lev-->lev-1)
                                      ! E(J): energy level, e(0)=0 
      end do

!      You have to change the file name to write down the result.
        input_levd = LEVD
        print*,'INPUT file:', input_levd 

      open(final,file=input_levd,
     *     status='unknown',
     *     form='unformatted')

      read(final) ngw,levmaxw,Nrayw
      ! confirm grid number here.
      write(*,'(''NG, read='',2i4)') ng,ngw
      write(*,'(''levmax, read='',2i4)') levmax,levmaxw
      write(*,'(''Nray, read='',2i4)') Nray,Nrayw
        if (ngw .ne. ng)then
                print*,'ERROR  NG in the data file is different!'
                stop
        endif

      read(final) xf,yf,zf,dx0
      read(final) vx,vy,vz,sigma
           print*,'####### debug ########'
!          print*,'####### vx=vy=vz =0.  ########'
!          print*,'####### sigma * 0.001  ########'
!       vx = 0.
!       vy = 0.
!       vz = 0.
!     sigma = 0.001 * sigma
      read(final) n
!       print*,n(0,10,10,10)
!       print*,n(1,10,10,10)
!       print*,n(2,10,10,10)
!       print*,n(3,10,10,10)
!       print*,n(4,10,10,10)
!       print*,n(5,10,10,10)
      read(final) alpha,S
           print*,'####### debug ########'
           print*, 'max,min alpha',maxval(alpha), minval(alpha)
           print*,alpha(1,10,10,10)
           print*,alpha(2,10,10,10)
           print*,alpha(3,10,10,10)
           print*, 'max,min S',maxval(S), minval(S)
!          print*,'####### alpha = alpha*1e3  ########'
!          alpha = alpha* 1.
!          print*,'####### S + noise  ########'
           average_s = 0.
           do k = 1, izm
           do j = 1, iym
           do i = 1, ixm
           do i2 = 1, levmax 
             !random = ran1(-10)
             !S(i2,i,j,k) = S(i2,i,j,k) * (1.+ (2.*random-1.)*1.e-2)
              average_s = average_s + S(i2,i,j,k) 
           enddo
           enddo
           enddo
           enddo
           print*,'average_s = ', average_s/(ixm*iym*izm*levmax) 
!          do k = 1, izm
!          do j = 1, iym
!          do i = 1, ixm
!          do i2 = 1, levmax 
!          random = ran1(-10)
!          ! print*, 'random = ', random
!               S(i2,i,j,k) = average_s/(ixm*iym*izm*levmax) *
!     &             (1.+ (2.*random-1.)*5.e-1)
!          enddo
!          enddo
!          enddo
!          enddo
!          print*,'####### S + noise,end  ########'
            
!     from version 6.1
!     ver=6.1
!     if (ver >= 6.1) then
!       write(*,*) 'version >= 6.1, read BG.'
        read(final) BG
!       print*,'BG, BG(l)/cT2I'
!       write(*,*) BG,(BG(l)/cT2I(l),l=1,levmax)
!        BG(1)=3.341002122390623E-015 
!        BG(2)=3.051191868962448E-015
!        BG(3)=1.307816713074869E-015
!        BG(4)=3.987479826156674E-016
!        BG(5)=1.003391074232656E-016

!     end if
!      write(*,*) xf
!     make grid
!       set direction
!     th=pi/2d0!  +1d-7
      th= VIEWANGLE/180.*pi
!     th=0d0!  +1d-7  ! pole-on
      phi=0.00d0*pi   !  +1d-7

        print*,'th, phi: ', th/3.14159,'pi', phi/3.141592,'pi'

      nx=sin(th)*cos(phi)
      ny=sin(th)*sin(phi)
      nz=cos(th)

        print*,'nx, ny, nz =', nx, ny, nz

!      initial point
      x0=0d0
      y0=0d0
      z0=0d0
      !1-axis (nx*x+nz*z=0)
      dx1=nz/sqrt(nx**2+nz**2)
      dy1=0d0
      dz1=-nx/sqrt(nx**2+nz**2)
      !2-axis (nx,ny,nz)x(dx,0,dz)=(2)
      dx2=ny*dz1
      dy2=nz*dx1-nx*dz1
      dz2=-ny*dx1
!      write(*,*)dx1,dy1,dz1,dx2,dy2,dz2
      do i=1,nsp
        xg1(i)=dx1*(i-(1+nsp)/2d0)
        yg1(i)=dy1*(i-(1+nsp)/2d0)
        zg1(i)=dz1*(i-(1+nsp)/2d0)
        xg2(i)=dx2*(i-(1+nsp)/2d0)
        yg2(i)=dy2*(i-(1+nsp)/2d0)
        zg2(i)=dz2*(i-(1+nsp)/2d0)
      end do
!     go to infinity
      do i=1,nsp
        xg1(i)=x0+xg1(i)-nx*ng
        yg1(i)=y0+yg1(i)-ny*ng
        zg1(i)=z0+zg1(i)-nz*ng
        xg2(i)=x0+xg2(i)-nx*ng
        yg2(i)=y0+yg2(i)-ny*ng
        zg2(i)=z0+zg2(i)-nz*ng
      end do
      end




      subroutine integ
      use grid_size 
      use phys_const
!     implicit real*8(a-h,o-z)
      implicit none
!     integer,parameter :: ng=NG
!     integer,parameter :: ngz=NGZ 
!     integer,parameter :: ixm=ng,iym=ng,izm=ngz,nsp=ng,
!    *          ixm1=ixm+1,iym1=iym+1,izm1=izm+1,
!    *          ixm2=ixm+2,iym2=iym+2,izm2=izm+2 
!     real*8 nx,ny,nz
!     common /grid2/ xg1(nsp), yg1(nsp), zg1(nsp),
!    *               xg2(nsp), yg2(nsp), zg2(nsp),
!    *               nx, ny, nz
      real*8  Intensity(1:mvelmax,1:levmax)  ! lev=1 1->0
      integer ::i,j,k, i1, i2, ireturn1, ireturn2, lev
      real*8  ::x1,y1,z1  ! starting point
      real*8  ::x2,y2,z2  ! end point
      real*8  :: zinit, yinit, xinit
      real :: tmp_tbi(1:nsp, 1:nsp)
      character(40) :: outspect
      character*2 c

      TBcube = 0.
      radial_velocity = 0.

      ! observation maxtrix
      do i1=1,nsp
      do i2=1,nsp
!       write(*,*) '<integ> i1, i2 ',i1,i2
!      do i1=5,6
!      do i2=5,6
        ! 1=(xg1(i1),yg1(i1),zg1(i1))
        ! 2=(xg2(i2),yg2(i2),zg2(i2))
        zinit=zg1(i1)+zg2(i2)
        yinit=yg1(i1)+yg2(i2)
        xinit=xg1(i1)+xg2(i2)
!        write(*,*) i1,i2,xinit,yinit,zinit
        call find_bound_from_outside(xinit,yinit,zinit, x1,y1,z1,ireturn1)
!        write(*,*) x1,y1,z1
        call find_bound_from_inside(x1,y1,z1, x2,y2,z2,ireturn2)
!        write(*,*) x2,y2,z2
        if (ireturn1+ireturn2 == 0) then
          call solve_I(x1,y1,z1,x2,y2,z2,Intensity,i1,i2)
        end if

         call output(Intensity,i1,i2)
      end do
      end do

!     do lev = 1, levmax 
!         tmp_tbi = 0.
!      do i = 1, nsp
!      do j = 1, nsp
!          tmp_tbi(i,j) = TBIvel(i,j,lev)
!      enddo
!     enddo
        outspect =  SPECT//'nlev.b'
        print*,'output file: ', outspect
          open(11,
     *     file=outspect,
     *     status='unknown',form='unformatted')
                write(11) nsp, levmax
                write(11)  TBIvel 
          close(11)
!     enddo
          print*,'max,min TBIvel', maxval(TBIvel), minval(TBIvel)

!output 3D data cube (KW)
          print*,'max,min radial velocity', maxval(radial_velocity),
     *                             minval(radial_velocity)
          print*,'max,min TBcube', maxval(TBcube), minval(TBcube)

        do lev=1, 5
            write(c,'(i2.2)')lev
            open(12+lev, file=SPECT//c//'.cube',
     *                 status='unknown',form='unformatted')
                write(12+lev) nsp, mvelmax 
! Ngrid, num.of velocity channels 
                write(12+lev) radial_velocity,TBcube(:,:,:,lev) 
            close(12+lev)
        enddo

      end subroutine integ
        
      subroutine output(Intensity,i1,i2)
      use grid_size 
      use phys_const
!     implicit real*8(a-h,o-z)
      implicit none
      integer :: lev, i1, i2, mvel
!     integer,parameter::levmax=LEVMAX
!     integer, parameter ::  ng=NG 
!     integer, parameter ::  ngz=NGZ 
!     integer, parameter ixm=ng,iym=ng,izm=ngz,
!    *          ixm1=ixm+1,iym1=iym+1,izm1=izm+1,
!    *          ixm2=ixm+2,iym2=iym+2,izm2=izm+2 

!     common /spectrum/ vx,vy,vz,sigma,dv,rad_v,rad_vc
!     real*8 vx(0:ixm1,0:iym1,0:izm1),vy(0:ixm1,0:iym1,0:izm1),
!    *       vz(0:ixm1,0:iym1,0:izm1)
!     real*8 rad_v(mvelmax), rad_vc(mvelmax)

      real*8 ::  Intensity(1:mvelmax,1:levmax)  ! lev=1 1->0
      real*8 :: TBI
      integer :: initia=1
      character*2 c
      real*8  TB(1:mvelmax)

      if (initia==1) then
        do lev=1,levmax

!         open file
          write(c,'(i2.2)')lev
          open(10+lev,
     &     file=SPECT//c//'.d',
     &     status='unknown',form='formatted')

!         write radial velocity
          write (10+lev,2000) 0,0,0e0,
     *          (rad_v(mvel)/1d5,mvel=1,mvelmax)



        end do
    
        initia=0
      end if
!

!     print*,'<output> max, min of rad_v', maxval(rad_v), minval(rad_v)

      do lev=1,levmax  !levels

        TBI=0d0
        do mvel=1,mvelmax  !velocity
          TB(mvel)=Intensity(mvel,lev)/cT2I(lev)  ! I=(2 nu^2/c^2) kT
          TBI=TBI+TB(mvel)*dv/1d5                 ! TBI=\int TB dv
          TBcube(i1,i2, mvel, lev) = TB(mvel)    ! 3D data cube (i, j, v)
          radial_velocity(mvel) = rad_v(mvel)/1.D5
        end do
        TBIvel(i1,i2,lev) = TBI

        write (10+lev,2000) i1,i2,TBI,(TB(mvel),mvel=1,mvelmax)
!     *        (Intensity(mvel,lev),mvel=1,mvelmax)

! output intensity at a channel to binary file, upto J=5 (KW)

      end do

 2000 format(i3,2x,i3,1pe13.3/0p100f13.5)
      end subroutine


!
!     integrate dI/ds=-alpha I+S 
!
      subroutine solve_I(x1,y1,z1,x2,y2,z2,Intensity,i1,i2)
      use grid_size 
      use phys_const
!     implicit real*8(a-h,o-z)
      implicit none
      integer :: lev, mvel, i,j,k,l, i1, i2
!     integer,parameter ::  ng=NG 
!     integer,parameter ::  ngz=NGZ 
!     integer,parameter :: ixm=ng,iym=ng,izm=ngz,
!    *          ixm1=ixm+1,iym1=iym+1,izm1=izm+1,
!    *          ixm2=ixm+2,iym2=iym+2,izm2=izm+2 

!    real*8,parameter :: pi=3.1415926535d0 
!     real*8  nx,ny,nz
      real*8  vn 
      integer,parameter:: iranmax = 1
      real*8  Intensity(1:mvelmax,1:levmax)  ! lev=1 1->0
      real*8  Intensity_old(1:mvelmax, 1:levmax)  ! for averaging 

!     real*8 alpha(levmax,ixm,iym,izm),S(levmax,ixm,iym,izm)
!     common /alphas/ alpha,S

!     common /spectrum/ vx,vy,vz,sigma,dv,rad_v,rad_vc
!     real*8 vx(0:ixm1,0:iym1,0:izm1),vy(0:ixm1,0:iym1,0:izm1),
!    *       vz(0:ixm1,0:iym1,0:izm1)
!     real*8  rad_v(mvelmax),rad_vc(mvelmax)

!     common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0

      real*8, parameter::largest=1d50
      integer,parameter:: Lmax=2*ng
      real*8,parameter::  eps=1d-3
      real  random
      integer :: isigz, isigy, isigx, ix, iy, iz, iran
      integer :: ixmin, iymin, izmin
      integer :: ix0, iy0, iz0
      real*8 :: x,y,z,x1,y1,z1, x2, y2, z2
      real*8 :: tx,ty,tz, dl, tau0, S0, phi, tau
      real*8 :: ran1

!      common /CBR/BG(0:levmax)
!     common /CBR/BG(1:levmax)
!     real*8 BG
        character*2 c
      character(40) :: output_intensity
      output_intensity = INTENSITY 

!      Intensity(1:mvelmax,1:levmax)=0d0
      do lev=1,levmax
        do mvel=1,mvelmax
!debug
           Intensity(mvel,lev)=BG(lev)
        end do
      end do

!      do mvel=1,mvelmax
!        rad_vc(mvel)=dv*(mvel-(mvelmax+1)/2)/lc
!      end do

      isigx=int(nx+1d0)
      ix=isigx*int(x1-xf(0))
     *   +(1-isigx)*(ixm-int(xf(ixm1)-x1))
 
      isigy=int(ny+1d0)
      iy=isigy*int(y1-yf(0))
     *   +(1-isigy)*(iym-int(yf(iym1)-y1))

      isigz=int(nz+1d0)
      iz=isigz*int(z1-zf(0))
     *   +(1-isigz)*(izm-int(zf(izm1)-z1))

      x=x1
      y=y1
      z=z1

      do L=1,Lmax


!     check whether the present position = (x2,y2,z2) ?
        if((x-x2)**2+(y-y2)**2+(z-z2)**2 < eps**2) then
          goto 1000
        end if

!       Next crossing

        isigx=int(nx+1d0)
        if (nx==0d0) then
          tx=largest
        else
          tx=(xf(ix+isigx)-x)/nx
        end if

        isigy=int(ny+1d0)
        if (ny==0d0) then
          ty=largest
        else
          ty=(yf(iy+isigy)-y)/ny
        end if

        isigz=int(nz+1d0)
        if (nz==0d0) then
          tz=largest
        else
          tz=(zf(iz+isigz)-z)/nz
        end if

        if(tx <  0d0.or.ty < 0d0.or.tx < 0d0) then
          write(*,*) 'XXX',tx,ty,tz,x,y,z,x2,y2,z2
          stop
        end if

        dl=min(tx,ty,tz)
        ixmin=int(dl/tx)
        iymin=int(dl/ty)
        izmin=int(dl/tz)

        x=ixmin*xf(ix+isigx)+(iymin+izmin)*(x+nx*dl)
        y=iymin*yf(iy+isigy)+(ixmin+izmin)*(y+ny*dl)
        z=izmin*zf(iz+isigz)+(ixmin+iymin)*(z+nz*dl)
        ix0=ixmin*(ix+2*isigx-1)+(iymin+izmin)*ix
        iy0=iymin*(iy+2*isigy-1)+(izmin+ixmin)*iy
        iz0=izmin*(iz+2*isigz-1)+(ixmin+iymin)*iz


        vn=(vx(ix,iy,iz)*nx+vy(ix,iy,iz)*ny+vz(ix,iy,iz)*nz)/lc
        !print*,'vn = ',vn
        Intensity_old = 0.d0

        do iran = 1, iranmax
        random = ran1(-iran)
        do lev=1,levmax

          tau0=alpha(lev,ix,iy,iz)*(dl*dx0)
          S0=S(lev,ix,iy,iz)
          do mvel=1,mvelmax
            phi=exp(-(rad_vc(mvel)-vn)**2/2/sigma**2)
     *            /sqrt(2d0*pi)/sigma
            tau=tau0*phi 
!           tau=tau0*phi * (1.+ (2.*random-1.)*1.e-2)
!                                                 ! alpha==alpha/nu0
!           Intensity(mvel,lev)=Intensity(mvel,lev)*exp(-tau)
!    *                       +S0*(1d0-exp(-tau))
            Intensity(mvel,lev)=Intensity(mvel,lev)*exp(-tau) 
     &         +S0*(1d0-exp(-tau))
!debug
!           if(lev.eq.2 .and. mvel.eq.65 .and.
!    &           i1.eq.ng/2  )then
!     write(c,'(i2.2)') l
!     open(20+l,file=trim(output_intensity)//c//'.data',
!    *     status='unknown',
!    *     form='formatted')

!          write(20+l,'(3i3,2x,i3,3e13.5)') l,i1,i2,mvel, exp(-tau), S0,
!    &             Intensity(mvel,lev)

!              write(*,'(3i3,2x,i3,3e12.5)') l,i1,i2,mvel, exp(-tau), S0,
!     &             Intensity(mvel,lev)
!            endif
!debug
!           Intensity(mvel,lev)=Intensity(mvel,lev)* 1./(tau+1)
!    &         +S0*(1d0- 1./(tau+1))

!       ---- sum
!            Intensity_old(mvel,lev)= 
!     &        Intensity(mvel,lev) + Intensity_old(mvel,lev)

          enddo
          
        end do
        enddo

!         do lev=1,levmax
!           do mvel=1,mvelmax
!           Intensity(mvel,lev)= Intensity_old(mvel,lev) / iranmax 
!           enddo
!         enddo

        ix=ix0
        iy=iy0
        iz=iz0

      enddo !Lmax
 1000 continue
      end subroutine

      subroutine find_bound_from_outside(x0,y0,z0, x1,y1,z1,ireturn)
!     implicit real*8(a-h,o-z)
      use grid_size 
      use phys_const
      implicit none
!     integer,parameter :: ng=NG
!     integer,parameter :: ngz=NGZ
!     integer,parameter :: ixm=ng,iym=ng,izm=ngz,
!    *          ixm1=ixm+1,iym1=iym+1,izm1=izm+1,
!    *          ixm2=ixm1+1,iym2=iym1+1,izm2=izm1+1
      real*8 ::  x, y, z, tx, ty, tz, x0, y0, z0, dl
      integer :: isigx,ix,isigy,iy,isigz,iz
      integer :: ireturn
      integer :: ixmin, iymin, izmin
      real*8, parameter::largest=1d50
      real*8  ::x1,y1,z1  ! starting point
      real*8  ::x2,y2,z2  ! end point
     
!     common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0

!     find the crossing point through the boundary

      ! nx must be -1<nx<1
      ! nx must be -1<nx<1
      if (nx== 1d0) nx=nx-1d-10
      if (nx==-1d0) nx=nx+1d-10
      if (ny== 1d0) ny=ny-1d-10
      if (ny==-1d0) ny=ny+1d-10
      if (nz== 1d0) nz=nz-1d-10
      if (nz==-1d0) nz=nz+1d-10

!-----KW: 2016.10.23 ---------------
      isigx=int(nx+1d0)
      ix=isigx*1+(1-isigx)*ixm  !  0<nx: ix=1   nx<0: ix=ixm

      isigy=int(ny+1d0)
      iy=isigy*1+(1-isigy)*iym  !  0<ny: iy=1   ny<0: iy=iym              

      isigz=int(nz+1d0)
      iz=isigz*1+(1-isigz)*izm  !  0<nz: iz=1   nz<0: iz=izm              
!-----KW: 2016.10.23 ---------------

!     isigx=int(nx+1d0)
      if (x0 < xf(1) .or. x0 > xf(ixm1)) then
        ix=isigx*1+(1-isigx)*ixm  !  0<nx: ix=1   nx<0: ix=ixm
  
        if (nx==0d0) then
          tx=largest
        else
          tx=(xf(ix+1-isigx)-x0)/nx ! nx > 0: distance to xf(ixm+1) 
                                ! nx < 0:             xf(1)
        end if
        y=y0+ny*tx                ! if this y and z are outside
        z=z0+nz*tx                ! the boundary, this tx is not real.
        if (y < yf(1) .or. y > yf(iym1) .or. 
     *      z < zf(1) .or. z > zf(izm1)) then
          tx=largest
        end if
      else 
        tx=largest       ! xf(1) < x < xf(ixm1) 
      end if
      

!     isigy=int(ny+1d0)
      if (y0 < yf(1) .or. y0 > yf(iym1)) then
        iy=isigy*1+(1-isigy)*iym  !  0<ny: iy=1   ny<0: iy=iym              

        if (ny==0d0) then
          ty=largest
        else
          ty=(yf(iy+1-isigy)-y0)/ny
        end if
        x=x0+nx*ty                
        z=z0+nz*ty                
        if (x < xf(1) .or. x > xf(ixm1) .or.
     *      z < zf(1) .or. z > zf(izm1)) then
          ty=largest
        end if
      else 
        ty=largest       ! yf(1) < y < yf(iym1) 
      end if

!     isigz=int(nz+1d0)
      if (z0 < zf(1) .or. z0 > zf(izm1)) then
        iz=isigz*1+(1-isigz)*izm  !  0<nz: iz=1   nz<0: iz=izm              

        if (nz==0d0) then
          tz=largest
        else
          tz=(zf(iz+1-isigz)-z0)/nz
        end if
        x=x0+nx*tz                
        y=y0+ny*tz                
        if (x < xf(1) .or. x > xf(ixm1) .or.
     *      y < yf(1) .or. y > yf(iym1)) then
          tz=largest
        end if
      else
        tz=largest       ! yf(1) < y < yf(iym1) 
      end if

      dl=min(tx,ty,tz)
      if (dl == largest) then
        ireturn=1
      else
        ixmin=int(dl/tx)               ! 1: if tx=min 0: otherwise
        iymin=int(dl/ty)               ! 1: if ty=min 0: otherwise
        izmin=int(dl/tz)               ! 1: if tz=min 0: otherwise


!debug
!          print*,'iy+1-isigy', iy+1-isigy
!     if(ix+1-isigx > ixm2) then 
!          print*,'ix+1-isigx', ix+1-isigx 
!          stop
!     endif
!debug

      ! if(tx <= min(ty,tz)) then     ! crossing x-boundary
        x1=ixmin*xf(ix+1-isigx)+(iymin+izmin)*(x0+nx*dl)

      ! if(ty <= min(tx,tz)) then     ! crossing y-boundary
        y1=iymin*yf(iy+1-isigy)+(ixmin+izmin)*(y0+ny*dl)

      ! if(tz <= min(tx,ty)) then     ! crossing z-boundary
        z1=izmin*zf(iz+1-isigz)+(ixmin+iymin)*(z0+nz*dl)
        ireturn=0
      end if
      end subroutine find_bound_from_outside



      subroutine find_bound_from_inside(x0,y0,z0, x1,y1,z1,ireturn)
!     implicit real*8(a-h,o-z)
      use grid_size 
      use phys_const
      implicit none
!     integer,parameter ng=NG
!     integer,parameter ngz=NGZ
!     integer,parameter ixm=ng,iym=ng,izm=ngz,
!    *          ixm1=ixm+1,iym1=iym+1,izm1=izm+1,
!    *          ixm2=ixm1+1,iym2=iym1+1,izm2=izm1+1
      real*8 ::  tx, ty, tz, x0, y0, z0, dl, x1, y1, z1
      integer isigx,ix,isigy,iy,isigz,iz, ireturn
      integer :: ixmin, iymin, izmin
      real*8, parameter::largest=1d50
!     common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0

!     find the crossing point through the boundary

      ! nx must be -1<nx<1
      if (nx== 1d0) nx=nx-1d-10
      if (nx==-1d0) nx=nx+1d-10
      if (ny== 1d0) ny=ny-1d-10
      if (ny==-1d0) ny=ny+1d-10
      if (nz== 1d0) nz=nz-1d-10
      if (nz==-1d0) nz=nz+1d-10

      isigx=int(nx+1d0)
      ix=ixm*isigx+(1-isigx)*1  !  0<nx: ix=ixm   nx<0: ix=1      

      isigy=int(ny+1d0)
      iy=iym*isigy+(1-isigy)*1  !  0<ny: iy=iym   ny<0: iy=1              

      isigz=int(nz+1d0)
      iz=izm*isigz+(1-isigz)*1  !  0<nz: iz=izm   nz<0: iz=1              


!     tx,ty,tz are lengths traveling before reaching a boundary
 
      if (nx==0d0) then
        tx=largest
      else
        tx=(xf(ix+isigx)-x0)/nx ! nx > 0: distance to xf(ixm+1) 
                                ! nx < 0:             xf(1)
      end if

      if (ny==0d0) then
        ty=largest
      else
        ty=(yf(iy+isigy)-y0)/ny
      end if

      if (nz==0d0) then
        tz=largest
      else
        tz=(zf(iz+isigz)-z0)/nz
      end if

      dl=min(tx,ty,tz)
      if (dl == largest) then
        write(*,*) ' err in finding boundary from inside.', tx, ty, tz
        stop
      else
        ixmin=int(dl/tx)               ! 1: if tx=min 0: otherwise
        iymin=int(dl/ty)               ! 1: if ty=min 0: otherwise
        izmin=int(dl/tz)               ! 1: if tz=min 0: otherwise

      ! if(tx <= min(ty,tz)) then     ! crossing x-boundary
        x1=ixmin*xf(ix+isigx)+(iymin+izmin)*(x0+nx*dl)

      ! if(ty <= min(tx,tz)) then     ! crossing y-boundary
        y1=iymin*yf(iy+isigy)+(ixmin+izmin)*(y0+ny*dl)

      ! if(tz <= min(tx,ty)) then     ! crossing z-boundary
        z1=izmin*zf(iz+isigz)+(ixmin+iymin)*(z0+nz*dl)
        ireturn=0
      end if
      end subroutine find_bound_from_inside




!
!     dim(ista,iend) for myrank
!
      subroutine para_range(N1,N2,Nprocs,irank,ista,iend)
      integer, parameter: : kind=1
      if (kind == 1) then
        iwork1=(N2-N1+1)/Nprocs
        iwork2=mod(N2-N1+1,Nprocs)
        ista=irank*iwork1 + N1 + Min(irank,iwork2)
        iend=ista+iwork1-1
        if (iwork2 > irank) iend=iend+1
      else
        iwork=(N2-N1)/Nprocs+1
        ista=Min(Irank*Iwork+N1,N2+1)
        iend=Min(Ista+Iwork-1, N2)
      end if
      return
      end subroutine para_range


! ********************************* UNIFORM DEVIATES GENERATOR *********        
      FUNCTION RAN1(IDUM)                                                       
      real*8 :: ran1
      integer :: idum
      DIMENSION R(97)                                                           
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
        DO  J=1,97                                                            
          IX1=MOD(IA1*IX1+IC1,M1)                                               
          IX2=MOD(IA2*IX2+IC2,M2)                                               
          R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1                                  
        ENDDO
        IDUM=1                                                                  
      END IF                                                                    
      IX1=MOD(IA1*IX1+IC1,M1)                                                   
      IX2=MOD(IA2*IX2+IC2,M2)                                                   
      IX3=MOD(IA3*IX3+IC3,M3)                                                   
      J=1+(97*IX3)/M3                                                           
      IF(J.GT.97.OR.J.LT.1)PAUSE                                                
      RAN1=R(J)                                                                 
      R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1                                      
      RETURN                                                                    
      END                                                                       
!                                                                               
