#ifdef VPP
#define NG 64 
#define NGZ 32 
#define INFILENAME 'agn_input2.data'
#define NRAY 100
#define LOOPMAX 10
#define MOLECULE 'CO.d'
#define CCOEFF 'CO-H2para.dat'
#endif
#ifdef ALPHA
#define NG 64 
#define NGZ 32 
#define INFILENAME '/misc/home/wada/2005paper/nonLTE_AGN/agn_input2.data'
#define NRAY 100
#define LOOPMAX 10
#define MOLECULE 'CO.d'
#define CCOEFF 'CO-H2para.dat'
#endif
#define NRAY0 300
#define LEVMAX 10

      module Rays
      real*8, save,allocatable :: 
     &        x0(:,:,:), y0(:,:,:), z0(:,:,:)
      real*8, save,allocatable :: 
     &        nx(:,:,:,:), ny(:,:,:,:),
     &        nz(:,:,:,:), dnu_nu0(:,:,:,:)
      real*8, save,allocatable :: 
     &        x1(:,:,:,:), y1(:,:,:,:), z1(:,:,:,:)
      real*8, save,allocatable :: 
     &        x2(:,:,:,:), y2(:,:,:,:), z2(:,:,:,:)
      integer Nray
      end module Rays
!### KW ###############################################
      Module  state_inc

!---- Define Data cube size (ix=0:ixmax,iy=0:ixmax,iz=0:ixmax)
      integer, parameter :: ng=NG
      integer, parameter :: ngz=NGZ
      integer,  parameter ::  nmesh=NG
      integer,  parameter ::  ixmax=nmesh-1,iymax=nmesh-1,izmax=ngz-1
      integer, parameter :: ixm = ng, iym = ng, izm = ngz
      integer, parameter :: ixm1 = ixm+1, iym1 = iym+1, izm1 = izm+1
!---- Define maximum level sumpber. NSPECIESMAX is dummy index. Set 1.
!---- Calculation level is from 0 to NLEVMAX-1
      integer,  parameter ::  nlevmax=30,NSPECIESMAX=1      ! For CO
!        Parameter (NLEVMAX=21,NSPECIESMAX=1)    ! For HCO+

      integer, parameter ::      TABLEMIN=1,TABLEMAX=40

!---- Physical constants
      real*8, Parameter :: KB=1.380664D-16

        Parameter(NS=1) ! dummy
       INTEGER NL(NSPECIESMAX)

      real*8 tk( 0:ixmax,0:iymax,0:izmax)  ! temperature
      real   rho( 0:ixmax,0:iymax,0:izmax)  ! density

!      Real*8 ELEV(0:NLEVMAX)  for Imaeda code
      Real*8 E(0:NLEVMAX)      ! level energy, for Tomisaka code
	
      Real*8 MU,BB

!      Real*8 C(NSPECIESMAX,-1:NLEVMAX+1,-1:NLEVMAX+1) &

      real*8 :: C(0:nlevmax, 0:nlevmax)
      real*8 :: alpha(LEVMAX,ixmax+1,iymax+1,izmax+1)
      real*8 :: S(LEVMAX,ixmax+1,iymax+1,izmax+1)
      real*8 :: n(0:LEVMAX,ixmax+1,iymax+1,izmax+1)
      real*8 :: ntot(ixmax+1,iymax+1,izmax+1)
      real*8 :: CTABLE(TABLEMIN:TABLEMAX,-1:NLEVMAX+1,-1:NLEVMAX+1)

      real*8,dimension(0:ixm1,0:iym1,0:izm1) :: vx,vy,vz

!      real*8 :: GSTAT(0:NLEVMAX)

      Real*8 tklist(0:TABLEMAX)
      Character(16)    TABLENAME

      real*8,external :: g   ! statistical weight

      END MODULE state_inc

      function g(j) 
      real*8 :: g
      integer :: j

      g = 2*j +1
      
      END function g

      program main

!
!     this is non-LTE calculation
!
!     2.6.1
!     3.0:   (nx,ny,nz) was incorrectly assigned.  Distribute points
!            uniformly in the cube and then reduce their length.  This
!            lead a nonuniform distribution.  
!     4.0:   To solve A n =b (simultaneous equations), b was incorrectly
!            assigned (0,0,0,...,n(CO) at (1,1,1) grid point), which should
!            be different at each grid point.
!     5.0:   MPI parallel version
!     5.1:   Intensity(Nray,lev,i,j,k)--> Intensity(Nray,lev)
!            final output
!     6.0:   Iteration for the internal process (radiative transfer -
!              population loop)
!     6.1:   3K background
!              From this version, final output file contains BG at the end of
!              of the file.
!     7.0:   Memory reduced version
!     7.1:(to do) read data from molecular database
!            read   
!                   molecular data molecule_data=99: closed after use.
!                   structure infile=50: closed after use.
!            write  
!                   final=49: nlev.d 
!                   lev+50=50,51,...    norm
!                   lev+60=60,61,...    disp
!                   lev+10=10,11,...    Tr  
!            NRAY0, LEVMAX are changed to variables in cpp
!            L.986 do lev=0,lavmax-->do lev=1,lavmax
!
!     7.2:   Solve_I_0 is revised to fit 
!              (1) rays paths through the corner of a grid. 
!              (2) the ray stops at the corner by chance.
!             ( Aug.23,02 )
!     7.3:  different seed must be supplied to different PE
!           changed from seed to seed+Myrank
!     7.6:  non-uniform initial condition (density, temperature, 
!           velocity) given by a subroutine "read_data" 
!           Maximum levels are now up to J=10  
!           C(i,j) is interpolated for temperature, and calculated 
!           in a subroutine "get_c" with
!           a table "CO-H2para.dat".   by K.W. 5/23/2003
!
      use state_inc   ! module for get_c
         
      implicit real*8 (a-h,o-z) 
      parameter (levmax=LEVMAX,Loopmax=LOOPMAX)
      dimension dif(0:levmax)

#ifdef MPI
      include 'mpif.h'
      common /M_param/ Nprocs, Myrank, ista, iend

!------ read density and temperature data ---
      call read_data
!--------------------------------------------

      call mpi_init(ierr)
      call mpi_comm_size(mpi_comm_world, Nprocs, ierr)
      call mpi_comm_rank(mpi_comm_world, Myrank, ierr)

      call para_range(1,izm,Nprocs,Myrank,ista,iend)


      do Np=0,Nprocs-1
        if (Myrank== Np) then
          write(*,1000) Myrank,Nprocs,ista,iend
 1000     format('Proc#=',i3,'/',i3,3x,'Dim=(',i3,',',i3,')')
        end if
        call mpi_barrier(mpi_comm_world,ierr)
      end do
#endif

      if (Myrank==0) then
        print*,'call initia0'
      endif
      call initia0   ! make a model of uniform cloud 
#ifdef ALPHA
      print*,'<main> max, min of tk: ', maxval(tk), minval(tk)
#endif
      if (Myrank==0) then
        print*,'end  initia0'
      endif
!      call initiaR   ! read sample data from a file
!      call store_n0

!    getting C co-efficient
!      tk = 20.
      call get_c_0
      do k=0,izm-1
      do j=0,iym-1
      do i=0,ixm-1
!          call get_c(i,j,k)
      enddo
      enddo
      enddo

!

      do Loop=1,Loopmax
        call Solve_I
!debug
!        stop
!debug
        call norm(dif)
        call underrelaxation(1d0)
        if(mod(Loop,10**(int(log10(float(Loop)))))==0) then
!         if(mod(Loop,1)==0) then
#ifdef MPI
          if (Myrank==0) then
            call Temperature(Loop,dif)
            call final_output()
          end if
#endif
#ifndef MPI
          call Temperature(Loop,dif)
          call final_output()
#endif
        end if


      end do  ! end loop
      
#ifdef MPI
!     if (Myrank==0) then
!       call final_output()
!     end if
      call mpi_finalize(ierr)
#endif
#ifndef MPI
!     call final_output()
#endif

      end program main


      Subroutine relaxation
      use state_inc
      implicit real*8(a-h,o-z)
      parameter(levmax=LEVMAX,ijkm=ixm*iym*izm)
      common /density/ Xc
!      real*8  n(0:levmax,ijkm),ntot(ijkm), Xc
      real*8  Xc
      real*8  n0(0:levmax, ixm, iym, izm)
      real*8  dif(0:levmax)
      integer :: i,j,k
!      save n0

      entry store_n0
!      n0(:,:)=n(:,:)
      n0 = n
      return

      entry underrelaxation (fac)

!      n(:,:)=fac*n(:,:)+(1d0-fac)*n0(:,:)
!      n0(:,:)=n(:,:)
      n =fac*n +(1d0-fac)*n0
      n0=n
      return

      entry norm(dif)
      do lev=0,levmax
        dif(lev)=0d0
        do k=1,izm
        do j=1,iym
        do i=1,ixm
        dif(lev)=dif(lev)+abs((n(lev,i,j,k)-n0(lev,i,j,k))/n(lev,i,j,k))
        end do
        end do
        end do
        dif(lev)=dif(lev)/ijkm
      end do
      end

!     initialize

      subroutine read_data
      use state_inc
      implicit none
      integer :: i,j,k
      integer, parameter :: idum = -1, idum2 = -2, ix=1234
      real :: ran1,ran2,ran
      real :: tem( 0:ixmax,0:iymax,0:izmax)  ! temperature
      integer :: ngin
      character(60) infile_name

      infile_name = INFILENAME 
      print*,'INPUT FILE: ', infile_name
      open(1,file=infile_name,
     *     form='unformatted',status='old')
!     --- rho, t are single precision --------	
!     --- vx,vy,vz are double precision --------	
      read(1) ngin 
      read(1) rho, tem, vx,vy,vz
      close(1)
       if(ngin .ne. ng)then
           print*,'input ng is incorrect', ngin, ng
           stop
        endif

        tk = tem

      do k = 0, izmax
      do j = 0, iymax
      do i = 0, ixmax
!        rho(i,j,k) = 2.0d3 + 1.0e3*(2.*ran(ix) -1.)
!         rho(i,j,k) = 2.0d3
!         tk(i,j,k) = 20.*ran(idum)+ 10.
!         tk(i,j,k) = 20. 
!	print*,'read_data:vx,vy,vz ',
!     &  vx(i,j,k), vy(i,j,k),vz(i,j,k)
      enddo
      enddo
      enddo

#ifdef ALPHA
      print*,'max, min tk()', maxval(tk), minval(tk)
      print*,'max, min vx()', maxval(vx), minval(vx)
      print*,'max, min vy()', maxval(vy), minval(vy)
      print*,'max, min vz()', maxval(vz), minval(vz)
#endif
      end subroutine read_data

      subroutine initia0
      use Rays
      use state_inc
      implicit real*8(a-h,o-z)
      parameter(Nray0=NRAY0,levmax=LEVMAX,   
     *          ixm2=ixm+2,iym2=iym+2,izm2=izm+2,
     *          pi=3.1415926535d0, pc=3.0857d18)
      
      common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0, eps,
     *              xcent, ycent, zcent


!     common /Rays/ x0,y0,z0,nx,ny,nz,x1,y1,z1,x2,y2,z2,dnu_nu0,Nray
!     points where Js are calculated
!      real*8 x0(ixm,iym,izm), y0(ixm,iym,izm), z0(ixm,iym,izm) 
!     directions of Rays from boundary to (x0,y0,z0)
!     real*8 nx(Nray0,ixm,iym,izm), ny(Nray0,ixm,iym,izm),
!    *       nz(Nray0,ixm,iym,izm), dnu_nu0(Nray0,ixm,iym,izm)
!     initial position on the boundary
!     dimension x1(Nray0,ixm,iym,izm), y1(Nray0,ixm,iym,izm), 
!    *          z1(Nray0,ixm,iym,izm)
!     final position on the cell boundary
!     dimension x2(Nray0,ixm,iym,izm), y2(Nray0,ixm,iym,izm),
!    *          z2(Nray0,ixm,iym,izm) 

!     velocity field
      common /spectrum/ sigma
      common /density/ Xc
      common /const/   h, lc, bc
      real*8           lc

!     initial n0
!      real*8  n(0:levmax,ixm,iym,izm),ntot(ixm,iym,izm),XC
      real*8  XC

      real*8 n0,TB,kTB
      dimension rand(3*Nray0+3),rand_normal(Nray0)

      common /molecule/ A(levmax),B(levmax),
     *                  BG(levmax)
!     *                  A(levmax),B(levmax),C(0:levmax,0:levmax),


      integer,parameter:: molecule_data=99
      character (len=10):: kind
#ifdef MPI
      include 'mpif.h'
      common /M_param/ Nprocs, Myrank, ista, iend
#endif
#ifndef MPI
      ista=1
      iend=izm
#endif
      if(Myrank==0)then
      write(*,*) 'NG: ', NG
      write(*,*) 'NGZ: ', NGZ
      write(*,*) 'Nray: ', Nray
      write(*,*) 'levmax: ', levmax 
      endif

      allocate(x0(ixm,iym,izm), y0(ixm,iym,izm), z0(ixm,iym,izm))
      allocate(nx(Nray0,ixm,iym,ista:iend), 
     *         ny(Nray0,ixm,iym,ista:iend),
     *         nz(Nray0,ixm,iym,ista:iend), 
     *         dnu_nu0(Nray0,ixm,iym,ista:iend))
      allocate(x1(Nray0,ixm,iym,ista:iend),
     *         y1(Nray0,ixm,iym,ista:iend), 
     *         z1(Nray0,ixm,iym,ista:iend))
      allocate(x2(Nray0,ixm,iym,ista:iend), 
     *         y2(Nray0,ixm,iym,ista:iend),
     *         z2(Nray0,ixm,iym,ista:iend))

      H    = 6.626184D-27 !Plank Constant [erg s]
      lc=2.99792458d10    !Speed of Light [cm/s] 
      bc=1.380658d-16     !Boltzmann Constant [erg/K]


!     rmax=1*pc
!     dx0=rmax/(ng/2)
      dx0= 64*pc/ng    ! grid size (cm)

      n0=2d3                !H2
!      T=10d0
!      kT=bc*T
      TB=2.7d0
      kTB=bc*TB       


!      v0=1d5/3.0857d18  ! infall velocity (1km/s / 1pc)
      v0=1d5/3.0857d18*dx0  ! due to normalization this is necessary
!      v0=0d0
      vturb=1d5         ! micro turbulence ( cm/s)
!      vturb=1d4         ! micro turbulence (1km/s)
      sigma=vturb/lc/sqrt(2d0)

      open(molecule_data,file=MOLECULE,
     *     form='formatted',status='old')
      read(molecule_data,'(a10)') kind
      write(*,'(a10)') kind
      read(molecule_data,*) XC
!      XC=5.0D-5       !CO (Irvine et al. 1987)  CO/H2
      read(molecule_data,*) bb
!      bb=57.6356D9  ! CO Rotation Constant (Robinson 1974) in GHz
      read(molecule_data,*) mu
!      mu=0.112D-18  ! CO electric dipole moment 
      write(*,1000) xc,bb,mu
 1000 format('Composition=',1pe10.3,3x,
     *       'Rotation Const=',e10.3,3x,
     *       'Dipole Moment=',e10.3)

!      dx=dx0
!      dy=dx
!      dz=dx
!     r2=(rmax/dx0)**2
      eps=1d-8

      Nray=NRAY

      do lev=0,levmax
        e(lev)=bb*lev*(lev+1)*h              ! E(J): energy level, e(0)=0 
      end do

      COEF=32.D0/3.0D0 *(pi**4)*(mu**2)/(h**2)/lc
      do lev=1,levmax
        B(lev)=coef*lev/(2.0D0*lev+1.0D0)     ! B(J)=B J->J-1 
                 ! B(J+1->J)=(32/3) pi^4 mu^2/h^2/c (J+1)/(2J+3)
        A(lev)=16.D0*H*BB*(BB/LC)**2*lev**3*B(lev) ! A(J)=A J->J-1
                 ! A(J+1->J)=16 hB^3/c^2 (J+1)^3 B(J+1->J) 
      end do
      do lev=1,levmax
        BG(lev)=2*(e(lev)-e(lev-1))**3/(h*lc)**2
     *          /(exp((e(lev)-e(lev-1))/kTB)-1d0)    !lev=1: 1->0
      end do
      
      read(molecule_data,*) level0
      if (level0 .ne. levmax) then
        write(*,'(''levmax='',i2,3x,''lev data='',i2)')levmax,level0
      endif

! KW
!      do j=0,levmax
!        do i=0,levmax
!          read(molecule_data,*) ii,jj,C(i,j)
!        end do
!      end do
!      print*,'max(C), min(C)', maxval(C), minval(C)
!      stop
! KW

      close(molecule_data)

c---- Green & Thaddeus 1976  Table 7 (CO-H2 20K)
  
      do i=0,ixm+2
        xf(i)=(i-1)
      end do

      do j=0,iym+2
        yf(j)=(j-1)
      end do

      do k=0,izm+2
        zf(k)=(k-1)
      end do

      xcent=(xf(1)+xf(ixm1))/2d0
      ycent=(yf(1)+yf(iym1))/2d0
      zcent=(zf(1)+zf(izm1))/2d0

      xf(0:ixm2)=xf(0:ixm2)-xcent
      yf(0:iym2)=yf(0:iym2)-ycent
      zf(0:izm2)=zf(0:izm2)-zcent

      do k=1,izm
        do j=1,iym
          do i=1,ixm
            w=0d0
            do lev=0,levmax
              w=w+(2d0*lev+1d0)*exp(-E(lev)/(bc*tk(i-1,j-1,k-1)))
            end do
            do lev=0,levmax
!              n(lev,i,j,k)=n0*Xc*(2d0*lev+1d0)*exp(-E(lev)/kT)/w
               n(lev,i,j,k)=rho(i-1,j-1,k-1)*Xc*  
     &          (2d0*lev+1d0)*exp(-E(lev)/(bc*tk(i-1,j-1,k-1)))/w
            end do
!            ntot(i,j,k)=n0
            ntot(i,j,k)= rho(i-1,j-1,k-1)

!           if (((xf(i)+xf(i+1))/2d0)**2
!    *         +((yf(j)+yf(j+1))/2d0)**2
!    *         +((zf(k)+zf(k+1))/2d0)**2 > r2 ) then
!             do lev=0,levmax
!               n(lev,i,j,k)=n(lev,i,j,k)* 
!    &          ((((xf(i)+xf(i+1))/2d0)**2
!    &         +((yf(j)+yf(j+1))/2d0)**2
!    &         +((zf(k)+zf(k+1))/2d0)**2)/r2)**(-5)
!             end do
!             ntot(i,j,k)=ntot(i,j,k)*
!    &          ((((xf(i)+xf(i+1))/2d0)**2
!    &         +((yf(j)+yf(j+1))/2d0)**2
!    &         +((zf(k)+zf(k+1))/2d0)**2)/r2)**(-5)
!           end if

          end do
        end do
      end do

!     do k=0,izm1
!       do j=0,iym1
!         do i=0,ixm1
!           r=sqrt(((xf(i)+xf(i+1))/2d0)**2
!    *            +((yf(j)+yf(j+1))/2d0)**2
!    *            +((zf(k)+zf(k+1))/2d0)**2) 
!           if(r.eq.0d0) then
!             vx(i,j,k)=0d0
!             vx(i,j,k)=0d0
!             vx(i,j,k)=0d0
!           else
!             vx(i,j,k)=-v0*r*(xf(i)+xf(i+1))/2d0/r
!             vy(i,j,k)=-v0*r*(yf(j)+yf(j+1))/2d0/r
!             vz(i,j,k)=-v0*r*(zf(k)+zf(k+1))/2d0/r
!           end if
!         end do
!       end do
!     end do

      call seed2()
      call seed_normal()

      do k=1,izm
        do j=1,iym
          do i=1,ixm
            x0(i,j,k)=(xf(i)+xf(i+1))/2d0
            y0(i,j,k)=(yf(j)+yf(j+1))/2d0
            z0(i,j,k)=(zf(k)+zf(k+1))/2d0
          end do
        end do
      end do
        
#ifdef MPI
      do k=max(1,ista),min(izm,iend)
#endif
#ifndef MPI
      do k=1,izm
#endif
        do j=1,iym
          do i=1,ixm
            call mkNrays(x0(i,j,k),y0(i,j,k),z0(i,j,k),
     *                   nx(1,i,j,k),ny(1,i,j,k),nz(1,i,j,k),
     *                   x1(1,i,j,k),y1(1,i,j,k),z1(1,i,j,k),
     *                   x2(1,i,j,k),y2(1,i,j,k),z2(1,i,j,k),
     *                   dnu_nu0(1,i,j,k),sigma,rand,rand_normal,
     *                   Nray,i,j,k)
          end do
        end do
      end do

      nx(:,:,:,:)=-nx(:,:,:,:)
      ny(:,:,:,:)=-ny(:,:,:,:)
      nz(:,:,:,:)=-nz(:,:,:,:)

      end subroutine initia0

!     initialize from data
!     subroutine initiaR is deleted by KW

      subroutine getran(rand,N)
      implicit real*8(a-h,o-z)
      parameter(Nray0=NRAY0)
      save ix, init
      data isw/0/,init/1/
      dimension rand(N),ivw(128)
      real*4 a(10*Nray0),ran69069, ran
#ifdef MPI
      include 'mpif.h'
      common /M_param/ Nprocs, Myrank, ista, iend
#endif

      if (init==1) then
        write(*,'(''use getran after seed.'')')
        stop
      end if

#ifdef VPP
      call ranu3(ix,a,2*N,isw,ivw,icon)
      isw=1
      if (icon > 0) then
        write(*,'(''ranu3 icon='',i4)') icon
        stop
      end if
#endif

#ifdef ALPHA
      do i=1,2*N    !+N/20:  extra
        !a(i)=ran69069(ix)
         a(i)=ran(ix)
      end do
#endif
      rand(1:N)=a(1:N)*2d0-1d0

      do i=1,N
        if(rand(i)==0d0.or.abs(rand(i)).ge.1d0) then
          rand(i)=a(N+i)*2d0-1d0
          if(rand(i)==0d0.or.abs(rand(i)).ge.1d0) then
            write(*,*) 'error in randum number benerator'
            stop
          end if
        end if
      end do 

      return

      entry seed2()
#ifdef MPI
      ix=1234+Myrank
#endif
#ifndef MPI
      ix=1234
#endif
      init=0
      return
      end

      subroutine getran_normal(rand_normal,N)
      implicit real*8(a-h,o-z)
      parameter(Nray0=NRAY0)
      real*8 rand_normal(N)
      real*4 s(Nray0)
      data init/1/
      save init,ix
#ifdef MPI
      include 'mpif.h'
      common /M_param/ Nprocs, Myrank, ista, iend
#endif

      if (init==1) then
        write(*,'(''use getran_normal after seed_normal.'')')
        stop
      end if

#ifdef ALPHA
      call NRAND(N, rand_normal, ix)
#endif
#ifdef VPP
      call rann1(0e0,1e0,ix,s,N,icon)
      if (icon > 0) then
        write(*,'(''rann1 icon='',i4)') icon
        stop
      end if
      rand_normal(1:N)=s(1:N)
#endif
      return

      entry seed_normal()
#ifdef MPI
      ix=1234+Myrank
#endif
#ifndef MPI
      ix=1234
#endif
      init=0
      return
      end
      


      subroutine mkNrays(x0,y0,z0,nx,ny,nz,x,y,z,
     *                   x1,y1,z1,dnu_nu0,sigma,
     *                   rand,rand_normal,Nray,i0,j0,k0)

      use state_inc
      implicit real*8(a-h,o-z)
      parameter (ixm2=ixm+2,iym2=iym+2,izm2=izm+2)
      common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0, eps,
     *              xcent, ycent, zcent

      real*8 x0,y0,z0                      ! a point where J is calculated
      real*8 nx(Nray), ny(Nray), nz(Nray)  ! directions of Rays from boundary to (x0,y0,z0)
      dimension x(Nray), y(Nray), z(Nray)  ! initial position on the boundary
      dimension x1(Nray), y1(Nray), z1(Nray) ! final position on the cell boundary
      dimension ix(Nray), iy(Nray), iz(Nray) ! grid num of the initial position
      dimension rand(3*Nray+3)             ! randum number in (-1,1)
      dimension rand_normal(Nray)
      real*8 dnu_nu0(Nray),px,py,pz
      integer iray

!      integer::iran=1

!     choose a point in the cell
!          rand() is uniform random number from -1 to 1
!          REJECT if |rand()|=1

!     Make (Nray+1)*3 randum number in (0,1)
      call getran(rand,(Nray+1)*3)

!     Make Nray normal randum number average=0, sd=1
      call getran_normal(rand_normal,Nray)

!      if (iran==1) then
!        x0=(xf(i0)+xf(i0+1))/2
!     *            +rand(3*Nray+1)*(xf(i0+1)-xf(i0))/2*1d-3  ! random
!        y0=(yf(j0)+yf(j0+1))/2
!     *            +rand(3*Nray+2)*(yf(j0+1)-yf(j0))/2*1d-3 
!     z0=(zf(k0)+zf(k0+1))/2
!     *            +rand(3*Nray+3)*(zf(k0+1)-zf(k0))/2*1d-3 
!      else
!        x0=(xf(i0)+xf(i0+1))/2
!        y0=(yf(j0)+yf(j0+1))/2
!        z0=(zf(k0)+zf(k0+1))/2
!      end if

!     choose the direction randomly.

      pi=atan(1d0)*4d0
      w=0d0
      w2=0d0
      do iray=1,Nray
        costh=rand(iray)  ! -1 < cos(th) < +1
        phi=2d0*pi*rand(iray+Nray)
        px=sqrt(1d0-costh**2)*cos(phi)
        py=sqrt(1d0-costh**2)*sin(phi)
        pz=costh
!        s=sqrt(px*px+py*py+pz*pz)
!        s=1.
!       nx(iray)=px/s
!       ny(iray)=py/s
!       nz(iray)=pz/s
        nx(iray)=px
        ny(iray)=py
        nz(iray)=pz
        dnu_nu0(iray)=rand_normal(iray)*sigma       ! dnu/nu0
        w=w+dnu_nu0(iray)
        w2=w2+dnu_nu0(iray)**2
!	if(abs(nx(n)).le. 1e-3)then
!	print*, 'Nray: ', n,'nx: ',
!     &   nx(n),'px:', 'costh: ', costh, 'cos phi: ', cos(phi)
!	endif
      end do
!      write(*,*) w/Nray,sqrt(w2/Nray-(w/Nray)**2),sqrt(w2/Nray)

!     find the crossing point through the boundary

      do iray=1,Nray


                                     !  floor(x)+1     x>0-->1   x<0 -->0
!@@        isigx=int(nx(N)+1d0)
!@@        ix(N)=ixm*isigx+(1-isigx)*1  !  0<nx: ix=ixm   nx<0: ix=1      
!@@      isigy=int(ny(N)+1d0)
!@@     iy(N)=iym*isigy+(1-isigy)*1  !  0<ny: iy=iym   ny<0: iy=1              
!@@        isigz=int(nz(N)+1d0)
!@@        iz(N)=izm*isigz+(1-isigz)*1  !  0<nz: iz=izm   nz<0: iz=1              
         if(nx(iray) > 0)then
              ix(iray) = ixm
              isigx = 1
         else
              ix(iray) = 1
              isigx = 0
         endif
         if(ny(iray) > 0)then
              iy(iray) = iym
              isigy = 1
         else
              iy(iray) = 1
              isigy = 0
         endif
         if(nz(iray) > 0)then
              iz(iray) = izm
              isigz = 1
         else
              iz(iray) = 1
              isigz = 0
         endif


!       tx,ty,tz are lengths traveling before reaching a boundary
! debug
!	if( ix(N)+isigx .gt. ixm2 .or. iy(N)+isigy .gt. iym2  
!     &  .or. iz(N)+isigz .gt. izm2)then
!	   print*,'ERROR !!!', ix(N)+isigx, iy(N)+isigy, iz(N)+isigz
!	   stop
!	endif  
! debug
 
!@      tx=(xf(ix(N)+isigx)-x0)/nx(N) ! nx > 0: distance to xf(ixm+1) 
!@                                    ! nx < 0:             xf(1)
!@      ty=(yf(iy(N)+isigy)-y0)/ny(N)
!@      tz=(zf(iz(N)+isigz)-z0)/nz(N)

        if( nx(iray) > 0)then
          tx=(xf(ixm+1) - x0)/nx(iray)
        else
          tx=(xf(1) - x0)/nx(iray)
        endif
        if( ny(iray) > 0)then
          ty=(yf(iym+1) - y0)/ny(iray)
        else
          ty=(yf(1) - y0)/ny(iray)
        endif
        if( nz(iray) > 0)then
          tz=(zf(izm+1) - z0)/nz(iray)
        else
          tz=(zf(1) - z0)/nz(iray)
        endif


        if(abs(tx) > 1.e8) tx=1.e8
        if(abs(ty) > 1.e8) ty=1.e8
!       if(abs(tz) > 1.e4) tz=1.e4

!	print*,'tx, ty, tz = ', tx, ty, tz
        dl=min(tx,ty,tz)
!	print*, 'dl = ', dl, dl/tx, dl/ty, dl/tz
!       ixmin=int(dl/tx)               ! 1: if tx=min 0: otherwise
!       iymin=int(dl/ty)               ! 1: if ty=min 0: otherwise
!       izmin=int(dl/tz)               ! 1: if tz=min 0: otherwise

        if(dl < tx)then
           ixmin = 0
        else
           ixmin = 1
        endif
        if(dl < ty)then
           iymin = 0
        else
           iymin = 1
        endif
        if(dl < tz)then
           izmin = 0
        else
           izmin = 1
        endif

!debug
!       if(ixmin.eq.0 .and. iymin.eq.0 .and. izmin.eq.0)then
!	print*,'ixmin, iymin,izmin', ixmin, iymin, izmin
!	print*, dl/tx, dl/ty, dl/tz
!       endif
!debug

        ! if(tx <= min(ty,tz)) then     ! crossing x-boundary
          x(iray)=ixmin*xf(ix(iray)+isigx)
     *         +(1-ixmin)*(x0+nx(iray)*dl)


        ! if(ty <= min(tx,tz)) then     ! crossing y-boundary
          y(iray)=iymin*yf(iy(iray)+isigy)
     *         +(1-iymin)*(y0+ny(iray)*dl)

        ! if(tz <= min(tx,ty)) then     ! crossing z-boundary
          z(iray)=izmin*zf(iz(iray)+isigz)
     *         +(1-izmin)*(z0+nz(iray)*dl)
!        if(i0==1.and.j0==1.and.k0==1.and.N==4) then
!          write(*,*) ixmin,iymin,izmin,isigx,isigy,isigz,
!     *               X(N),y(N),z(N)
!        end if
      end do

!     find the crossing point through the cell boundary
      
      do iray=1, Nray
!       tx,ty,tz are lengths traveling before reaching a boundary
 
!@       isigx=int(nx(N)+1d0)
        if(nx(iray) > 0)then
          isigx = 1
        else
          isigx = 0
        endif
        tx=(xf(i0+isigx)-x0)/nx(iray)

!@      isigy=int(ny(iray)+1d0)
        if(ny(iray) > 0)then
           isigy = 1
        else
           isigy = 0
        endif
        ty=(yf(j0+isigy)-y0)/ny(iray)

!@      isigz=int(nz(iray)+1d0)
        if(nz(iray) > 0)then
           isigz = 1
        else
           isigz = 0
        endif
        tz=(zf(k0+isigz)-z0)/nz(iray)

        if(abs(tx) > 1.e8) tx=1.e8
        if(abs(ty) > 1.e8) ty=1.e8
!       if(abs(tz) > 1.e4) tz=1.e4

        dl=min(tx,ty,tz)
        ixmin=int(dl/tx)               ! 1: if tx=min 0: otherwise
        iymin=int(dl/ty)               ! 1: if ty=min 0: otherwise
        izmin=int(dl/tz)               ! 1: if tz=min 0: otherwise

        ! if(tx <= min(ty,tz)) then     ! crossing x-boundary
          x1(iray)=ixmin*xf(i0+isigx)
     *         +(1-ixmin)*(x0+nx(iray)*dl)

        ! if(ty <= min(tx,tz)) then     ! crossing y-boundary
          y1(iray)=iymin*yf(j0+isigy)
     *         +(1-iymin)*(y0+ny(iray)*dl)

        ! if(tz <= min(tx,ty)) then     ! crossing z-boundary
          z1(iray)=izmin*zf(k0+isigz)
     *         +(1-izmin)*(z0+nz(iray)*dl)

!         if(i0==2.and.j0==7.and.k0==1) then
!  	  print*,'<mkNray>  Nray: ', n,':',i0,j0,k0,':',
!     &      i0+isigx,':',
!     &      xf(i0+isigx),x0,
!     &      ':',nx(n),':',tx
!         endif

      end do
!      if(i0==1.and.j0==1.and.k0==1) then
!        N=4
!        write(*,*) x(N),y(N),z(N),x1(N),y1(N),z1(N)
!        write(*,*) nx(N),ny(N),nz(N),ix(N),iy(N),iz(N)
!        write(*,*) zf
!      end if


!debug
!	stop
!debug

      end  subroutine mkNrays 





!
!     Solve I
!
      subroutine Solve_I
      use Rays
      use state_inc
      implicit real*8(a-h,o-z)
      parameter(levmax=LEVMAX,Nray0=NRAY0,
     *          ixm2=ixm+2,iym2=iym+2,izm2=izm+2,
     *          pi=3.1415926d0,pi4=pi*4d0)
      integer, parameter::dim_n=(levmax+1)*ixm*iym*izm,
     &                    dim_c=(nlevmax+1)*(nlevmax+1)
      common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0, eps,
     *              xcent, ycent, zcent
!      common /Rays/ x0,y0,z0,nx,ny,nz,x1,y1,z1,x2,y2,z2,dnu_nu0,Nray
!     points where Js are calculated
!      real*8 x0(ixm,iym,izm), y0(ixm,iym,izm), z0(ixm,iym,izm) 
!     directions of Rays from boundary to (x0,y0,z0)
!      real*8 nx(Nray0,ixm,iym,izm), ny(Nray0,ixm,iym,izm),
!     *       nz(Nray0,ixm,iym,izm), dnu_nu0(Nray0,ixm,iym,izm)
!     initial position on the boundary
!      dimension x1(Nray0,ixm,iym,izm), y1(Nray0,ixm,iym,izm), 
!     *          z1(Nray0,ixm,iym,izm)
!     final position on the cell boundary
!      dimension x2(Nray0,ixm,iym,izm), y2(Nray0,ixm,iym,izm),
!     *          z2(Nray0,ixm,iym,izm) 

      common /const/ h,lc,bc
      real*8 lc 
!     Alpha and S
      real*8  Intensity(Nray0,levmax)  ! I(J)=I of J->J-1   
!      common /alphas/ S


!     density
!      common /density/ n, ntot, Xc
      common /density/ Xc
!      real*8  n(0:levmax,ixm,iym,izm),ntot(ixm,iym,izm),Xc
      real*8  Xc

#ifdef MPI
      real*8  work(0:levmax,ixm,iym,izm)
      real*8 :: work_c(0:nlevmax, 0:nlevmax)
#endif

      common /molecule/  A(levmax),B(levmax), BG(levmax)
!     *                  A(levmax),B(levmax),C(0:levmax,0:levmax),

#ifdef MPI
      include 'mpif.h'
      common /M_param/ Nprocs, Myrank, ista, iend
#endif

!      g(J)=2*J+1

      do k=1,izm
        do j=1,iym
          do i=1,ixm
            do lev=1,levmax
              ! alpha, B: indexed by using upper level
!              alpha(lev,i,j,k)=(E(lev)-E(lev-1))/pi4 
!     *         *(n(lev-1,i,j,k)*g(lev)/g(lev-1)-n(lev,i,j,k))*B(lev)
              alpha(lev,i,j,k)=h/pi4 
     *         *(n(lev-1,i,j,k)*g(lev)/g(lev-1)-n(lev,i,j,k))*B(lev)
!debug
!              print*,lev, 'alpha', i,j,k,alpha(lev,i,j,k)
!debug
!               if(alpha(lev, i,j,k) .le. 0.)then
!                  print*,'error: alpha < 0', i,j,k
!                  stop
!               endif


!              print*,lev, i,j,k,' L1580, alpha = ', alpha(lev, i,j,k)
                                                       ! B(lev)=B lev-> lev-1
              S(lev,i,j,k)=n(lev,i,j,k) *A(lev)
     *          /(n(lev-1,i,j,k)*g(lev)/g(lev-1)-n(lev,i,j,k))/B(lev)
            end do
          end do
        end do
      end do

!      print*,'<solve_i> max, min of alpha', maxval(alpha), minval(alpha)


!debug
!      print*,'L962 alpha(1,3,3,1) ', alpha(1,3,3,1)
!      print*,'L962 alpha(2,3,3,1) ', alpha(2,3,3,1)
!      print*,'L962 alpha(5,3,3,1) ', alpha(5,3,3,1)
!      stop
!debug


!     Solve dI/ds=-alpha I+j      

!     ******************
#ifdef MPI
      n(:,:,:,:)=0d0       ! necessary for all_reduce
#endif
!     ******************
#ifndef MPI
      do k=1,izm
#endif
#ifdef MPI
      do k=max(ista,1),min(iend,izm)
#endif
        do j=1,iym
          do i=1,ixm
!            Intensity(1:Nray,1:levmax)=0d0

!           do lev=0,levmax
            do lev=1,levmax
              Intensity(1:Nray,lev)=BG(lev)
            end do

!debug
!            print*,'<solve_i> alpha', i,j,k, alpha(1,i,j,k)
!debug

!           solve external 
            call solve_I_0(Intensity(1,1),
     *                  nx(1,i,j,k),ny(1,i,j,k),nz(1,i,j,k),
     *                  x1(1,i,j,k),y1(1,i,j,k),z1(1,i,j,k),
     *                  x2(1,i,j,k),y2(1,i,j,k),z2(1,i,j,k),
     *                  dnu_nu0(1,i,j,k),
     *                  Nray,i,j,k)

!           solve internal one

!           ---  C(i,j) ------
            call get_c(i-1,j-1,k-1)

#ifdef MPI
      call mpi_allreduce(c,work_c,dim_c,mpi_double_precision,
     *                   mpi_sum,mpi_comm_world,ierr)
      c = work_c
#endif

            call internal(Intensity(1,1),
     *                   x0(i,j,k),y0(i,j,k),z0(i,j,k),
     *                   nx(1,i,j,k),ny(1,i,j,k),nz(1,i,j,k),
     *                   x2(1,i,j,k),y2(1,i,j,k),z2(1,i,j,k),
     *                   dnu_nu0(1,i,j,k),
     *                   Xc,Nray,i,j,k)
          end do
        end do
      end do

#ifdef MPI
      call mpi_allreduce(n,work,dim_n,mpi_double_precision,
     *                   mpi_sum,mpi_comm_world,ierr)
      n(:,:,:,:)=work(:,:,:,:)
#endif
      end subroutine Solve_I

!     Solve internal non-LTE 

      subroutine internal(Intensity,x0,y0,z0,nx,ny,nz,x2,y2,z2,
     *                    dnu_nu0,
     *                    Xc,Nray,i0,j0,k0)

      use state_inc   ! module for get_c

      implicit real*8(a-h,o-z)
!      parameter (ng=NG)
      parameter(ixm2=ixm+2,iym2=iym+2,izm2=izm+2,
     *          pi=3.14159265d0,pi4=pi*4d0)
      parameter (Nray0=NRAY0,levmax=LEVMAX)
!      real*8 S(levmax)
      real*8 w(levmax), Intensity(Nray0,levmax), Js(levmax)
      real*8 nx(Nray), ny(Nray), nz(Nray)  ! directions of Rays from boundary to (x0,y0,z0)
      common /molecule/ A(levmax),B(levmax), BG(levmax)

      common /const/ h,lc,bc
      real*8 lc 

!     velocity field
      common /spectrum/ sigma

      common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0, eps,
     *              xcent, ycent, zcent

      real*8 dnu_nu0(Nray)

      real*8 A0(0:levmax,0:levmax),A1(0:levmax,0:levmax),
     *       b0(0:levmax),b1(0:levmax),totC(0:levmax)

      real*8 d(0:levmax), m(0:levmax,0:levmax)

      real*8 nbak(0:levmax)
      real*8,parameter::e_conv=1d-2
      integer,parameter::loopm=100

      dimension x2(Nray),y2(Nray),z2(Nray), dl(Nray0)

      save a0, b0, init

!        data Loopm/1000/
      data init/1/

!      g(J)=2*J+1

!     initialize
      if(init==1) then


!----get C-coefficient depending on temperature ----
!      tk = 20.
!     call get_c(i0-1,j0-1,k0-1)
     
!debug
        do i=0,levmax-1
          do j=0,levmax
!             print*,'L1290 C(i,j) =', C(i,j)
!               print*,'---------'
!                print*, 'L2264: C(i,j) =', i,j, C(i,j)
          enddo
        enddo

!      print*,'max(C), min(C)', maxval(C), minval(C)
!      stop
!       C = 3.5D-11
!debug

        ! maxtrix not depend J
        a0(:,:)=0d0
        totC(:)=0d0

        do i=0,levmax-1
          do j=0,levmax
            totC(i)=totC(i)+C(i,j)
          end do
        end do
!!!        totC(:)=totC(:)*ntot    ! C=C*n(H)
!    test''
        do i=0,levmax-1
          do j=0,levmax
            if(i.ne.j) a0(i,j)=-C(j,i) !!!*ntot    ! n(H)*C
!            print*,i,j,'L1306 a0 = ', C(j,i)
          end do
        end do

       
        do i=0,levmax-1
          a0(i,i)=a0(i,i)+totC(i)
        end do

!        print*, 'a0 = ', a0
!        a0(levmax,0:levmax)=1d0
       
        b0(0:levmax-1)=0d0
!        b0(levmax)=ntot*Xc   ! n(CO)
        b0(levmax)=Xc   ! n(CO)

        init=0
      end if



!     initial alpha, S  => initial J

    ! cic interpolation
      if (2*x0 > (xf(i0)+xf(i0+1))) then
        i=i0+1
      else
        i=i0
      end if
      if (2*y0 > (yf(j0)+yf(j0+1))) then
        j=j0+1
      else
        j=j0
      end if
      if (2*z0 > (zf(k0)+zf(k0+1))) then
        k=k0+1
      else
        k=k0
      end if
      dx=xf(i0+1)-xf(i0)
      dy=yf(j0+1)-yf(j0)
      dz=zf(k0+1)-zf(k0)

      vx0=(x0+dx/2-xf(i))*(y0+dy/2-yf(j))*(z0+dz/2-zf(k))*vx(i,j,k)
     *   +(xf(i)-x0+dx/2)*(y0+dy/2-yf(j))*(z0+dz/2-zf(k))*vx(i-1,j,k)
     *   +(x0+dx/2-xf(i))*(yf(j)-y0+dy/2)*(z0+dz/2-zf(k))*vx(i,j-1,k)
     *   +(xf(i)-x0+dx/2)*(yf(j)-y0+dy/2)*(z0+dz/2-zf(k))*vx(i-1,j-1,k)
     *   +(x0+dx/2-xf(i))*(y0+dy/2-yf(j))*(zf(k)-z0+dz/2)*vx(i,j,k-1)
     *   +(xf(i)-x0+dx/2)*(y0+dy/2-yf(j))*(zf(k)-z0+dz/2)*vx(i-1,j,k-1)
     *   +(x0+dx/2-xf(i))*(yf(j)-y0+dy/2)*(zf(k)-z0+dz/2)*vx(i,j-1,k-1)
     *   +(xf(i)-x0+dx/2)*(yf(j)-y0+dy/2)*(zf(k)-z0+dz/2)
     *               *vx(i-1,j-1,k-1)
      vx0=vx0/(dx*dy*dz)

      vy0=(x0+dx/2-xf(i))*(y0+dy/2-yf(j))*(z0+dz/2-zf(k))*vy(i,j,k)
     *   +(xf(i)-x0+dx/2)*(y0+dy/2-yf(j))*(z0+dz/2-zf(k))*vy(i-1,j,k)
     *   +(x0+dx/2-xf(i))*(yf(j)-y0+dy/2)*(z0+dz/2-zf(k))*vy(i,j-1,k)
     *   +(xf(i)-x0+dx/2)*(yf(j)-y0+dy/2)*(z0+dz/2-zf(k))*vy(i-1,j-1,k)
     *   +(x0+dx/2-xf(i))*(y0+dy/2-yf(j))*(zf(k)-z0+dz/2)*vy(i,j,k-1)
     *   +(xf(i)-x0+dx/2)*(y0+dy/2-yf(j))*(zf(k)-z0+dz/2)*vy(i-1,j,k-1)
     *   +(x0+dx/2-xf(i))*(yf(j)-y0+dy/2)*(zf(k)-z0+dz/2)*vy(i,j-1,k-1)
     *   +(xf(i)-x0+dx/2)*(yf(j)-y0+dy/2)*(zf(k)-z0+dz/2)
     *               *vy(i-1,j-1,k-1)
      vy0=vy0/(dx*dy*dz)

      vz0=(x0+dx/2-xf(i))*(y0+dy/2-yf(j))*(z0+dz/2-zf(k))*vz(i,j,k)
     *   +(xf(i)-x0+dx/2)*(y0+dy/2-yf(j))*(z0+dz/2-zf(k))*vz(i-1,j,k)
     *   +(x0+dx/2-xf(i))*(yf(j)-y0+dy/2)*(z0+dz/2-zf(k))*vz(i,j-1,k)
     *   +(xf(i)-x0+dx/2)*(yf(j)-y0+dy/2)*(z0+dz/2-zf(k))*vz(i-1,j-1,k)
     *   +(x0+dx/2-xf(i))*(y0+dy/2-yf(j))*(zf(k)-z0+dz/2)*vz(i,j,k-1)
     *   +(xf(i)-x0+dx/2)*(y0+dy/2-yf(j))*(zf(k)-z0+dz/2)*vz(i-1,j,k-1)
     *   +(x0+dx/2-xf(i))*(yf(j)-y0+dy/2)*(zf(k)-z0+dz/2)*vz(i,j-1,k-1)
     *   +(xf(i)-x0+dx/2)*(yf(j)-y0+dy/2)*(zf(k)-z0+dz/2)
     *              *vz(i-1,j-1,k-1)
      vz0=vz0/(dx*dy*dz)

      do Nr=1,Nray
 
    ! cic interpolation
        if (2*x2(Nr) > (xf(i0)+xf(i0+1))) then
          i=i0+1
        else
          i=i0
        end if
        if (2*y2(Nr) > (yf(j0)+yf(j0+1))) then
          j=j0+1
        else
          j=j0
        end if
        if (2*z2(Nr) > (zf(k0)+zf(k0+1))) then
          k=k0+1
        else
          k=k0
        end if
        vx2=(x2(Nr)+dx/2-xf(i))*(y2(Nr)+dy/2-yf(j))
     *              *(z2(Nr)+dz/2-zf(k))*vx(i,j,k)
     *     +(xf(i)-x2(Nr)+dx/2)*(y2(Nr)+dy/2-yf(j))
     *              *(z2(Nr)+dz/2-zf(k))*vx(i-1,j,k)
     *     +(x2(Nr)+dx/2-xf(i))*(yf(j)-y2(Nr)+dy/2)
     *              *(z2(Nr)+dz/2-zf(k))*vx(i,j-1,k)
     *     +(xf(i)-x2(Nr)+dx/2)*(yf(j)-y2(Nr)+dy/2)
     *              *(z2(Nr)+dz/2-zf(k))*vx(i-1,j-1,k)
     *     +(x2(Nr)+dx/2-xf(i))*(y2(Nr)+dy/2-yf(j))
     *              *(zf(k)-z2(Nr)+dz/2)*vx(i,j,k-1)
     *     +(xf(i)-x2(Nr)+dx/2)*(y2(Nr)+dy/2-yf(j))
     *              *(zf(k)-z2(Nr)+dz/2)*vx(i-1,j,k-1)
     *     +(x2(Nr)+dx/2-xf(i))*(yf(j)-y2(Nr)+dy/2)
     *              *(zf(k)-z2(Nr)+dz/2)*vx(i,j-1,k-1)
     *     +(xf(i)-x2(Nr)+dx/2)*(yf(j)-y2(Nr)+dy/2)
     *              *(zf(k)-z2(Nr)+dz/2)*vx(i-1,j-1,k-1)
        vx2=vx2/(dx*dy*dz)

        vy2=(x2(Nr)+dx/2-xf(i))*(y2(Nr)+dy/2-yf(j))
     *              *(z2(Nr)+dz/2-zf(k))*vy(i,j,k)
     *     +(xf(i)-x2(Nr)+dx/2)*(y2(Nr)+dy/2-yf(j))
     *              *(z2(Nr)+dz/2-zf(k))*vy(i-1,j,k)
     *     +(x2(Nr)+dx/2-xf(i))*(yf(j)-y2(Nr)+dy/2)
     *              *(z2(Nr)+dz/2-zf(k))*vy(i,j-1,k)
     *     +(xf(i)-x2(Nr)+dx/2)*(yf(j)-y2(Nr)+dy/2)
     *              *(z2(Nr)+dz/2-zf(k))*vy(i-1,j-1,k)
     *     +(x2(Nr)+dx/2-xf(i))*(y2(Nr)+dy/2-yf(j))
     *              *(zf(k)-z2(Nr)+dz/2)*vy(i,j,k-1)
     *     +(xf(i)-x2(Nr)+dx/2)*(y2(Nr)+dy/2-yf(j))
     *              *(zf(k)-z2(Nr)+dz/2)*vy(i-1,j,k-1)
     *     +(x2(Nr)+dx/2-xf(i))*(yf(j)-y2(Nr)+dy/2)
     *              *(zf(k)-z2(Nr)+dz/2)*vy(i,j-1,k-1)
     *     +(xf(i)-x2(Nr)+dx/2)*(yf(j)-y2(Nr)+dy/2)
     *              *(zf(k)-z2(Nr)+dz/2)*vy(i-1,j-1,k-1)
        vy2=vy2/(dx*dy*dz)

        vz2=(x2(Nr)+dx/2-xf(i))*(y2(Nr)+dy/2-yf(j))
     *              *(z2(Nr)+dz/2-zf(k))*vz(i,j,k)
     *     +(xf(i)-x2(Nr)+dx/2)*(y2(Nr)+dy/2-yf(j))
     *              *(z2(Nr)+dz/2-zf(k))*vz(i-1,j,k)
     *     +(x2(Nr)+dx/2-xf(i))*(yf(j)-y2(Nr)+dy/2)
     *              *(z2(Nr)+dz/2-zf(k))*vz(i,j-1,k)
     *     +(xf(i)-x2(Nr)+dx/2)*(yf(j)-y2(Nr)+dy/2)
     *              *(z2(Nr)+dz/2-zf(k))*vz(i-1,j-1,k)
     *     +(x2(Nr)+dx/2-xf(i))*(y2(Nr)+dy/2-yf(j))
     *              *(zf(k)-z2(Nr)+dz/2)*vz(i,j,k-1)
     *     +(xf(i)-x2(Nr)+dx/2)*(y2(Nr)+dy/2-yf(j))
     *              *(zf(k)-z2(Nr)+dz/2)*vz(i-1,j,k-1)
     *     +(x2(Nr)+dx/2-xf(i))*(yf(j)-y2(Nr)+dy/2)
     *              *(zf(k)-z2(Nr)+dz/2)*vz(i,j-1,k-1)
     *     +(xf(i)-x2(Nr)+dx/2)*(yf(j)-y2(Nr)+dy/2)
     *              *(zf(k)-z2(Nr)+dz/2)*vz(i-1,j-1,k-1)
        vz2=vz2/(dx*dy*dz)

!       calculate dl
        dl(Nr)=sqrt((x2(Nr)-x0)**2
     *             +(y2(Nr)-y0)**2
     *             +(z2(Nr)-z0)**2)
        df=dnu_nu0(Nr)+((vx2-vx0)*nx(Nr)
     *                 +(vy2-vy0)*ny(Nr)
     *                 +(vz2-vz0)*nz(Nr))/lc
        phi=exp(-df**2/2/sigma**2)/sqrt(2d0*pi)/sigma
        dl(Nr)=dl(Nr)*phi*dx0

      end do

      do loop=1,loopm

        do lev=1,levmax
          w(lev)=0d0
          do Nr=1,Nray
            tau=alpha(lev, i0,j0,k0)*dl(Nr)
            w(lev)=w(lev)+Intensity(Nr,lev)*exp(-tau)
     *               +S(lev, i0,j0,k0)*(1d0-exp(-tau))
          end do
          Js(lev)=w(lev)/Nray
        end do
!       write(*,*) i0,j0,k0,Js

        a1(:,:)=a0(:,:)*ntot(i0,j0,k0)     ! C=C*n(H)
!        print*, a0
        a1(levmax,0:levmax)=1d0
        b1(:)=b0(:)*ntot(i0,j0,k0)         ! B(levmax)=XCO*n(H)
  
        do i=1,levmax-1
          a1(i,i)=a1(i,i)+A(i)
        end do
        do i=0,levmax-1
          a1(i,i+1)=a1(i,i+1)-A(i+1)
        end do
  
        do i=0,levmax-1
          a1(i,i)=a1(i,i)+g(i+1)/g(i)*B(i+1)*Js(i+1)
        end do
        do i=1,levmax-1
          a1(i,i)=a1(i,i)+B(i)*Js(i)
        end do
        do i=0, levmax-1
          a1(i,i+1)=a1(i,i+1)-B(i+1)*Js(i+1)
        end do
        do i=1,levmax-1
          a1(i,i-1)=a1(i,i-1)-g(i)/g(i-1)*B(i)*Js(i)
        end do


!       Here I will write Gauss Elimination 

!        nbak(:)=n(:)
       do k = 0, levmax
        nbak(k) = n(k, i0, j0, k0)
       enddo

       do k=0,levmax-1
          d(k)=1d0/a1(k,k)
          do i=k+1,levmax
            m(i,k)=a1(i,k)*d(k)
            do j=k+1,levmax
              a1(i,j)=a1(i,j)-m(i,k)*a1(k,j)
            end do
            b1(i)=b1(i)-m(i,k)*b1(k)
          end do
        end do
        d(levmax)=1d0/a1(levmax,levmax)

        n(levmax, i0,j0,k0)=b1(levmax)*d(levmax)
        do k=levmax-1,0,-1
          n(k,i0, j0, k0)=b1(k)
          do i=k,levmax-1
            n(k,i0,j0,k0)=n(k,i0,j0,k0)-a1(k,i+1)*n(i+1,i0,j0,k0)
          end do
          n(k,i0,j0,k0)=n(k,i0,j0,k0)*d(k)
        end do

!******
        do lev=1,levmax
          alpha(lev, i0,j0,k0)=h/pi4 
     *     *(n(lev-1,i0,j0,k0)*g(lev)/g(lev-1)-n(lev,i0,j0,k0))*B(lev)

!debug
!          if(alpha(lev, i0,j0,k0) < 0)then
!          print*,lev,'<internal> alpha',i0, j0, k0,alpha(lev, i0,j0,k0)
!          endif

!           write(*,'(a19, 4i2, g12.3)') 
!     &      'line 1491, alpha = ', lev, i0, j0, k0, alpha(lev)
!debug
          S(lev,i0,j0,k0)=n(lev,i0,j0,k0) *A(lev)
     *    /(n(lev-1,i0,j0,k0)*g(lev)/g(lev-1)-n(lev,i0,j0,k0))/B(lev)
        end do
!******
        if (loop > 1) then
          diff=0d0
          do k=0,levmax

!          if (nbak(k)==0d0) then
!            write(*,*) k,nbak(k)
!          else
             diff=max(diff,abs(n(k,i0,j0,k0)-nbak(k))
     &            /nbak(k))
!          end if
          end do
          if (diff < e_conv) return
        end if
      end do
      end subroutine internal


!     Slave Solve_I_0

      subroutine Solve_I_0(Intensity,nx,ny,nz,x1,y1,z1,
     *                     x2,y2,z2,dnu_nu0,
     *                     Nray,i0,j0,k0)
      use state_inc
      implicit real*8(a-h,o-z)

      parameter (Nray0=NRAY0,Lmax=ng*3,levmax=LEVMAX,
     *           ixm2=ixm+2,iym2=iym+2,izm2=izm+2,
     *           pi=3.14159265d0)
      common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0, eps,
     *              xcent, ycent, zcent
      common /const/ h,lc,bc
      real*8 lc 
!     velocity field
      common /spectrum/ sigma

      real*8 dnu_nu0(Nray), phi(Nray0)

      real*8 nx(Nray), ny(Nray), nz(Nray)  ! directions of Rays from boundary to (x0,y0,z0)
      real*8 Intensity(Nray0,levmax), dl(Nray0)
      dimension x1(Nray), y1(Nray), z1(Nray)  ! initial position on the boundary
      dimension x2(Nray), y2(Nray), z2(Nray) ! final position on the cell boundary
!      dimension S(levmax,ixm,iym,izm)

      dimension x(Nray0),y(Nray0),z(Nray0), iflag(Nray0)
      dimension ix(Nray0), iy(Nray0), iz(Nray0)
      dimension ix0(Nray0), iy0(Nray0), iz0(Nray0)

      integer :: iray

      vx0=vx(i0,j0,k0)
      vy0=vy(i0,j0,k0)
      vz0=vz(i0,j0,k0)

!     copy initial position.  to avoid changing x1, y1, and z1
!      print*,'<solve_i_0> max, min alpha', maxval(alpha), minval(alpha)

      x(1:Nray)=x1(1:Nray)        
      y(1:Nray)=y1(1:Nray)        
      z(1:Nray)=z1(1:Nray)
      iflag(1:Nray)=0              !=1 reach the cell; =0 not yet

!     find cell number

      do iray=1,Nray
!       isigx=int(nx(iray)+1d0)
!       ix(iray)=isigx*int(x(iray)-xf(0))
!    *       +(1-isigx)*(ixm-int(xf(ixm1)-x(iray)))
!       isigy=int(ny(iray)+1d0)
!       iy(iray)=isigy*int(y(iray)-yf(0))
!    *       +(1-isigy)*(iym-int(yf(iym1)-y(iray)))
!       isigz=int(nz(iray)+1d0)
!       iz(iray)=isigz*int(z(iray)-zf(0))
!    *       +(1-isigz)*(izm-int(zf(izm1)-z(iray)))

        if(nx(iray) > 0)then
          ix(iray) = int(x(iray)-xf(0))
        else
          ix(iray) = (ixm-int(xf(ixm1)-x(iray)))
        endif
        if(ny(iray) > 0)then
          iy(iray) = int(y(iray)-yf(0))
        else
          iy(iray) = (iym-int(yf(iym1)-y(iray)))
        endif
        if(nz(iray) > 0)then
          iz(iray) = int(z(iray)-zf(0))
        else
          iz(iray) = (izm-int(zf(izm1)-z(iray)))
        endif

!       if(i0==2.and.j0==7.and.k0==1) then
        if(iz(iray) .gt. ng)then
         print*,'<solve_i_0> iz > ng Nray: ',iray,':',i0,j0,k0,':',iz(iray)
        endif
      end do


      do L=1,Lmax

!     check whether the present position = (x2,y2,z2) ?
        iflag0=1
        do iray =1, Nray
          if((x(iray)-x2(iray))**2
     *      +(y(iray)-y2(iray))**2
     *      +(z(iray)-z2(iray))**2  < eps**2) then
            iflag(iray)=1
          else
            iflag0=0
          end if
        end do
        if (iflag0==1) then
          goto 1000
        end if

!     Next crossing


        do iray =1,Nray

!@        isigx=int(nx(iray)+1d0)
  	  if(nx(iray) >0)then
		isigx = 1
	  else
		isigx = 0
	  endif
          tx=(xf(ix(iray)+isigx)-x(iray))/nx(iray)

!@         isigy=int(ny(iray)+1d0)
	  if(ny(iray) >0)then
		isigy = 1
	  else
		isigy = 0
	  endif
          ty=(yf(iy(iray)+isigy)-y(iray))/ny(iray)


!@          isigz=int(nz(iray)+1d0)
	  if(nz(iray) >0)then
		isigz = 1
	  else
		isigz = 0
	  endif
          tz=(zf(iz(iray)+isigz)-z(iray))/nz(iray)

          if(abs(tx) > 1.e4) tx=1.e4
          if(abs(ty) > 1.e4) ty=1.e4
!         if(abs(tz) > 1.e4) tz=1.e4

          dl(iray)=min(tx,ty,tz)
	  if(dl(iray) < 0)then
	  print*,'<solve_i_0> dl < 0', iray, ty,':',
     &           yf(iy(iray)+isigy),y(iray),ny(iray)
	  print*,'<solve_i_0> tx,ty,tz:', tx, ty,tz
	  endif
!@        ixmin=int(dl(iray)/tx)
!@        iymin=int(dl(iray)/ty)
!@        izmin=int(dl(iray)/tz)
	if(dl(iray) < tx)then
		ixmin = 0
	else
		ixmin = 1
	endif
	if(dl(iray) < ty)then
		iymin = 0
	else
		iymin = 1
	endif
	if(dl(iray) < tz)then
		izmin = 0
	else
		izmin = 1
	endif
!          if(ixmin+iymin+izmin.ge.2) write(*,*) ixmin,iynin,izmin
!          if(iflag(iray).eq.0.and.dl(iray).le.0d0)then
!            write(*,*) N,i0,j0,k0,isigx,tx,isigy,ty,isigz,tz
!            write(*,*) x1(iray),y1(iray),z1(iray),x2(iray),y2(iray),z2(iray)
!            write(*,*) nx(iray),ny(iray),nz(iray),ix(iray),iy(iray),iz(iray)
!          end if

!         Here, (iymin+izmin) --> (1-ixmin) 

          x(iray)=iflag(iray)*x(iray)
     *        +(1-iflag(iray))*(
     *            ixmin*xf(ix(iray)+isigx)
     *            +(1-ixmin)*(x(iray)+nx(iray)*dl(iray)) )

          y(iray)=iflag(iray)*y(iray)
     *        +(1-iflag(iray))*(
     *            iymin*yf(iy(iray)+isigy)
     *            +(1-iymin)*(y(iray)+ny(iray)*dl(iray)) )

          z(iray)=iflag(iray)*z(iray)
     *        +(1-iflag(iray))*(
     *            izmin*zf(iz(iray)+isigz)
     *            +(1-izmin)*(z(iray)+nz(iray)*dl(iray)) )
      
!         if x(iray)+nx(iray)*dl(iray))=xf(ix(iray)+isigx) due to error,
!         this gives incorrect set of (x,ix)

!          ix0(iray)=iflag(iray)*ix(iray)
!     *        +(1-iflag(iray))*(
!     *            ixmin*(ix(iray)+2*isigx-1)
!     *            +(1-ixmin)*ix(iray))
          ix0(iray)=iflag(iray)*ix(iray)
     *          +(1-iflag(iray))*(
     *            isigx*int(x(iray)-xf(0))
     *            +(1-isigx)*(ixm-int(xf(ixm1)-x(iray))))

!          iy0(iray)=iflag(iray)*iy(iray)
!     *        +(1-iflag(iray))*(
!     *            iymin*(iy(iray)+2*isigy-1)
!     *            +(1-iymin)*iy(iray))
          iy0(iray)=iflag(iray)*iy(iray)
     *          +(1-iflag(iray))*(
     *            isigy*int(y(iray)-yf(0))
     *            +(1-isigy)*(iym-int(yf(iym1)-y(iray))))

!          iz0(iray)=iflag(iray)*iz(iray)
!     *        +(1-iflag(iray))*(
!     *            izmin*(iz(iray)+2*isigz-1)
!     *            +(1-izmin)*iz(iray))
          iz0(iray)=iflag(iray)*iz(iray)
     *          +(1-iflag(iray))*(
     *            isigz*int(z(iray)-zf(0))
     *            +(1-isigz)*(izm-int(zf(izm1)-z(iray))))

          df=dnu_nu0(iray)
     *               +((vx(ix(iray),iy(iray),iz(iray))-vx0)*nx(iray)
     *               +(vy(ix(iray),iy(iray),iz(iray))-vy0)*ny(iray)
     *               +(vz(ix(iray),iy(iray),iz(iray))-vz0)*nz(iray))/lc
          phi(iray)=exp(-df**2/2/sigma**2)/sqrt(2d0*pi)/sigma
        end do


!debug
!       print*,i0, j0, k0,'<solve_i_0> alpha(1,2,2,1) = ', alpha(1,2,2,1)
!debug


!!        do lev=1,levmax
        do iray =1,Nray

!          tau=alpha(lev,ix(iray),iy(iray),iz(iray))*(dl(iray)*dx0)*phi(iray)
!                                               ! alpha==alpha/nu0
!          Intensity(N,lev)=
!     *        iflag(iray)*Intensity(N,lev)
!     *       +(1-iflag(iray))*(Intensity(N,lev)*exp(-tau)
!     *                     +S(lev,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))
!debug
!           print*, l, n,'<solve_i_0> alpha(1,2,2,1) = ', alpha(1,2,2,1)
!debug
          tau=alpha(1,ix(iray),iy(iray),iz(iray))
     &          *(dl(iray)*dx0)*phi(iray)
                                                 ! alpha==alpha/nu0
	  if(tau < 0) then
!	   print*,'<solve_i_0> tau <0 ix,iy,iz ',
!     &      ix(iray),iy(iray),iz(iray),
!     &       alpha(1,ix(iray),iy(iray),iz(iray))
!     &     ix(n),iy(n),iz(n), dl(n), phi(n), alpha(1,ix(iray),iy(iray),iz(iray))
!           stop
	  endif


          Intensity(iray,1)=
     *        iflag(iray)*Intensity(iray,1)
     *       +(1-iflag(iray))*(Intensity(iray,1)*exp(-tau)
     *         +S(1,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))

          tau=alpha(2,ix(iray),iy(iray),iz(iray))
     &        *(dl(iray)*dx0)*phi(iray)
                                               ! alpha==alpha/nu0
          Intensity(iray,2)=
     *        iflag(iray)*Intensity(iray,2)
     *       +(1-iflag(iray))*(Intensity(iray,2)*exp(-tau)
     *        +S(2,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))

          tau=alpha(3,ix(iray),iy(iray),iz(iray))
     &           *(dl(iray)*dx0)*phi(iray)
                                               ! alpha==alpha/nu0
          Intensity(iray,3)=
     *        iflag(iray)*Intensity(iray,3)
     *       +(1-iflag(iray))*(Intensity(iray,3)*exp(-tau)
     *            +S(3,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))

          tau=alpha(4,ix(iray),iy(iray),iz(iray))
     &           *(dl(iray)*dx0)*phi(iray)
                                                 ! alpha==alpha/nu0
          Intensity(iray,4)=
     *        iflag(iray)*Intensity(iray,4)
     *       +(1-iflag(iray))*(Intensity(iray,4)*exp(-tau)
     *            +S(4,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))

          tau=alpha(5,ix(iray),iy(iray),iz(iray))
     &            *(dl(iray)*dx0)*phi(iray)
                                               ! alpha==alpha/nu0
          Intensity(iray,5)=
     *        iflag(iray)*Intensity(iray,5)
     *       +(1-iflag(iray))*(Intensity(iray,5)*exp(-tau)
     *           +S(5,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))


          tau=alpha(6,ix(iray),iy(iray),iz(iray))
     &            *(dl(iray)*dx0)*phi(iray)
                                               ! alpha==alpha/nu0
          Intensity(iray,6)=
     *        iflag(iray)*Intensity(iray,6)
     *       +(1-iflag(iray))*(Intensity(iray,6)*exp(-tau)
     *           +S(6,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))

          tau=alpha(7,ix(iray),iy(iray),iz(iray))
     &            *(dl(iray)*dx0)*phi(iray)
                                               ! alpha==alpha/nu0
          Intensity(iray,7)=
     *        iflag(iray)*Intensity(iray,7)
     *       +(1-iflag(iray))*(Intensity(iray,7)*exp(-tau)
     *           +S(7,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))

          tau=alpha(8,ix(iray),iy(iray),iz(iray))
     &            *(dl(iray)*dx0)*phi(iray)
                                               ! alpha==alpha/nu0
          Intensity(iray,8)=
     *        iflag(iray)*Intensity(iray,8)
     *       +(1-iflag(iray))*(Intensity(iray,8)*exp(-tau)
     *           +S(8,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))

          tau=alpha(9,ix(iray),iy(iray),iz(iray))
     &            *(dl(iray)*dx0)*phi(iray)
                                               ! alpha==alpha/nu0
          Intensity(iray,9)=
     *        iflag(iray)*Intensity(iray,9)
     *       +(1-iflag(iray))*(Intensity(iray,9)*exp(-tau)
     *           +S(9,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))


          tau=alpha(10,ix(iray),iy(iray),iz(iray))
     &            *(dl(iray)*dx0)*phi(iray)
                                               ! alpha==alpha/nu0
          Intensity(iray,10)=
     *        iflag(iray)*Intensity(iray,10)
     *       +(1-iflag(iray))*(Intensity(iray,10)*exp(-tau)
     *           +S(10,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))

!!          end do
        end do
        ix(1:Nray)=ix0(1:Nray)
        iy(1:Nray)=iy0(1:Nray)
        iz(1:Nray)=iz0(1:Nray)
      end do
      write(*,*)'error in solve I'
      stop
 1000 return

      end subroutine solve_i_0





      subroutine Temperature(Loop,dif)
      use Rays
      use state_inc
      implicit real*8(a-h,o-z)
      parameter(Nray0=NRAY0,levmax=LEVMAX,   
     *          ixm2=ixm+2,iym2=iym+2,izm2=izm+2,pc=3.0857d18)

!      common /Rays/ x0,y0,z0,nx,ny,nz,x1,y1,z1,x2,y2,z2,dnu_nu0,Nray
!     points where Js are calculated
!      real*8 x0(ixm,iym,izm), y0(ixm,iym,izm), z0(ixm,iym,izm)  ! we need these values. 
!     directions of Rays from boundary to (x0,y0,z0)
!      real*8 nx(Nray0,ixm,iym,izm), ny(Nray0,ixm,iym,izm),
!     *       nz(Nray0,ixm,iym,izm), dnu_nu0(Nray0,ixm,iym,izm)
!     initial position on the boundary
!      dimension x1(Nray0,ixm,iym,izm), y1(Nray0,ixm,iym,izm), 
!     *          z1(Nray0,ixm,iym,izm)
!     final position on the cell boundary
!      dimension x2(Nray0,ixm,iym,izm), y2(Nray0,ixm,iym,izm),
!     *          z2(Nray0,ixm,iym,izm)

!     velocity
      common /spectrum/ sigma
 
      dimension dif(0:levmax)

      common /const/ h,lc,bc
      real*8 lc 

      common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0, eps,
     *              xcent, ycent, zcent

      common /density/ Xc
      real*8  Xc

      common /molecule/ A(levmax),B(levmax),BG(levmax)
!     *                  A(levmax),B(levmax),C(0:levmax,0:levmax),
      real*8  Tex(1:levmax,ixm,iym,izm)          ! T(J+1->J)

	character(19) :: file_name_norm
	character(19) :: file_name_nlev
	character(19) :: file_name_disp
	character(19) :: file_name_te


!     Alpha and S
!      real*8 S(levmax,ixm,iym,izm)
!      common /alphas/ S

      character(2) :: num
      character(3) :: loop_num

      integer init,final
      data  init/1/,final/49/
      save  init,final

!      g(J)=2*J+1

      if (init == 1) then
        do lev=0,levmax
          write(num,'(i2.2)') lev
       file_name_norm = "data3/"//"norm"//num//".d"
       open(lev+50,file=file_name_norm,
     & status='unknown', form='formatted')
        end do
!       final output
!	file_name_nlev = "data3/"//"nlev.d"
!        open(final,file=file_name_nlev,status='unknown',
!     *          form='unformatted')
        init=0
      end if

      write(loop_num,'(i3.3)')Loop

      if(mod(Loop,1) == 0) then
	file_name_nlev = "data3/"//loop_num//"nlev.d"
        open(final,file=file_name_nlev,status='unknown',
     *          form='unformatted')

        do lev=1,levmax
          write(num,'(i2.2)') lev
!         open(lev+10,file='data3/'//loop_num//'Tr_' //num//'.d',
!    *                status='unknown',form='unformatted')
        end do
        do lev=1,levmax
          write(num,'(i2.2)') lev
          file_name_disp = "data3/"//loop_num//"Disp_" //num//".d" 
          open(lev+60,file=file_name_disp,
     *                status='unknown',form='formatted')
        end do

!       local temperature
        do lev=1,levmax
          do k=1,izm
            do j=1,iym
              do i=1,ixm
!                write(*,*)i,j,k,lev,n(lev,i,j,k),n(lev-1,i,j,k)
                Tex(lev,i,j,k)=-(e(lev)-e(lev-1))
     *              /(bc*log(abs(n(lev,i,j,k)/n(lev-1,i,j,k))
     *                             *g(lev-1)/g(lev)))
              end do
            end do
          end do
        end do

!       do lev=1,levmax
!         do k=1,izm
!           do j=1,iym
!             do i=1,ixm
!               r=sqrt(x0(i,j,k)**2+y0(i,j,k)**2+z0(i,j,k)**2)
!               write(10+lev,*)r*dx0/pc,Tex(lev,i,j,k)
!             end do
!           end do
!         end do
!          ---- output binary data (KW) --------------------
	  file_name_te = "data3/"//loop_num//"Te_" //num//".data"
          open(10,file=file_name_te,
     *                status='unknown',form='unformatted')
             write(10) Tex
          close(10)

!         close(10+lev)
!       end do

!       dispersion
   
        mmax=ixm
        do m=1,mmax
          do lev=1,levmax
            sum1=0d0
            sum2=0d0
            nsum=0
            do k=1,izm
            do j=1,iym
            do i=1,ixm
              w=x0(i,j,k)**2+y0(i,j,k)**2+z0(i,j,k)**2
              if (w >= (1d0*m-0.5d0)**2 
     *                  .and. w < (1d0*m+0.5d0)**2) then
                nsum=nsum+1
                sum1=sum1+Tex(lev,i,j,k)
                sum2=sum2+Tex(lev,i,j,k)**2
              end if
            end do
            end do
            end do
            if (nsum==0) then
              disp=0d0
            else
              disp=sqrt(abs(sum2/nsum-(sum1/nsum)**2))
            end if
            write(lev+60,'(1p2e10.3)')m*dx0/pc,disp
          end do
        end do

        do lev=1,levmax
          close(lev+60)
        end do 

      end if

!     write down progress in one-step every time step
 
      do lev=0,levmax
        write (lev+50,'(i3,1p10e10.3)') Loop,dif(lev)
        write (*,'(i3,i3,1pe10.3)') Loop,lev,dif(lev)
!        call flush(lev+50)
      end do
      
      write(*,*) loop
      return

      entry final_output()
      write(final) ng,levmax,Nray
      write(final) xf,yf,zf,dx0
      write(final) vx,vy,vz,sigma
      write(final) n
      write(final) alpha,S
!     from ver6.1
      write(final) BG
      close(final)
      return
      end

       
#ifdef ALPHA
       SUBROUTINE NRAND(N, X, IR)
************************************************************************
* NORMAL RANDOM NUMBER GENERATOR (BOX-MULLER METHOD)                   *
* PARAMETERS                                                           *
*   (1) N      (I) THE NUMBER OF RANDOM NUMBERS TO BE GENERATED        *
*                  (INPUT)                                             *
*   (2) X      (D) NORMAL RANDOM NUMBERS (OUTPUT)                      *
*   (3) IR     (I) THE INITIAL SEED FOR UNIFORM RANDOM NUMBER GENERATOR*
*                  (INPUT)                                             *
*                  THE SEED FOR THE NEXT CALL (OUTPUT)                 *
* COPYRIGHT: Y. OYANAGI, JUNE 30, 1989  V.1                            *
************************************************************************
       DOUBLE PRECISION X(N), R, T, PI2
       PARAMETER(PI2=2.0*3.14159265358979D0)
*PAREMETER CHECK
      NN = N
      IF( NN .LE. 0) THEN
       WRITE(6,*) '(SUBR.NRAND) PARAMETER ERROR. N = ', NN
       WRITE(6,*) 'RETURN WITH NO FURTHER CALCULATION.'
       RETURN
      ELSE IF( MOD(N, 2) .NE. 0) THEN
       WRITE(6,*) '(SUBR.NRAND) WARNING. N IS ODD'
       NN=N+1
      END IF
*MAIN LOOP
      CALL URAND1(NN, X, IR)
      DO 10 I = 1, NN, 2
       R=SQRT(-2.0D0*LOG(X(I)))
       T=PI2*X(I+1)
       X(I  ) = R * SIN(T)
       X(I+1) = R * COS(T)
   10 CONTINUE
      RETURN
      END
      SUBROUTINE URAND1(N, X, IR)
************************************************************************
* UNIFORM RANDOM NUMBER GENERATOR (MIXED CONGRUENTIAL METHOD)          *
*     PORTABLE BUT SLOW.  THE PERIOD IS ONLY 1664501.                  *
* PARAMETERS                                                           *
*   (1) N      (I) THE NUMBER OF RANDOM NUMBERS TO BE GENERATED        *
*                  (INPUT)                                             *
*   (2) X      (D) UNIFORM RANDOM NUMBERS (OUTPUT)                     *
*   (3) IR     (I) THE INITIAL SEED  (INPUT)                           *
*                  THE SEED FOR THE NEXT CALL (OUTPUT)                 *
* COPYRIGHT: Y. OYANAGI, JUNE 30, 1989  V.1                            *
************************************************************************
*
       DOUBLE PRECISION X(N), INVM
       PARAMETER (M = 1664501, LAMBDA = 1229, MU = 351750)
       PARAMETER (INVM = 1.0D0 / M)
*PAREMETER CHECK
      IF( N .LE. 0) THEN
       WRITE(6,*) '(SUBR.URAND1) PARAMETER ERROR. N = ', N
       WRITE(6,*) 'RETURN WITH NO FURTHER CALCULATION.'
       RETURN
      END IF
      IF( IR .LT. 0 .OR. IR .GE. M) THEN
       WRITE(6,*) '(SUBR.URAND1) WARNING. IR = ', IR
      END IF
*MAIN LOOP
      DO 10 I = 1, N
       IR = MOD( LAMBDA * IR + MU, M)
       X(I) = IR * INVM
   10 CONTINUE
      RETURN
      END
#endif

#ifdef MPI
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
#endif      

!-----------------------------------------------------
      subroutine get_c_0

      use state_inc


      implicit none

      Double Precision    xb,yb,zb

      Integer*4 ix,iy,iz

      Integer*4 i,j,k,l

      Character(16) ::   filename
      Real*8 tempdble
      Real*8 tempdblearray(0:NLEVMAX-1)

      Integer levels_in_table,number_of_tables
      Character(64) :: molecule

      Real*8    tlow,thigh,q
      Integer*4 tlowindex,thighindex
	real*8 :: xc

!--------------------------------------------------

!     filename = 'CO-H2para.dat'
      filename = CCOEFF
      Open(Unit=10,File=filename,Status='OLD')

      Read (10,*) molecule
      Write(*,*) molecule
      Read (10,*) XC
      Read (10,*) MU
      Read (10,*) BB
      Write(*,*) 'Abundance              = ',XC
      Write(*,*) 'Electric Dipole Moment = ',MU
      Write(*,*) 'Rotational Constant    = ',BB
      Read (10,*) levels_in_table
      Read (10,*) number_of_tables

      If (levels_in_table.ne.NLEVMAX) Then
         write(*,*) "Maximum level is incorrect!!"
         write(*,*) "Set NLEVMAX = ",levels_in_table, 
     &     " (in parameter.inc ) and recompile the program."
         write(*,*) levels_in_table,NLEVMAX
         Stop
      Endif
      If (number_of_tables.ne.TABLEMAX) Then
         write(*,*) "The number of Level data is inconsistent!!"
         write(*,*) "Set TABLEMAX = ",number_of_tables,
     &  " and recompile the program."
         Stop
      Endif
      Read(10,*)

	print*,'tablemax, tablemin: ', TABLEMIN,TABLEMAX

      Do l=TABLEMIN,TABLEMAX

         Read(10,*) tklist(l)
         Read(10,*)

         Do j=0,NLEVMAX-1
            Read(10,*) (tempdblearray(i),i=0,NLEVMAX-1)
            Do k = 0,NLEVMAX-1
!	       print*,'tempdblearray = ', j,k, tempdblearray(k)
               CTABLE(l,k,j) = tempdblearray(k)
            End do
         End do

      End do

      Close(Unit=10)

!--- for test ----------
!do iz = 0,izmax
!do iy = 0,iymax
!do ix = 0,ixmax
! tk(ix,iy,iz) = ix*iy*iz/3 + 20.
!enddo
!enddo
!enddo
!print*, 'ixmax, iymax, izmax: ', ixmax, iymax, izmax
!print*, ' nl(ns) ', nl(ns)
!do l=TABLEMIN,TABLEMAX
!Do j= 0, NL(NS)
!Do k= 0, NL(NS)
! 	print*,l,k,j, ctable(l,k,j)
!enddo
!enddo
!enddo
      end subroutine get_c_0
!#############################################################
!     calculating C coeeficients from a table

      subroutine get_c(ii,jj,kk)

      use state_inc
      implicit none

      integer :: ii, jj, kk, i,j, tlowindex, thighindex
      real :: tlow, thigh
      real*8 :: q
      integer, parameter :: levmax =5

!--- Calculate Collisional Rate from tk(ix,iy,iz). -> C(NS,i,j)
      
               If (tk(ii,jj,kk).gt.(TABLEMAX*10.0D0)) Then
                  write(*,*) "Kinetic temperature is too high!!"
                  Stop
               Endif

               If (tk(ii,jj,kk).lt.5.0D0) Then
                  write(*,*) "Kinetic temperature is too low!!"
                  print*, ii, jj, kk, tk(ii, jj, kk)
                  Stop
               Endif

               tlowindex  = Int(tk(ii,jj,kk)*0.1D0)
               thighindex = tlowindex+1

               tlow  =  tlowindex*10.0
               thigh = thighindex*10.0
      
               If (tlow.eq.0.0D0) tlow = 5.0
      

!---- Log Linier Interpolation
!               Do i = 0,NL(NS)
!                  C(NS,i,i) = 0.0D0
!                  C(i,i) = 0.0D0
!               End do
               c = 0.0D0

               q = (Log(tk(ii,jj,kk))-Log(tlow))/(Log(thigh)-Log(tlow))
!               do i = 1, nlevmax
               do i = 1, levmax
               do j = 0,i-1
!             C(NS,i,j) = Exp( Log(CTABLE(tlowindex ,i,j))*(1.0D0-q) &
!                                +Log(CTABLE(thighindex,i,j))*q)
                C(i,j) = Exp( Log(CTABLE(tlowindex ,i,j))*(1.0D0-q) 
     &                          +Log(CTABLE(thighindex,i,j))*q)


!                print*, 'L2264: C(i,j) =', i,j, C(i,j)
!                 if(i.eq.1 .and. kk.eq.0)then
!                 print*,'L2064: C(i,j) =', i,j, C(i,j)
!                 endif

               enddo
               enddo


!               Do j = 1, nlevmax
                Do j = 1, levmax
                  Do i = 0,j-1
!                     C(NS,i,j) = C(NS,j,i)*GSTAT(j)/GSTAT(i)*  &
!                              exp(-(elev(j)-elev(i))/(KB*tk(i0,j0,k0)))
                     C(i,j) = C(j,i)*G(j)/G(i)*  
     &                 exp(-(e(j)-e(i))/(KB*tk(ii,jj,kk)))

!                if(c(i,j) .eq. 0.)then
!                 print*,'L2079: C(i,j) =', i,j, C(i,j)
!                endif

                  End do
               End do

               
!!!! debug
!               print*,'C(3,4), C(4,3) ', C(3,4), C(4,3), e(4), e(3)
!               print*,'C(1,2), C(2,1) ', C(1,2), C(2,1), e(2), e(1)

!               stop

!if(mod(ix,2).eq.0 .and. mod(iy,2).eq.0 .and. mod(iz,2).eq.0)then

!               do i = 0, levmax
!                  do j = 0,i-1
!                  do j = 0,levmax
!                     print*, tk(ii,jj,kk), 'c(i,j)', i,j, c(i,j)
!  print*, i, j, tk(ix,iy,iz), c(ns,i,j)
!  print*, tk(ix,iy,iz), c(ns,1,0)
!                  enddo
!               enddo
!endif


!---- The end of C(ix,iy,iz) calculation
               end subroutine get_c

!****************************** GAMMA DEVIATES GENERATOR ***** P 206 










