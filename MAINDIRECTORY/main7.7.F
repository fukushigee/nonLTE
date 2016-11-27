#ifdef VPP
#define NG 64 
#define NGZ 32 
#define INFILENAME 'agn_input2.data'
#define NRAY 100
#define LOOPMAX 10
#define MOLECULE 'CO.d'
#define CCOEFF 'CO-H2para.dat'
#endif
!#--- when run on octa, modify parameters below. (MY) ----#
#ifdef ALPHA
#define NG 64
#define NGZ 64
!#define INFILENAME '~/phys/Machida/rad_data/st48832_l7.dat'
!#define INFILENAME '~/phys/Machida/rad_data/st48832_l7_10K.dat'
!#define INFILENAME '/home/wada/2005paper/nonLTE_AGN/agn_input3.data'
!#define INFILENAME '/home/wada/2005paper/nonLTE_AGN/agn_input128.data'
!#define INFILENAME '~/phys/RT_bench/HYDRO_DATA/3d2lev.dat'
#define INFILENAME 'agn_input3.data'
!#define DATADIR 'data3'
#define NRAY 1500
#define LOOPMAX 280
!#define CCOEFF 'CO12_test.dat'
!#define CCOEFF 'CO13_test.dat'
!#define CCOEFF 'HCN.dat'
!#define CCOEFF 'HCO+.dat'
!#define CCOEFF 'HCO+_2lev.dat'
#define CCOEFF 'HCN_test.dat'
!#define CCOEFF 'HCO+_test.dat'
!#define CCOEFF 'SiO_reform.dat'
!#define CCOEFF 'HNC.dat'
#endif
#define NRAY0 1500
#define LEVMAX 1
#define TABLEMIN 1
!# TABLEMAX = 15(HCO+) 14(HCN, HNC, CO12, CO13) 13(SiO, reform)
#define TABLEMAX 15
!# NLEVMAX = 40(CO12, CO13, SiO), 29(HCN, HNC), 30(HCO+)
#define NLEVMAX 30
! upperlimit of masing intensity
#define JLIM 1.d-1
! avoid exp(tau) (default:600)
#define BIGTAU -600.d0
! initial condition (0 for LTE, 1 for popini)
#define INI 0
! micro turbulence [cm/sec]
#define VTB 1.d5
!#define VTB 20.d5
!#define VTB 2.d-5
!! grid size [cm]
!#define DX0 3.5787489605d2*au
!#define DX0 16.00409*au 
!#define DX0 64*pc/ng
#define DX0 0.1*pc/(ng/2)
!!-- background radiation temperature (default: CMB, 2.7d0 --!!
#define TBG 1.d-30
!# when use scalar sigma (torus), flag 0
#define SIG 0
!#--- end of parameter setting part (MY) ----#

!-------------------------------------------------------------!
      module Rays
      real*8, save,allocatable :: 
     *        x0(:,:,:), y0(:,:,:), z0(:,:,:)
      real*8, save,allocatable :: 
     *        nx(:,:,:,:), ny(:,:,:,:),
     *        nz(:,:,:,:), dnu_nu0(:,:,:,:)
      real*8, save,allocatable :: 
     *        x1(:,:,:,:), y1(:,:,:,:), z1(:,:,:,:)
      real*8, save,allocatable :: 
     *        x2(:,:,:,:), y2(:,:,:,:), z2(:,:,:,:)
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
      integer,  parameter ::  nlevmax=NLEVMAX,NSPECIESMAX=1      ! For HCN, MY

!---- Physical constants
      real*8, Parameter :: KB=1.380664D-16

      Parameter(NS=1)           ! dummy
      INTEGER NL(NSPECIESMAX)

      real*8 tk( 0:ixmax,0:iymax,0:izmax)  
                               ! kin temperature of hydro data
      real   rho( 0:ixmax,0:iymax,0:izmax)  ! density

!      Real*8 ELEV(0:NLEVMAX)  for Imaeda code
      Real*8 E(0:NLEVMAX)      ! level energy, for Tomisaka code
	
      Real*8 MU,BB

!      Real*8 C(NSPECIESMAX,-1:NLEVMAX+1,-1:NLEVMAX+1) &

      real*8 :: C(0:nlevmax, 0:nlevmax)
      real*8 :: alpha(LEVMAX,ixmax+1,iymax+1,izmax+1)
      real*8 :: S(LEVMAX,ixmax+1,iymax+1,izmax+1)
      real*8 :: n(0:LEVMAX,ixmax+1,iymax+1,izmax+1)
      real*8 :: ntot (ixmax+1,iymax+1,izmax+1)
      real*8 :: CTABLE(TABLEMIN:TABLEMAX,-1:NLEVMAX+1,-1:NLEVMAX+1)

      real*8,dimension(0:ixm1,0:iym1,0:izm1) :: vx,vy,vz
!      real*8,dimension(0:ixm1,0:iym1,0:50) :: vz

!---- thermal width(MY, Jul.02.2007)
      real*8, dimension(1:ixmax+1, 1:iymax+1, 1:izmax+1) :: veff

      Real*8 tklist(0:TABLEMAX)
      Character(16)    TABLENAME

      real*8,external :: g   ! statistical weight

      END MODULE state_inc

!=== MY, for initial population =========!
      module inipop
      implicit none
      integer, parameter :: levmax = LEVMAX
      end module inipop


!----------------------------------------------------------------------!
      function g(j) 
      real*8 :: g
      integer :: j

      g = 2*j +1
      
      END function g

!======================================================================!
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
!     7.7:  make direct use of Leiden database available for
!           every spiecies. Molecule transition data should be converted
!           with 'mol_conv.F' coded by M. Yamada: 'mol_conv.F' is an 
!           update version of "rea_data.F", by K. Tomisaka.
!                                      Dec.28.2005, M. Yamada
!
      use state_inc   ! module for get_c
         
      implicit real*8 (a-h,o-z) 
      parameter (levmax=LEVMAX,Loopmax=LOOPMAX)
      dimension dif(0:levmax)

!--- test for poprot subroutine(MY, Jul.10.2006)
      real*8 :: fr(0:levmax), sum

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
      call get_c_0

!! test initial population (MY, Jul.05.2006)
      select case(INI)
      case(0)
         call popLTE_test
         if(Myrank == 0) then
            print*, 'end of popLTE_test'
         endif
      !
      ! initial population
      case default
         call popini
         if(Myrank == 0) then
            print*, 'end of popini'
            open(11,form='unformatted')
            write(11) n
            close(11)
         endif
      end select

      do Loop=1,Loopmax
!         print*, 'Loop before Solve_I=', Loop
         call Solve_I
!debug
!        stop
!debug
        call norm(dif)
        call underrelaxation(1d0)
!!        print*, 'Loop before Temperature=', Loop
!!        print*, Loop, float(Loop), 10**(int(log10(float(Loop))))
!!     $       , min(10, 10**(int(log10(float(Loop)))))
!!        if(mod(Loop,10**(int(log10(float(Loop)))))==0) then
!! test (MY, Jan.10.2008)
        if(mod( Loop,min(10,10**(int(log10(float(Loop))))) )==0) then
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

!-------------------------------------------------------------------!
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
        dif(lev)=0.d0
        do k=1,izm
           do j=1,iym
              do i=1,ixm
                 dif(lev)
     $                =dif(lev)
     $                +abs((n(lev,i,j,k)-n0(lev,i,j,k))/n(lev,i,j,k))
!                 if(n(lev,i,j,k).eq. 0.d0) then
!                    print*, 'Here!'
!                    print*, 'ijkm=', i, j, k, lev 
!                 endif
              end do
           end do
        end do
        dif(lev)=dif(lev)/ijkm
      end do
      end

!----------------------------------------------------------------------!
!     initialize
!----------------------------------------------------------------------!
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
      if(ngin .ne. ng) then
         print*,'input ng is incorrect', ngin, ng
         stop
      endif

      tk = tem


#ifdef ALPHA
      print*, 'ng=', ng, 'izmax=', izmax
      print*,'max, min rho()', maxval(rho), minval(rho)
      print*,'max, min tk()', maxval(tk), minval(tk)
      print*,'max, min vx()', maxval(vx), minval(vx)
      print*,'max, min vy()', maxval(vy), minval(vy)
      print*,'max, min vz()', maxval(vz), minval(vz)
#endif
      end subroutine read_data

!======================================================================!

      subroutine initia0
      use Rays
      use state_inc
      implicit real*8(a-h,o-z)
      parameter(Nray0=NRAY0,levmax=LEVMAX,   
     *          ixm2=ixm+2,iym2=iym+2,izm2=izm+2,
     *          pi=3.1415926535d0, pc=3.0857d18, au = 1.496d13)
      
      real*8, dimension(1:ixmax+1, 1:iymax+1, 1:izmax+1) :: sigma
      real*8 sigmac    ! scalar sigma
      
      common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0, eps,
     *              xcent, ycent, zcent


!     velocity field
      common /spectrum/ sigma, sigmac
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
!      dx0= 64*pc/ng    ! grid size (cm)
      dx0 = DX0 
      print*, 'dx0=', dx0

!      TB=2.7d0
      TB=TBG
      print*, 'background=', TB, ' K'
      kTB=bc*TB       


!!------- generate thermal width
      ! 
      print*,'vturb=',VTB
      vturb=VTB                 ! microturbulence
      !
      pm = 1.673d-24            ! proton mass [g]
      cth = 2.d0*bc/pm
      !
      do i = 0, ixmax 
         do j = 0, iymax
            do k = 0, izmax
               veff(i+1, j+1, k+1) = dsqrt(cth*tk(i, j, k)+vturb**2)
            enddo
         enddo
      enddo
      !
!      print*, 'min and max veff', minval(veff), maxval(veff)
!      print*, 'min and max tk in initia0', minval(tk), maxval(tk)

!      v0=1d5/3.0857d18  ! infall velocity (1km/s / 1pc)
      v0=1d5/3.0857d18*dx0      ! due to normalization this is necessary
!! MY, Jul.02.2007
      select case(SIG)
      case(0)
         sigmac=vturb/lc/sqrt(2d0)
         print*, 'sigmac=', sigmac
      case default
         sigma = veff/lc/sqrt(2.d0)
      end select
!!

!     read mol data
      open(molecule_data,file=CCOEFF,
     *     form='formatted',status='old')
      read(molecule_data,'(a10)') kind
      write(*,'(a10)') kind
      read(molecule_data,*) XC
      read(molecule_data,*) mu
                                ! CO electric dipole moment 
      read(molecule_data,*) bb
                                ! CO Rotation Constant (Robinson 1974) in GHz
      write(*,1000) xc,mu,bb
 1000 format('Composition=',1pe10.3,3x,
     *       'Dipole Moment=',e10.3,3x,
     *       'Rotation Const=',e10.3)

!      dx=dx0
!      dy=dx
!      dz=dx
!     r2=(rmax/dx0)**2
      eps=1d-8

      Nray=NRAY

!     print parameters
      write(*,'(''Ng='',i3)') ng
      write(*,'(''Nray='',i4,'' in '',i4)') Nray, Nray0

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
      print*, 'min and max BG=', minval(BG), maxval(BG)

      read(molecule_data,*) level0
      if (level0 .ne. levmax) then
        write(*,'(''levmax='',i2,3x,''lev data='',i2)')levmax,level0
      endif

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

! shift density array
      do k=1,izm
        do j=1,iym
          do i=1,ixm
            ntot(i,j,k)= rho(i-1,j-1,k-1)
         end do
        end do
      end do


!!
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
!! MY, Jun.02.2007
!!
      select case(SIG)
      case(0)
         !
         do j=1,iym
            do i=1,ixm
               call mkNrays(x0(i,j,k),y0(i,j,k),z0(i,j,k),
     *              nx(1,i,j,k),ny(1,i,j,k),nz(1,i,j,k),
     *              x1(1,i,j,k),y1(1,i,j,k),z1(1,i,j,k),
     *              x2(1,i,j,k),y2(1,i,j,k),z2(1,i,j,k),
     *                   dnu_nu0(1,i,j,k),sigmac,rand,rand_normal,
!     *              dnu_nu0(1,i,j,k),sigma(i,j,k),rand,rand_normal,
     *              Nray,i,j,k)
            end do
         end do
         !
      case default
         !
         do j=1,iym
            do i=1,ixm
               call mkNrays(x0(i,j,k),y0(i,j,k),z0(i,j,k),
     *              nx(1,i,j,k),ny(1,i,j,k),nz(1,i,j,k),
     *              x1(1,i,j,k),y1(1,i,j,k),z1(1,i,j,k),
     *              x2(1,i,j,k),y2(1,i,j,k),z2(1,i,j,k),
!     *                   dnu_nu0(1,i,j,k),sigma,rand,rand_normal,
     *              dnu_nu0(1,i,j,k),sigma(i,j,k),rand,rand_normal,
     *              Nray,i,j,k)
            end do
         end do
         !
      end select
      !
      end do

      nx(:,:,:,:)=-nx(:,:,:,:)
      ny(:,:,:,:)=-ny(:,:,:,:)
      nz(:,:,:,:)=-nz(:,:,:,:)

      end subroutine initia0


!======================================================================!
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
      
!======================================================================!

      subroutine mkNrays(x0,y0,z0,nx,ny,nz,x,y,z,
     *                   x1,y1,z1,dnu_nu0,sigma0,
     *                   rand,rand_normal,Nray,i0,j0,k0)

      use state_inc
      implicit real*8(a-h,o-z)
      parameter (ixm2=ixm+2,iym2=iym+2,izm2=izm+2)
      common /grid/ xf(0:ixm2), yf(0:iym2), zf(0:izm2), dx0, eps,
     *              xcent, ycent, zcent

      real*8 x0,y0,z0           ! a point where J is calculated
      real*8 nx(Nray), ny(Nray), nz(Nray)  
                                ! directions of Rays from boundary 
                                !     to (x0,y0,z0)
      dimension x(Nray), y(Nray), z(Nray)  
                                !     initial position on the boundary
      dimension x1(Nray), y1(Nray), z1(Nray)
                                ! final position on the cell boundary
      dimension ix(Nray), iy(Nray), iz(Nray)
                                ! grid num of the initial position
      dimension rand(3*Nray+3)  ! randum number in (-1,1)
      dimension rand_normal(Nray)
      real*8 dnu_nu0(Nray),px,py,pz
      integer iray
      !
      real*8 sigma0             ! = sigma(i0, j0, k0)

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
         costh=rand(iray)       ! -1 < cos(th) < +1
         phi=2d0*pi*rand(iray+Nray)
         px=sqrt(1d0-costh**2)*cos(phi)
         py=sqrt(1d0-costh**2)*sin(phi)
         pz=costh
!     s=sqrt(px*px+py*py+pz*pz)
!     s=1.
!     nx(iray)=px/s
!     ny(iray)=py/s
!     nz(iray)=pz/s
         nx(iray)=px
         ny(iray)=py
         nz(iray)=pz
!! MY, Jul.02.2007
         select case(SIG)
         case(0)
            dnu_nu0(iray)=rand_normal(iray)*sigma0 ! dnu/nu0
         case default
            dnu_nu0(iray)=rand_normal(iray)*sigma0 ! dnu/nu0
         end select
         w=w+dnu_nu0(iray)
         w2=w2+dnu_nu0(iray)**2
      end do
!     write(*,*) w/Nray,sqrt(w2/Nray-(w/Nray)**2),sqrt(w2/Nray)
!     find the crossing point through the boundary

      do iray=1,Nray
         if( nx(iray) > 0) then
            ix(iray) = ixm		
            isigx = 1
         else
            ix(iray) = 1
            isigx = 0
         endif
         if( ny(iray) > 0) then
            iy(iray) = iym		
            isigy = 1
         else
            iy(iray) = 1
            isigy = 0
         endif
         if( nz(iray) > 0) then
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

         if( nx(iray) > 0) then
            tx=(xf(ixm+1) - x0)/nx(iray)
         else
            tx=(xf(1) - x0)/nx(iray)
         endif
         if( ny(iray) > 0) then
            ty=(yf(iym+1) - y0)/ny(iray)
         else
            ty=(yf(1) - y0)/ny(iray)
         endif
         if( nz(iray) > 0) then
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

        if(dl < tx) then
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
          if( nx(iray) > 0) then
             isigx = 1
          else
             isigx = 0
          endif
          tx=(xf(i0+isigx)-x0)/nx(iray)
          
!@      isigy=int(ny(iray)+1d0)
          if( ny(iray) > 0) then
             isigy = 1
          else
             isigy = 0
          endif
          ty=(yf(j0+isigy)-y0)/ny(iray)

!@      isigz=int(nz(iray)+1d0)
          if( nz(iray) > 0)then
             isigz = 1
          else
             isigz = 0
          endif
          tz=(zf(k0+isigz)-z0)/nz(iray)

          if(abs(tx) > 1.e8) tx=1.e8
          if(abs(ty) > 1.e8) ty=1.e8
!     if(abs(tz) > 1.e4) tz=1.e4

          dl=min(tx,ty,tz)
          ixmin=int(dl/tx)      ! 1: if tx=min 0: otherwise
          iymin=int(dl/ty)      ! 1: if ty=min 0: otherwise
          izmin=int(dl/tz)      ! 1: if tz=min 0: otherwise

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

      end  subroutine mkNrays 




!======================================================================!
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
      common /density/ Xc
      real*8  Xc

#ifdef MPI
      real*8  work(0:levmax,ixm,iym,izm)
      real*8 :: work_c(0:nlevmax, 0:nlevmax)
#endif

      common /molecule/  A(levmax),B(levmax), BG(levmax)


#ifdef MPI
      include 'mpif.h'
      common /M_param/ Nprocs, Myrank, ista, iend
#endif


      do k=1,izm
        do j=1,iym
          do i=1,ixm
            do lev=1,levmax
              ! alpha, B: indexed by using upper level
!              alpha(lev,i,j,k)=(E(lev)-E(lev-1))/pi4 
!     *         *(n(lev-1,i,j,k)*g(lev)/g(lev-1)-n(lev,i,j,k))*B(lev)
              alpha(lev,i,j,k)=h/pi4 
     *         *(n(lev-1,i,j,k)*g(lev)/g(lev-1)-n(lev,i,j,k))*B(lev)
!!debug(MY)
!              print*,lev, 'alpha', i,j,k,alpha(lev,i,j,k)
             if((n(lev-1,i,j,k).eq.0.d0).OR.(n(lev,i,j,k).eq.0.d0)) then
                 print*,lev, 'alpha', alpha(lev,i,j,k),n(lev-1,i,j,k)
     $                ,n(lev,i,j,k)
                 print*, i,j,k
              endif
!!debug(MY)
!debug
               if(alpha(lev, i,j,k) .le. 0.)then
!                  print*,'error: alpha < 0', lev,alpha(lev,i,j,k)
!! (7.7.3)
!! (7.7.3)                  alpha(lev,i,j,k)=0.d0
!                  stop
               endif


              S(lev,i,j,k)=n(lev,i,j,k) *A(lev)
     *          /(n(lev-1,i,j,k)*g(lev)/g(lev-1)-n(lev,i,j,k))/B(lev)
!! (7.7.3)
!              if(S(lev,i,j,k).le.0.) then
!                 print*,'S=',S(lev,i,j,k),i,j,k,alpha(lev,i,j,k)
!              endif
!! (7.7.3)
            end do
          end do
        end do
      end do

!      print*,'<solve_i> max, min of alpha', maxval(alpha), minval(alpha)


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

!!            if(Myrank == 0) print*,'end of solve_I_0'
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
!!            if(Myrank == 0) then
!!               print*,'end of internal in solve_I'
!!            endif
          end do
        end do
      end do

#ifdef MPI
      call mpi_allreduce(n,work,dim_n,mpi_double_precision,
     *                   mpi_sum,mpi_comm_world,ierr)
      n(:,:,:,:)=work(:,:,:,:)
#endif
      end subroutine Solve_I

!======================================================================!

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
      real*8 sigmac

      common /molecule/ A(levmax),B(levmax), BG(levmax)

      common /const/ h,lc,bc
      real*8 lc 

!     velocity field
      common /spectrum/ sigma(1:ixmax+1, 1:iymax+1, 1:izmax+1), sigmac

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

      real*8 :: jlimit
      jlimit = JLIM
      
!        data Loopm/1000/
      data init/1/


!      g(J)=2*J+1

!     initialize
      if(init==1) then

!!(MY, Feb012007)
!         print*,'Einstein coeffs in internal'
!         do i=1, levmax
!            print*, i, B(i), BG(i)
!         enddo
!         print*,'IN INTERNAL: ntot=', ntot(i0,j0,k0)
!     $        , rho(i0-1,j0-1,k0-1)
!         print*, 'IN INTERNAL: BEFORE', C(0,1), i0, j0, k0
!         print*, 'IN INTERNAL:', tk(i0-1, j0-1, k0-1)
         call get_c(i0-1, j0-1, k0-1)  ! for debugging?
!         print*, 'IN INTERNAL: AFTER', C(0,1), i0, j0, k0

!----get C-coefficient depending on temperature ----
!      tk = 20.
!     call get_c(i0-1,j0-1,k0-1)

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
!! MY, Jul.02.2007
        select case(SIG)
        case(0)
           phi=exp(-df**2/2/sigmac**2)/sqrt(2d0*pi)/sigmac
        case default
           phi=exp(-df**2/2/sigma(i0, j0, k0)**2)
     $          /sqrt(2d0*pi)/sigma(i0, j0, k0)
        end select
!!
        dl(Nr)=dl(Nr)*phi*dx0
        
      end do

      do loop=1,loopm

        do lev=1,levmax
          w(lev)=0d0
          do Nr=1,Nray
!! MY(Feb.01.2007)
!             alpha(lev, i0,j0,k0) = 0.d0
            tau=alpha(lev, i0,j0,k0)*dl(Nr)
            if((tau<0).AND.(dabs(tau).ge.600.d0)) then
               print*,'Large tau',tau
               tau = BIGTAU
            endif
!!MY(Jul.11.2006)
            w(lev)=w(lev)+Intensity(Nr,lev)*exp(-tau)
     *               +S(lev, i0,j0,k0)*(1d0-exp(-tau))
!            if(alpha(lev,i0,j0,k0).le.0.d0) then
!!            if(Intensity(Nr,lev).eq.NaN) then
!!               print*,'dl=',Intensity(Nr,lev),dl(Nr),Nr,lev
!!               w(lev) = wold
!!               print*,'w=',w(lev)
!!            endif
          end do
          Js(lev)=w(lev)/Nray
!! MY(Feb.01.2007)
          if(Js(lev).eq.0.d0) then
!             print*,'ZERO Js', Js(lev)
             Js(lev) = 1.d-60
!             print*,'AFTER ZERO JS', Js(lev)
          endif
!!MY(Jul.11.2006 -- just for fun!
          if((Js(lev).eq.NaN).OR.(Js(lev).gt.1.d0)) then 
          print*,'JS LIMIT:',Js(lev)
             Js(lev)=jlimit
             print*,'Js', Js(lev),i0,j0,k0
          endif

!!MY
!!          print*,'Js=',Js(lev),i0,j0,k0
        end do
!       write(*,*) i0,j0,k0,Js

        a1(:,:)=a0(:,:)*ntot(i0,j0,k0)     ! C=C*n(H)
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
!!-- debug(MY)
          if(n(k,i0,j0,k0).eq.0.d0) then
             print*,'internal n',n(k,i0,j0,k0),k,d(k)
          endif
!!-- debug(MY)
        end do

!******
        do lev=1,levmax
           alpha(lev, i0,j0,k0)=h/pi4 
     *          *(n(lev-1,i0,j0,k0)*g(lev)/g(lev-1)-n(lev,i0,j0,k0))
     $          *B(lev)
!debug
!          if(alpha(lev, i0,j0,k0) < 0)then
!          endif
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


!======================================================================!
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
      common /spectrum/ sigma(1:ixmax+1, 1:iymax+1, 1:izmax+1), sigmac

      real*8 dnu_nu0(Nray), phi(Nray0)

      real*8 nx(Nray), ny(Nray), nz(Nray)  
                                ! directions of Rays from boundary to 
                                ! (x0,y0,z0)
      real*8 Intensity(Nray0,levmax), dl(Nray0)
      dimension x1(Nray), y1(Nray), z1(Nray)  
                                ! initial position on the boundary
      dimension x2(Nray), y2(Nray), z2(Nray) 
                                ! final position on the cell boundary
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
           print*,'<solve_i_0> iz > ng Nray: ',iray,':',i0,j0,k0,':'
     $          ,iz(iray)
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
           if( nx(iray) >0) then
              isigx = 1
           else
              isigx = 0
           endif
           tx=(xf(ix(iray)+isigx)-x(iray))/nx(iray)

!@         isigy=int(ny(iray)+1d0)
           if( ny(iray) >0) then
              isigy = 1
           else
              isigy = 0
           endif
           ty=(yf(iy(iray)+isigy)-y(iray))/ny(iray)


!@          isigz=int(nz(iray)+1d0)
           if( nz(iray) >0) then
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
     &             yf(iy(iray)+isigy),y(iray),ny(iray)
              print*,'<solve_i_0> tx,ty,tz:', tx, ty,tz
           endif
!@        ixmin=int(dl(iray)/tx)
!@        iymin=int(dl(iray)/ty)
!@        izmin=int(dl(iray)/tz)
           if( dl(iray) < tx) then
              ixmin = 0
           else
              ixmin = 1
           endif
           if( dl(iray) < ty) then
              iymin = 0
           else
              iymin = 1
           endif
           if( dl(iray) < tz) then
              izmin = 0
           else
              izmin = 1
           endif
!          if(ixmin+iymin+izmin.ge.2) write(*,*) ixmin,iynin,izmin

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
!! MY(Jul.02.2007)
          !!
          select case(SIG)
          case(0)
             phi(iray)=exp(-df**2/2/sigmac**2)/sqrt(2d0*pi)/sigmac
          case default 
             phi(iray)=
     $            exp(-df**2/2/sigma(ix(iray),iy(iray),iz(iray))**2)
     $            /sqrt(2d0*pi)/sigma(ix(iray),iy(iray),iz(iray))
          end select
        end do




!        do lev=1,levmax
        do iray =1,Nray
           do lev = 1, levmax
              !
              tau=alpha(lev,ix(iray),iy(iray),iz(iray))
     &             *(dl(iray)*dx0)*phi(iray)
                                ! alpha==alpha/nu0
!!(MY, Jul.12.2006)
              if((tau<0.d0).AND.(dabs(tau).gt.600.d0)) then
                 print*,'large tau', tau,lev
                 tau = BIGTAU
              endif
              !
              Intensity(iray,lev)=
     *             iflag(iray)*Intensity(iray,lev)
     *             +(1-iflag(iray))*(Intensity(iray,lev)*exp(-tau)
     *             +S(lev,ix(iray),iy(iray),iz(iray))*(1d0-exp(-tau)))
           enddo
        enddo
        ix(1:Nray)=ix0(1:Nray)
        iy(1:Nray)=iy0(1:Nray)
        iz(1:Nray)=iz0(1:Nray)
      end do
      write(*,*)'error in solve I'
      stop
 1000 return

      end subroutine solve_i_0




!======================================================================!
      subroutine Temperature(Loop,dif)
      use Rays
      use state_inc
      implicit real*8(a-h,o-z)
      parameter(Nray0=NRAY0,levmax=LEVMAX,   
     *          ixm2=ixm+2,iym2=iym+2,izm2=izm+2,pc=3.0857d18)
      parameter(pi=3.14159265d0,pi4=pi*4d0)

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
      common /spectrum/ sigma(1:ixmax+1, 1:iymax+1, 1:izmax+1), sigmac
 
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
     &           status='unknown', form='formatted')
         end do
!       final output
!	file_name_nlev = "data3/"//"nlev.d"
!        open(final,file=file_name_nlev,status='unknown',
!     *          form='unformatted')
         init=0
      end if

      write(loop_num,'(i3.3)')Loop
      print*, 'Loop=', Loop
      print*, 'loop_num=', loop_num

      if(mod(Loop,1) == 0) then
         file_name_nlev = "data3/sp"//loop_num//"nlev.d"
         open(final,file=file_name_nlev,status='unknown',
     *        form='unformatted')

         do lev=1,levmax
            write(num,'(i2.2)') lev
!         open(lev+10,file='data3/'//loop_num//'Tr_' //num//'.d',
!     *                status='unknown',form='unformatted')
         end do
         do lev=1,levmax
            write(num,'(i2.2)') lev
!     !          file_name_disp = "data3/"//loop_num//"Disp_" //num//".d" 
!     !          open(lev+60,file=file_name_disp,
!     !     *                status='unknown',form='formatted')
         end do

!       local temperature
        do lev=1,levmax
           do k=1,izm
              do j=1,iym
                 do i=1,ixm
!     write(*,*)i,j,k,lev,n(lev,i,j,k),n(lev-1,i,j,k)
                    Tex(lev,i,j,k)=-(e(lev)-e(lev-1))
     *                   /(bc*log(abs(n(lev,i,j,k)/n(lev-1,i,j,k))
     *                   *g(lev-1)/g(lev)))
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
!! MY, Jan.10.2008
!!        file_name_te = "data3/"//loop_num//"Te_" //num//".data"
!!        open(10,file=file_name_te,
!!     *       status='unknown',form='unformatted')
!!        write(10) Tex
!!        close(10)

!         close(10+lev)
!       end do

!       dispersion
   
!!        mmax=ixm
!!        do m=1,mmax
!!           do lev=1,levmax
!!              sum1=0d0
!!              sum2=0d0
!!              nsum=0
!!              do k=1,izm
!!                 do j=1,iym
!!                    do i=1,ixm
!!                       w=x0(i,j,k)**2+y0(i,j,k)**2+z0(i,j,k)**2
!!                       if (w >= (1d0*m-0.5d0)**2 
!!     *                      .and. w < (1d0*m+0.5d0)**2) then
!!                          nsum=nsum+1
!!                          sum1=sum1+Tex(lev,i,j,k)
!!                          sum2=sum2+Tex(lev,i,j,k)**2
!!                       end if
!!                    end do
!!                 end do
!!              end do
!!              if (nsum==0) then
!!                 disp=0d0
!!              else
!!                 disp=sqrt(abs(sum2/nsum-(sum1/nsum)**2))
!!              end if
!!              write(lev+60,'(1p2e10.3)')m*dx0/pc,disp
!!           end do
!!        end do

!!        do lev=1,levmax
!!          close(lev+60)
!!        end do 

      end if

!     write down progress in one-step every time step
 
!        print*, 'here3'
      do lev=0,levmax
        write (lev+50,'(i3,1p10e10.3)') Loop,dif(lev)
        write (*,'(i3,i3,1pe10.3)') Loop,lev,dif(lev)
!        call flush(lev+50)
      end do
!        print*, 'here4'
      
      write(*,*) loop
      return

      entry final_output()
!      print*,'const. in final_output'
!      print*, 'h=', h, 'pi4=', pi4
!!-- Sep. 12.2006 (KT)     
      do k=1,izm
        do j=1,iym
          do i=1,ixm
            do lev=1,levmax
              ! alpha, B: indexed by using upper level
!              alpha(lev,i,j,k)=(E(lev)-E(lev-1))/pi4 
!     *         *(n(lev-1,i,j,k)*g(lev)/g(lev-1)-n(lev,i,j,k))*B(lev)
              alpha(lev,i,j,k)=h/pi4 
     *         *(n(lev-1,i,j,k)*g(lev)/g(lev-1)-n(lev,i,j,k))*B(lev)
              S(lev,i,j,k)=n(lev,i,j,k) *A(lev)
     *          /(n(lev-1,i,j,k)*g(lev)/g(lev-1)-n(lev,i,j,k))/B(lev)
            end do
          end do
        end do
      end do
!!-- END Sep. 12.2006 (KT)     
!
      write(final) ng,levmax,Nray
      write(final) xf,yf,zf,dx0
      !!
      select case(SIG)
      case(0)
         write(final) vx,vy,vz,sigmac         
      case default
         write(final) vx,vy,vz,sigma         
      end select
      !!
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

!#############################################################
!     reading C coeficients from a table
      subroutine get_c_0

      use state_inc


      implicit none

      Double Precision    xb,yb,zb

      Integer*4 ix,iy,iz

      Integer*4 i,j,k,l

      Character(16) ::   filename
      Real*8 tempdblearray(0:NLEVMAX)

      Integer levels_in_table,number_of_tables
      Character(64) :: molecule

      real*8 :: xc


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

!	print*,'tablemax, tablemin: ', TABLEMIN,TABLEMAX

      Do l=TABLEMIN,TABLEMAX

         Read(10,*) tklist(l)
!         write(*,*)'Tklist',tklist(l)
         Read(10,*)

!         print*, 'NLEVMAX=', NLEVMAX
         Do j=0,NLEVMAX
            Read(10,*) (tempdblearray(i),i=0,NLEVMAX)
            Do k = 0,NLEVMAX
!	       print*,'tempdblearray = ', k,j, tempdblearray(k)
               CTABLE(l,k,j) = tempdblearray(k)
            End do
         End do

      End do

      Close(Unit=10)

      end subroutine get_c_0
!#############################################################
!     calculating C coeficients from a table

      subroutine get_c(ii,jj,kk)

      use state_inc
      implicit none

      integer :: ii, jj, kk, i,j, ilabel !tlowindex, thighindex
      real :: tlow, thigh
      real*8 :: q
!MY(Jul.05.2006)
!      integer, parameter :: levmax =5
      integer, parameter :: levmax = LEVMAX
!--MY
      real*8, parameter :: tiny = 1.d-180

      integer :: it, l

!--- Calculate Collisional Rate from tk(ix,iy,iz). -> C(NS,i,j)
!--MY (CPP)
!      print*,'TABLEMAX in get_c', TABLEMAX,tklist(TABLEMAX)
!      print*,'LEVMAX in get_c', LEVMAX, levmax

!--MY      
      If (tk(ii,jj,kk).gt.tklist(TABLEMAX)) Then
         write(*,*) "Kinetic temperature is too high!!"
         Stop
      Endif

!--MY
      If (tk(ii,jj,kk).lt.tklist(TABLEMIN)) Then
         write(*,*) "Kinetic temperature is too low!!"
         print*, ii, jj, kk, tk(ii, jj, kk)
         Stop
      Endif


!-- MY
      do it = TABLEMIN, TABLEMAX
         if( tklist(it).ge.tk(ii,jj,kk) ) exit
      enddo
      thigh = tklist(it)
      tlow = tklist(it-1)
      ilabel = it
!     print*, 'thigh and tlow in get_c',tlow,thigh,tk(ii,jj,kk)
!     $              ,it
      
!     If (tlow.eq.0.0D0) tlow = 5.0
      If (tlow.eq.0.0D0) tlow = tklist(TABLEMIN)
      

!---- Log Linier Interpolation
!               Do i = 0,NL(NS)
!                  C(NS,i,i) = 0.0D0
!                  C(i,i) = 0.0D0
!               End do
      c = 0.0D0

      q = (log(tk(ii,jj,kk))-log(tlow))
     $     /(log(thigh)-log(tlow))
!?               do i = 1, levmax
!?                  do j = 0,i-1
!?                     C(i,j) = Exp( Log(CTABLE(tlowindex ,i,j))*(1.0D0-q) 
!?     &                    +Log(CTABLE(thighindex,i,j))*q )
!?                  enddo
!?               enddo
      do i=0, levmax
         do j =0, levmax
            if(i.ne.j) then
               if((CTABLE(ilabel-1,i,j).le.tiny).OR
     $              .(CTABLE(ilabel,i,j).le.tiny)) then
                                ! linear interpolation
                  C(i, j) = CTABLE(ilabel-1,i,j)*(1.d0-q)
     $                 +CTABLE(ilabel,i,j)*q
               else
                  C(i, j)       ! log-linear interpolation
     $                 = exp(log(CTABLE(ilabel-1,i,j)*1.d50)
     $                 *(1.d0-q)
     $                 +log(CTABLE(ilabel,i,j)*1.d50)*q)/1.d50
               endif
            else                ! i=j
               C(i,j) = 0.d0
            endif
         enddo
      enddo

!!-- calculate de-excitation coefficients
!!m test jun.21.2006(MY)
!!m                Do j = 1, levmax
!!m                  Do i = 0,j-1
!!m                     C(i,j) = C(j,i)*G(j)/G(i)
!!m     $                    *exp(-(e(j)-e(i))/(KB*tk(ii,jj,kk)))
!!m                  Enddo
!!m               Enddo
!! test jun.21.2006(MY)
               

!               stop



!---- The end of C(ix,iy,iz) calculation
               end subroutine get_c

!****************************** GAMMA DEVIATES GENERATOR ***** P 206 










