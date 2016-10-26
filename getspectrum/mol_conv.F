#ifdef LINUX
#define INFILE 'hcn_leiden.dat'
#define ABUNDV 2.d-8
#endif
!----------------------------------------------------------------------!
!
!     extracts collisional coefficients from Leiden molecular data
!     and convert into main.7.6.7.F form
!                                         Jul.26.2005, M.Yamada
!                                         Aug.1.2005, M.Yamada
!                                         Jun.24.2006, M. Yamada
!----------------------------------------------------------------------!
      program mol_conv
      implicit none
      integer, parameter :: infile = 1 ! read-in file
      real (kind=8), parameter :: vc = 2.997924528d10, ! light speed
     $                            hp = 6.626184d-27,   ! Planck const.
     $                            pi = 3.1415926535d0,    
     $                            bc = 1.380658d-16    ! boltzmann const.
      character (len=10) :: molecule   ! species
      real (kind=8) :: xmw             ! molecular weight
      integer :: nlevels               ! number of energy levels
      character (len=40) :: dummy      ! to read energy level data
                                       ! not requied 
      integer :: ntrans                ! number of radiative transitions
      integer :: ncols                 ! number of collision partners
      integer :: ntemps                ! temperature levels
      real (kind=8) :: tcol(1:20)      ! temperature column.
      real (kind=8) :: coleff(1:20, 0:100, 0:100)
                                       ! coll. coefficients
                                       ! coleff(ntk,ll,lu)
      integer :: ll, lu, ln            ! upper and lower level
      integer :: i, j, k, kT
      real (kind=8) xj 
      real (kind=8) el, gl, bb         ! del_E(1/cc), stat. weight,
                                       ! el. dipole moment
      real (kind=8) bbave
      real (kind=8) g(1:100), e(1:100) ! stat. weight array, ener. diff.
      real (kind=8) :: t0(1:20)        ! temperature array
      real (kind=8) :: mu, b, a, f, ek, coef, coefave
      character (len=16), parameter :: infname = INFILE
      ! INSERT PARAMETERS BELOW BY HAND !
      real (kind=8), parameter :: abund = ABUNDV,    ! mol abundance
     $                            eldip = 0.d0  ! el dipole mom.
      ! END OF MODIFICATION 

      open(infile, file=infname, form='formatted',status='old')
        !
        read(infile,'(/a10)') molecule
        write(*,'(a10," :Molecule Species")') molecule
        write(*,'(e10.4," :Mulecule Abundance")') abund
        write(*,'(e10.4," :electric dipole moment")') eldip
        !
        read(infile,'(/f4.1)') xmw
!        write(*,'(f4.1, 6x,":Molecular Weight")') xmw
        !
        read(infile,'(/i3)') nlevels
        write(*,'(i3, 7x,":Number of levels")') nlevels
        !
        read(infile,*)          ! read comment
        !
!        write(*,'(''   LL        EL        gL      J   BB'')') 
        bbave = 0.d0
        do i=1,nlevels
           read(infile,*) ln, el, gl, xj
           bb = vc*el/(xj*(xj+1.0))   ! rot.constant
           g(i-1) = gl 
           e(i-1) = el*hp*vc
           if(xj .ne. 0 ) bbave = bbave + bb
        enddo
        write(*,'(e10.4," :BB rotation constant")') bbave/dble(nlevels)
        !
        read(infile,'(/i3)') ntrans
        write(*,'(i3, 7x,":number of radiative transitions")') ntrans
        !
        read(infile,*)           ! read comment
        !
        coefave = 0.d0
        do i=1,ntrans
!!
!!           read(infile,'(2x,i3,3x,i3,3x,i3,2x,e11.4,3x,f13.7,3x,f8.2)')
!!     $          Ln,Lu,Ll,A,F,Ek
           read(infile,*)
     $          Ln,Lu,Ll,A,F,Ek
!           print*,'here'
!        Ju=Lu-1           ! Ju=Lu-1; L counts from 1 not 0 
           B=A/(16.D0*hp*BB*(BB/vc)**2*(lu-1)**3) ! B coeff 
           coef=B/((lu-1)/(2.0D0*(lu-1)+1.0D0)) 
           mu=sqrt(COEF/(32.D0/3.0D0 *(pi**4)/(hp**2)/vc)) ! dip. mom.
           coefave = coefave +mu
!        write(*,'(3(i2,2x),1pe11.4,3x,0pf13.7,3x,f7.2,1pe10.3)') 
!     *       ln,lu,ll,A,F,Ek,mu
        enddo
        print*, 'electric dipole=',coefave/dble(ntrans)
        !
        read(infile,'(/i3)') ncols
!        write(*,'(i3, 7x,":number of collision partners")') ncols
        !
        read(infile,'(/a40)') dummy
!        print*, '         :collision detail:', dummy
        !
        read(infile,'(///i3)') ntemps
!        print*, 'number of coll temperature:', ntemps
        write(*,'(i3,7x,":number of temperature bins")') ntemps
        !
        read(infile,*)           ! read comment
        !
        read(infile,'(20(1x,f6.1))') (tcol(kT),kT=1,ntemps)
        t0 = tcol
        !
        read(infile,*)          ! read comment
        !
        ! coll. coefficients
        !
        do i=2,ntrans+1
           do j=1,i-1
              read(infile,'(2x, i3, 3x, i3, 3x, i3, 1x, 20e8.1)')
!!     $             ln, lu, ll, (coleff(k,j-1,i-1),k=1,ntemps)
     $             ln, lu, ll, (coleff(k,i-1,j-1),k=1,ntemps)
           enddo
        enddo
        !
        ! rearrange and write out coeffs.
        !
!!        do k=2,ntemps
        do k=1,ntemps
           print*, ' '
           print*, tcol(k)
           write(*,'(100("( 0,",i2,")"))')(j, j=0,ntrans)
        ! excitation coefficients
           do i = 1, ntrans
              do j=0, i-1
                 coleff(k, j, i) = coleff(k, i, j)*g(i)/g(j)
     $                *dexp(-(e(i)-e(j))/bc/tcol(k))
              enddo
              coleff(k,j,j) = 0.d0
           enddo
           coleff(k,0,0) = 0.d0
        ! rearrange
           do j=0,ntrans
              write(*,'(1x, 100(1pg10.2e3))')
     $             (coleff(k,i,j),i=0,ntrans)
           enddo
        enddo

      close(infile)

      stop
      end program mol_conv
