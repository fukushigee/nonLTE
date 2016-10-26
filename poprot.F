!----------------------------------------------------------------------!
      subroutine poprot(i, j, k, fr, sum)
      use state_inc
      use inipop
      !
      implicit none
      !
      integer :: i1, j1, k1
      integer :: i, j, k
      !
      real*8 :: rmat(0:levmax, 0:levmax), bmat(0:levmax, 0:levmax)
      real*8 :: amat(1:levmax+1, 1:levmax+1)
      real*8 :: dummy(1:levmax+1, 1:levmax+1), dum
      real*8 :: sum               ! to be 1
      real*8 :: densty, tempra
      real*8 :: fr(1: levmax+1)  ! array to insert solution
      !
      real*8, parameter :: escp = 1.d0  ! escape probability
      real*8 :: qj(1:levmax)
      !
      real*8 :: dsig
      integer :: indx(1:levmax+1)

      real*8 :: aj, bj, bg
      real*8 :: h, lc, bc


      common /const/ h, lc, bc
      common /molecule/ aj(levmax), bj(levmax),bg(levmax)

!      print*,'levmax in poprot', levmax
!      print*,'i=',i,'j=',j,'k=',k

!      print*,'EINSTEIN in poprot:'
!      do i = 1, levmax
!         print*, i, bj(i), bg(i)
!      enddo

      tempra = tk(i, j, k)
      densty = rho(i, j, k)


!....... collisional coeffcients .......!
      call get_c(i, j, k)

!-----------------------------------------------------!
!        CONSTRUCT MATRIX AND SOLVE POPULATION        !
!-----------------------------------------------------!
!....... prepare R(i,j) .......!
      do i =0, levmax
         do j = 0, levmax
            rmat(i, j) = c(i, j)*densty
            bmat(i, j) = 0.d0  ! initialize
         enddo
      enddo
      !
      do j=0, levmax
         bmat(0, j) = 1.d0
         fr(j+1) = 0.d0
      enddo
      fr(1) = -1.d0
      !
      do j = 1, levmax
         qj(j) = 1.d0
     $        /((dexp((e(j)-e(j-1))/2.73d0/bc))-1.d0)
      enddo
      !
      do j =1, levmax
         rmat(j,j-1) = rmat(j,j-1)+aj(j)*escp*(1.d0+qj(j))
         rmat(j-1,j) = rmat(j-1,j)+g(j)/g(j-1)*aj(j)*escp*qj(j)
      enddo

!....... A(i,i)
      do i =0, levmax
         dum = 0.d0
         do j=0, levmax
            dum = dum + rmat(i, j)
         enddo
         rmat(i, i) = dum
      enddo


!....... shift matrix .......!
      do i=1, levmax+1
         do j =1, levmax+1
            amat(i, j) = (-1.d0)*rmat(j-1,i-1)
         enddo
         amat(i, i) = rmat(i-1, i-1)
      enddo
      ! compile matrix
      do i=1, levmax+1
         do j=1, levmax+1
            dummy(i,j) = amat(i, j)-bmat(i-1,j-1)
         enddo
      enddo

!....... solve population .......!
      call ludcmp(dummy, levmax+1, levmax+1, indx, dsig)
!      print*,'dsig in poprot=',dsig
      call lubksb(dummy, levmax+1, levmax+1,indx, fr)
      dum = 0.d0
      do j=1, levmax+1
         dum = dum+fr(j)
      enddo
      sum = dum
!      print*,'sum=', dum
!      print*,(fr(j),j=1, levmax+1)
      do j =1, levmax+1
         if(fr(j) == 1.d0) print*,'zero',j,densty, tempra
      enddo

      return
      end subroutine poprot
