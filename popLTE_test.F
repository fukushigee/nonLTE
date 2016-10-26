!======================================================================!
!     popLTE_test : test routine for insert LTE population as          !
!                   for the initial conditions                         !
!                   Origianlly a part of subroutine "initia0" (ver.7.7 !
!                   or later), but is separated for the test of        !
!                   inserting various conditions                       !
!                                                                      !
!                   Jun. 5. 2006, M. Yamada                            !
!                                                                      !
!    
!======================================================================!
      subroutine popLTE_test
      use state_inc
      use inipop     ! test
      !
      implicit none
      !
      integer :: i, j, k, lev
      !
      real*8 :: w, xc
      real*8 :: h, lc, bc
      
      common /density/xc
      common /const/ h, lc, bc

#ifdef MPI
      include 'mpif.h'
      common /M_param/Nprocs, Myrank, ista, iend
#endif

      
      do k =1, izm
         do j=1, iym
            do i=1, ixm
               w = 0.d0
               do lev = 0, levmax
                  w = w+(2.d0*dble(lev)+1.d0)
     $                 *exp(-e(lev)/(bc*tk(i-1,j-1,k-1)))
               enddo
!               print*, 'w in popLTE =', w, i, j, k
               do lev =0, levmax
                  n(lev,i,j,k) = rho(i-1,j-1,k-1)*xc*
     $                 (2.d0*dble(lev)+1.d0)*exp(-e(lev)
     $                 /(bc*tk(i-1,j-1,k-1)))/w
!                  print*, 'n',n(lev,i,j,k),lev,i,j,k
               enddo
            enddo
         enddo
      enddo


      end subroutine popLTE_test
