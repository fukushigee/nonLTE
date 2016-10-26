!----------------------------------------------------------------------!
!     subroutine popini : calculates initial n(:,:,:,:) instead of     !
!                         LTE population using A & C coefficients      !
!                         n(lev,i,j,k) = xc*rho(i-1,j-1,k-1)*fr(lev-1) !
!                                                                      !
!                        Jul.10.2006, M. Yamada                        !
!----------------------------------------------------------------------!
      subroutine popini
      use state_inc
      use inipop
      !
      implicit none
      !
      integer :: i, j, k, lev
      !
      real*8 :: sum
      real*8 :: fr(1:levmax+1)
      real*8 :: xc

      common /density/ xc

      !
      do k =1, izm
         do j =1, iym
            do i =1, ixm
               call poprot(i-1, j-1, k-1, fr, sum)
               if(abs(sum-1.d0).ge.1.d-6) print*, sum
               do lev = 0, levmax
                  n(lev,i,j,k) = xc*rho(i-1,j-1,k-1)*fr(lev+1)
                  if(n(lev,i,j,k).eq.0) then
                     print*,'zero in popini', rho(i-1,j-1,k-1)
                  endif
               enddo
!               print*,'diff', sum
!               print*,n(5,i,j,k),rho(i-1,j-1,k-1)*fr(6)*xc
!     $              , n(0,i-1,j-1,k-1)
            enddo
         enddo
      enddo

      return
      end subroutine popini
