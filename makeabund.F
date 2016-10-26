!----------------------------------------------------------------------!
!     makeabund.F : generate abundance matrix to be include in         !
!                   main7.8.3.F                                        !
!                                                                      !
!                                 Feb.20.2006, M. Yamada               !
!----------------------------------------------------------------------!
      program makeabund
      implicit none
      !
      integer, parameter:: ng=64      ! wada calc.data
      integer, parameter:: ngz=32     ! wada calc.data
      !
      real*8 abund(0:ng-1, 0:ng-1, 0:ngz-1)
      integer i, j, k
      !

!------- initialize -------!
      abund = 0.1d-5
      ! first cube of 1/4 volume lack molecules
      do i = 0, ng/2-1
         do j = 0, ng/2-1
            do k = 0, ng/2-1
               abund(i,j,k) = 0.d0
            enddo
         enddo
      enddo

      abund = 2.d-9  ! uniform
!------- write out -------!
      open(1, file = 'abund_q1.dat', form='unformatted')
!      open(1, file = 'abund_128.dat', form='unformatted')  ! uniform
      write(1) ng, ng, ngz
      write(1) abund
      close(1)

      end program makeabund
