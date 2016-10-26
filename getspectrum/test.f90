program main

real :: a
integer :: i

a = 1.0

do i = 1, 100
  a = a  + 1.0
enddo

print*, a

end program main
