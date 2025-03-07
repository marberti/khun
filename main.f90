program main

  use :: mod_khun

  implicit none

  integer, parameter :: d = 4
  integer, dimension(d,d) :: m
  integer, dimension(d) :: ind
  integer :: sum

  m(1,1) = 78; m(1,2) = 74; m(1,3) = 54; m(1,4) = 55
  m(2,1) = 77; m(2,2) = 18; m(2,3) = 63; m(2,4) = 15
  m(3,1) = 32; m(3,2) = 61; m(3,3) = 12; m(3,4) = 74
  m(4,1) = 34; m(4,2) = 34; m(4,3) = 43; m(4,4) =  4

  call khun(d,m,ind,sum,.true.)
  write(*,*) "ind = ", ind
  write(*,*) "sum = ", sum

end program main
