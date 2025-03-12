program main

  use :: mod_khun

  implicit none

  integer, parameter :: d = 20
  integer, dimension(d,d) :: m
  integer, dimension(d) :: ind
  integer :: sum
  character(20) :: sum_str
  integer :: i
  integer :: j

  do i = 1, d
    read(*,*) m(i,:)
  end do

!  call khun(d,m,ind,sum,verbose=.true.)
  call khun(d,m,ind,sum,verbose=.false.)
  write(*,'(20(I0.2))',advance="no") ind
  write(sum_str,*) sum
  sum_str = adjustl(sum_str)
  write(*,'(A)') ":"//trim(sum_str)

end program main
