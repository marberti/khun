program main

  use :: mod_khun

  implicit none

  integer, parameter :: d = 20
  integer, dimension(d,d) :: m
  integer, dimension(d) :: ind
  integer :: sum
  character(20) :: sum_str
  character(3) :: mode
  integer :: i
  integer :: j

  call get_command_argument(1,mode)

  do i = 1, d
    read(*,*) m(i,:)
  end do

  call khun(d,m,mode,ind,sum,verbose=.false.)
  write(*,'(20(I0.2))',advance="no") ind
  write(sum_str,*) sum
  sum_str = adjustl(sum_str)
  write(*,'(A)') ":"//trim(sum_str)

end program main
