program main

  use, intrinsic :: iso_fortran_env

  implicit none

  integer, parameter :: D = 8

  integer, dimension(3,3) :: m
  integer :: i
  integer :: j
  logical, dimension(3,3) :: mask
  integer :: val
  integer, dimension(2) :: loc
  real(REAL64) :: a
  integer :: n

  do i = 1, D
    do j = 1, D
      call random_number(a)
      n = floor(a*1000.0_REAL64)
      write(*,'(3X,I3)',advance="no") n
    end do
    write(*,*)
  end do
  stop

  write(*,*) 3.141592653589793_REAL64 / 180.0_REAL64
  stop

  do i = 1, 3
    do j = 1, 3
      m(i,j) = (i-1)*3 + j
      mask(i,j) = .true.
    end do
  end do

  do i = 1, 3
    write(*,*) m(i,:)
  end do

  do i = 1, 3
    write(*,*)
    do j = 1, 3
      write(*,*) mask(j,:)
    end do
    val = minval(m,mask=mask)
    loc = minloc(m,mask=mask)
    write(*,*) val, loc
    mask(loc(1),:) = .false.
    mask(:,loc(2)) = .false.
  end do

end program main
