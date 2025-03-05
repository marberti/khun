module mod_khun

  use, intrinsic :: iso_fortran_env

  implicit none
  save
  private

  public :: khun

contains

!==============================================================================
! Public
!==============================================================================

subroutine khun(d,m,ind,sum)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(in) :: m
  integer, dimension(d), intent(out) :: ind
  integer, intent(out) :: sum

  logical, parameter :: verbose = .true.
  integer :: i
  integer :: j
  integer, dimension(d) :: u
  integer, dimension(d) :: v
  integer, dimension(d,d) :: q
  logical, dimension(d) :: er ! essential rows
  logical, dimension(d) :: ec ! essential columns

  if (verbose) then
    write(*,*) "Input matrix m ="
    do i = 1, d
      do j = 1, d
        write(*,'(X,I3)',advance="no") m(i,j)
      end do
      write(*,*)
    end do
  end if

  call initial_cover(d,m,u,v)
  if (verbose) then
    write(*,*) "Initial cover:"
    write(*,'(A)',advance="no") " u ="
    do i = 1, d
      write(*,'(X,I3)',advance="no") u(i)
    end do
    write(*,*)
    write(*,'(A)',advance="no") " v ="
    do i = 1, d
      write(*,'(X,I3)',advance="no") v(i)
    end do
    write(*,*)
  end if

  call compute_q(d,m,u,v,q)
  if (verbose) then
    write(*,*) "Qualification matrix q ="
    do i = 1, d
      do j = 1, d
        write(*,'(X,I2)',advance="no") q(i,j)
      end do
      write(*,*)
    end do
  end if

  er = .false.
  call compute_ec(d,q,er,ec)
  write(*,*) "er = ", er
  write(*,*) "ec = ", ec
  stop 33
  
  ! routine_I
  ! check complete cover
  ! compute_ec
  ! routine_II

  do i = 1, d
    do j = 1, d
      if (q(i,j) == 2) then
        ind(i) = j
        exit
      end if
    end do
  end do

  sum = 0
  do i = 1, d
    sum = sum + m(i,ind(i))
  end do

end subroutine khun

!==============================================================================
! Private
!==============================================================================

subroutine initial_cover(d,m,u,v)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(in) :: m
  integer, dimension(d), intent(out) :: u
  integer, dimension(d), intent(out) :: v

  integer, dimension(d) :: a
  integer, dimension(d) :: b
  integer :: a_sum
  integer :: b_sum
  integer :: a_max
  integer :: b_max
  integer :: i
  integer :: j

  do i = 1, d
    a_max = m(i,1)
    do j = 2, d
      if (m(i,j) > a_max) a_max = m(i,j)
    end do
    a(i) = a_max
  end do

  do j = 1, d
    b_max = m(1,j)
    do i = 2, d
      if (m(i,j) > b_max) b_max = m(i,j)
    end do
    b(j) = b_max
  end do

  a_sum = sum(a)
  b_sum = sum(b)

  !write(*,*) "a: ", a, "=> ", a_sum
  !write(*,*) "b: ", b, "=> ", b_sum

  if (a_sum <= b_sum) then
    u = a
    v = 0
  else
    u = 0
    v = b
  end if

end subroutine initial_cover

!==============================================================================

subroutine compute_q(d,m,u,v,q)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(in) :: m
  integer, dimension(d), intent(in) :: u
  integer, dimension(d), intent(in) :: v
  integer, dimension(d,d), intent(out) :: q

  integer :: i
  integer :: j
  integer :: ii
  logical :: no_2

  q = 0
  do i = 1, d
    do j = 1, d
      if (u(i) + v(j) == m(i,j)) q(i,j) = 1
    end do
  end do

  do i = 1, d
    do j = 1, d
      if (q(i,j) == 1) then

        no_2 = .true.
        do ii = 1, i - 1
          if (q(ii,j) == 2) then
            no_2 = .false.
            exit
          end if
        end do

        if (no_2) then
          q(i,j) = 2
          exit
        end if

      end if
    end do
  end do

end subroutine compute_q

!==============================================================================

subroutine compute_ec(d,q,er,ec)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(in) :: q
  logical, dimension(d), intent(in) :: er
  logical, dimension(d), intent(out) :: ec

  integer :: i
  integer :: j

  do j = 1, d
    do i = 1, d
      if ((q(i,j) == 2).and.(er(i).eqv..false.)) then
        ec(j) = .true.
        exit
      end if
    end do
  end do

end subroutine compute_ec

!==============================================================================

subroutine routine_I()

end subroutine routine_I

!==============================================================================

subroutine routine_II(d,m,u,v,er,ec)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(in) :: m
  integer, dimension(d), intent(inout) :: u
  integer, dimension(d), intent(inout) :: v
  logical, dimension(d), intent(in) :: er
  logical, dimension(d), intent(in) :: ec

  integer :: dd
  integer :: dd_cur
  integer :: mm
  integer :: i
  integer :: j
  logical :: assigned
  logical :: zero_on_u

  assigned = .false.
  do i = 1, d
    if (er(i).eqv..true.) cycle
    do j = 1, d
      if (ec(j).eqv..true.) cycle
      dd_cur = u(i) + v(j) - m(i,j)
      if (assigned.eqv..false.) then
        dd = dd_cur
        assigned = .true.
      else if (dd_cur < dd) then
        dd = dd_cur
      end if
    end do
  end do
  
  zero_on_u = .false.
  do i = 1, d
    if ((er(i).eqv..false.).and.(u(i) == 0)) then
      zero_on_u = .true.
      exit
    end if
  end do

  mm = dd
  if (zero_on_u.eqv..false.) then
    do i = 1, d
      if ((er(i).eqv..false.).and.(u(i) < mm)) mm = u(i)
    end do

    do i = 1, d
      j = i
      if (er(i).eqv..false.) u(i) = u(i) - mm
      if (ec(j).eqv..true.)  v(j) = v(j) + mm
    end do
  else
    do j = 1, d
      if ((ec(j).eqv..false.).and.(v(j) < mm)) mm = v(j)
    end do

    do i = 1, d
      j = i
      if (er(i).eqv..true.)  u(i) = u(i) + mm
      if (ec(j).eqv..false.) v(j) = v(j) - mm
    end do
  end if

end subroutine routine_II

!==============================================================================

end module mod_khun
