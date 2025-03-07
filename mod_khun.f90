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

subroutine khun(d,m,ind,sum,verbose)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(in) :: m
  integer, dimension(d), intent(out) :: ind
  integer, intent(out) :: sum
  logical, intent(in), optional :: verbose

  character(*), parameter :: my_name = "khun"
  logical :: verbose_flag
  integer :: i
  integer :: j
  integer, dimension(d) :: u
  integer, dimension(d) :: v
  integer, dimension(d,d) :: q
  logical, dimension(d) :: er ! essential rows
  logical, dimension(d) :: ec ! essential columns
  character(4) :: status
  integer :: step

  if (present(verbose)) then
    verbose_flag = verbose
  else
    verbose_flag = .false.
  end if

  if (verbose_flag) then
    write(*,*) "Entering "//my_name//" subroutine"
    write(*,*) "Input matrix m ="
    do i = 1, d
      do j = 1, d
        write(*,'(X,I3)',advance="no") m(i,j)
      end do
      write(*,*)
    end do
  end if

  call initial_cover(d,m,u,v)
  if (verbose_flag) then
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
  if (verbose_flag) then
    write(*,*) "Qualification matrix q ="
    do i = 1, d
      do j = 1, d
        write(*,'(X,I2)',advance="no") q(i,j)
      end do
      write(*,*)
    end do
  end if

  step = 0
  do
    step = step + 1
    if (verbose_flag) then
      write(*,*)
      write(*,*) "**** Step: ", step
    end if

    call routine_I(d,q,er,status)
    if (check_complete_cover(d,q)) exit
    if (status == "Ia") cycle
    call compute_ec(d,q,er,ec)
    if (verbose_flag) then
      write(*,*) "er = ", er
      write(*,*) "ec = ", ec
    end if
    call routine_II(d,m,u,v,er,ec)
    call compute_q(d,m,u,v,q)
    if (verbose_flag) then
      write(*,*) "Qualification matrix q ="
      do i = 1, d
        do j = 1, d
          write(*,'(X,I2)',advance="no") q(i,j)
        end do
        write(*,*)
      end do
    end if
  end do

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

subroutine routine_I(d,q,er,status)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(inout) :: q
  logical, dimension(d), intent(out) :: er
  character(4), intent(inout) :: status

  integer :: i
  integer :: j
  logical :: eligible

  er = .false.
  status = "Ib"

  do j = 1, d
    eligible = .true.
    do i = 1, d
      if (q(i,j) == 2) then
        eligible = .false.
        exit
      end if
    end do

    if (eligible) then
      do i = 1, d
        if (q(i,j) == 1) then
          call build_sequence(d,q,i,j,er,status)
          if (status == "Ia") return
        end if
      end do
    end if
  end do

end subroutine routine_I

!==============================================================================

subroutine build_sequence(d,q,init_i,init_j,er,status)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(inout) :: q
  integer, intent(in) :: init_i
  integer, intent(in) :: init_j
  logical, dimension(d), intent(out) :: er
  character(4), intent(out) :: status

  character(*), parameter :: my_name = "build_sequence"
  integer, dimension(d,d) :: seq
  integer :: seq_last
  integer :: seq_n
  integer :: seq_i
  integer :: seq_j
  integer :: i
  integer :: j
  integer :: i_start
  logical :: found_1
  logical :: found_2

  seq = 0
  seq_last = 1
  seq_n = 1
  seq_i = init_i
  seq_j = init_j
  seq(seq_i,seq_j) = seq_n

  do
    if (seq_last == 1) then
      
      found_2 = .false.
      do j = 1, d
        if (q(seq_i,j) == 2) then
          found_2 = .true.
          seq_last = 2
          i_start = 1
          seq_n = seq_n + 1
          seq_j = j
          seq(seq_i,seq_j) = seq_n
          exit
        end if
      end do

      if (found_2.eqv..false.) then
        call invert_1_2_in_sequence(d,q,seq,seq_n)
        status = "Ia"
        return
      end if
    
    else if (seq_last == 2) then

      found_1 = .false.
      do i = i_start, d
        if ((q(i,seq_j) == 1).and.(is_distinct(d,seq,i))) then
          found_1 = .true.
          seq_last = 1
          seq_n = seq_n + 1
          seq_i = i
          seq(seq_i,seq_j) = seq_n
          exit
        end if
      end do

      if (found_1.eqv..false.) then
        er(seq_i) = .true.
        i_start = seq_i + 1
        call delete_last_two_of_sequence(d,seq,seq_n,seq_i,seq_j)
        if (seq_n == 0) return
      end if

    else
      write(*,*) "FATAL ERROR: "//my_name//&
        ": the developer made an unforgivable mistake"
      stop 42
    end if
  end do

end subroutine build_sequence

!==============================================================================

logical function is_distinct(d,seq,i)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(in) :: seq
  integer, intent(in) :: i

  integer :: j

  do j = 1, d
    if (seq(i,j) /= 0) then
      is_distinct = .false.
      return
    end if
  end do

  is_distinct = .true.

end function is_distinct

!==============================================================================

subroutine invert_1_2_in_sequence(d,q,seq,seq_n)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(inout) :: q
  integer, dimension(d,d), intent(in) :: seq
  integer, intent(in) :: seq_n

  character(*), parameter :: my_name = "invert_1_2_in_sequence"
  integer :: i
  integer :: j
  integer :: n
  logical :: found

  do n = 1, seq_n
    found = .false.
    do i = 1, d
      do j = 1, d

        if (seq(i,j) == n) then
          if (q(i,j) == 1) then
            q(i,j) = 2
          else if (q(i,j) == 2) then
            q(i,j) = 1
          else
            write(*,*) "FATAL ERROR: "//my_name//&
              ": the developer made an unforgivable mistake"
            stop 42
          end if

          found = .true.
          exit
        end if

      end do
      if (found) exit
    end do
  end do

end subroutine invert_1_2_in_sequence

!==============================================================================

subroutine delete_last_two_of_sequence(d,seq,seq_n,seq_i,seq_j)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(inout) :: seq
  integer, intent(inout) :: seq_n
  integer, intent(out) :: seq_i
  integer, intent(out) :: seq_j

  character(*), parameter :: my_name = "delete_last_two_of_sequence"
  integer :: i
  integer :: j
  integer :: count
  logical :: found

  if (seq_n < 2) then
    write(*,*) "FATAL ERROR: "//my_name//&
      ": the developer made an unforgivable mistake"
    stop 42
  end if

  do count = 1, 2
    found = .false.
    do i = 1, d
      do j = 1, d
        if (seq(i,j) == seq_n) then
          seq(i,j) = 0
          seq_n = seq_n - 1
          found = .true.
          exit
        end if
      end do
      if (found) exit
    end do
  end do

  if (seq_n /= 0) then
    do i = 1, d
      do j = 1, d
        if (seq(i,j) == seq_n) then
          seq_i = i
          seq_j = j
        end if
      end do
    end do
  end if

end subroutine delete_last_two_of_sequence

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

logical function check_complete_cover(d,q)

  integer, intent(in) :: d
  integer, dimension(d,d), intent(in) :: q

  integer :: i
  integer :: j
  integer :: count

  count = 0
  do i = 1, d
    do j = 1, d
      if (q(i,j) == 2) then
        count = count + 1
        exit
      end if
    end do
  end do

  if (count == d) then
    check_complete_cover = .true.
  else
    check_complete_cover = .false.
  end if

end function check_complete_cover

!==============================================================================

end module mod_khun
