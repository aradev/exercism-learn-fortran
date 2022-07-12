
module matrix
  implicit none

contains

  function row(m, m_dim, i) result(r)
    character(len=*), intent(in) :: m(:)
    integer, intent(in) :: m_dim(2)
    integer, intent(in) :: i
    integer,dimension(m_dim(2)) :: r

    r(:) = 0

    read(m(i), *) r(1:m_dim(2))

  end function

  function column(m, m_dim, j) result(c)
    character(len=*), intent(in) :: m(:)
    integer, intent(in) :: m_dim(2)
    integer, intent(in) :: j
    integer, dimension(m_dim(1)) :: c
    integer :: i
    integer,dimension(m_dim(2)) :: r

    c(:) = 0

    do i = 1, m_dim(1)
      read(m(i), *) r(1:m_dim(2))
      c(i) = r(j)
    end do

  end function

end module
