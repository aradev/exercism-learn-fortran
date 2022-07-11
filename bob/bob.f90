module bob
  implicit none
contains



function has_letters(statement)
  logical :: has_letters
  character(len=*), intent(in) :: statement
  character (*), parameter   :: all_letters = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
  integer                    :: i

  do i = 1, len_trim(statement)           ! All non-blanks
    if (index(all_letters, statement(i:i)) > 0) then
      has_letters = .true.
      return
    end if
  end do

  has_letters = .false.
end function has_letters

  function is_question(statement)
    logical :: is_question
    character(len=*), intent(in) :: statement

    if (index("?", statement(len_trim(statement):len_trim(statement))) > 0) then
      is_question = .true.
      return
    end if

    is_question = .false.
  end function is_question

  function is_all_uppercase(statement)
    logical :: is_all_uppercase
    character(len=*), intent(in) :: statement
    character (*), parameter   :: low = 'abcdefghijklmnopqrstuvwxyz'
    integer                    :: i

    do i = 1, len_trim(statement)           ! All non-blanks
      if (index(low, statement(i:i)) > 0) then
        is_all_uppercase = .false.
        ! print *, 'The value of amount (integer) is: ', statement, i
        return
      end if
    end do

    is_all_uppercase = .true.
  end function is_all_uppercase

  function is_empty(statement)
    logical :: is_empty
    character(len=*), intent(in) :: statement

    if (len_trim(statement) > 0) then
      is_empty = .false.
      return
    end if

    is_empty = .true.
  end function is_empty


  function hey(statement)
    character(100) :: hey
    character(len=*), intent(in) :: statement

    if (is_empty(statement)) then
      hey = "Fine. Be that way!"
      return
    end if

    if (has_letters(statement) .and. is_all_uppercase(statement) .and. is_question(statement)) then
      hey = "Calm down, I know what I'm doing!"
      return
    end if


    if (has_letters(statement) .and. is_all_uppercase(statement)) then
      hey = "Whoa, chill out!"
      return
    end if

    if (is_question(statement)) then
      hey = "Sure."
      return
    end if
  
    ! stating anything else
    hey = "Whatever."

  end function hey

end module bob
