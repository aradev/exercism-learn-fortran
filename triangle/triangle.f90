
module triangle
  implicit none

  interface equilateral
    module procedure equilateral_real
    module procedure equilateral_int
  end interface

  interface scalene
    module procedure scalene_real
    module procedure scalene_int
  end interface

  interface isosceles
    module procedure isosceles_real
    module procedure isosceles_int
  end interface

  interface is_triangle
    module procedure is_triangle_real
    module procedure is_triangle_int
  end interface

 contains

 logical function is_triangle_real(edges)
    real,dimension(3) :: edges

    if ( .not. edges(1) > 0 ) then
      is_triangle_real = .false.
      return
    end if

    if ( .not. edges(2) > 0 ) then
      is_triangle_real = .false.
      return
    end if

    if ( .not. edges(3) > 0 ) then
      is_triangle_real = .false.
      return
    end if

    if ( edges(1) + edges(2) < edges(3) ) then
      is_triangle_real = .false.
      return
    end if

    if ( edges(3) + edges(2) < edges(1) ) then
      is_triangle_real = .false.
      return
    end if

    if ( edges(3) + edges(1) < edges(2) ) then
      is_triangle_real = .false.
      return
    end if

    is_triangle_real = .true.
  end function

  logical function is_triangle_int(edges)
    integer, dimension(3) :: edges

    if ( .not. edges(1) > 0 ) then
      is_triangle_int = .false.
      return
    end if

    if ( .not. edges(2) > 0 ) then
      is_triangle_int = .false.
      return
    end if

    if ( .not. edges(3) > 0 ) then
      is_triangle_int = .false.
      return
    end if

    if ( edges(1) + edges(2) < edges(3) ) then
      is_triangle_int = .false.
      return
    end if

    if ( edges(3) + edges(2) < edges(1) ) then
      is_triangle_int = .false.
      return
    end if

    if ( edges(3) + edges(1) < edges(2) ) then
      is_triangle_int = .false.
      return
    end if

    is_triangle_int = .true.
  end function


  logical function equilateral_real(edges)
    real,dimension(3) :: edges

    if ( .not. is_triangle(edges) ) then
      equilateral_real = .false.
      return
    end if

    if ( (abs(edges(1) - edges(2)) > 1.0D-6)) then
      equilateral_real = .false.
      return
    end if

    if ( (abs(edges(1) - edges(3)) > 1.0D-6)) then
      equilateral_real = .false.
      return
    end if

    if ( (abs(edges(3) - edges(2)) > 1.0D-6)) then
      equilateral_real = .false.
      return
    end if

    equilateral_real = .true.
  end function

  logical function equilateral_int(edges)
    integer,dimension(3) :: edges

    if ( .not. is_triangle(edges) ) then
      equilateral_int = .false.
      return
    end if

    if ( (abs(edges(1) - edges(2)) > 1.0D-6)) then
      equilateral_int = .false.
      return
    end if

    if ( (abs(edges(1) - edges(3)) > 1.0D-6)) then
      equilateral_int = .false.
      return
    end if

    if ( (abs(edges(3) - edges(2)) > 1.0D-6)) then
      equilateral_int = .false.
      return
    end if

    equilateral_int = .true.
  end function

  logical function isosceles_real(edges)
    real,dimension(3) :: edges

    if ( .not. is_triangle(edges) ) then
      isosceles_real = .false.
      return
    end if

    if ( (abs(edges(1) - edges(2)) < 1.0D-6)) then
      isosceles_real = .true.
      return
    end if

    if ( (abs(edges(1) - edges(3)) < 1.0D-6)) then
      isosceles_real = .true.
      return
    end if

    if ( (abs(edges(2) - edges(3)) < 1.0D-6)) then
      isosceles_real = .true.
      return
    end if

    isosceles_real = .false.
  end function

  logical function isosceles_int(edges)
    integer,dimension(3) :: edges

    if ( .not. is_triangle(edges) ) then
      isosceles_int = .false.
      return
    end if

    if ( (abs(edges(1) - edges(2)) < 1.0D-6)) then
      isosceles_int = .true.
      return
    end if

    if ( (abs(edges(1) - edges(3)) < 1.0D-6)) then
      isosceles_int = .true.
      return
    end if

    if ( (abs(edges(2) - edges(3)) < 1.0D-6)) then
      isosceles_int = .true.
      return
    end if

    isosceles_int = .false.
  end function

  logical function scalene_real(edges)
    real,dimension(3) :: edges

    if ( .not. is_triangle(edges) ) then
      scalene_real = .false.
      return
    end if

    if (isosceles(edges) ) then
      scalene_real = .false.
      return
    end if

    scalene_real = .true.
  end function

  logical function scalene_int(edges)
    integer,dimension(3) :: edges

    if ( .not. is_triangle(edges) ) then
      scalene_int = .false.
      return
    end if

    if (isosceles(edges) ) then
      scalene_int = .false.
      return
    end if

    scalene_int = .true.
  end function

end module
