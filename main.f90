! This file is part of EulerFTN.
! Copyright (C) 2013-2014 Adam Hirst <adam@aphirst.karoo.co.uk>
!
! EulerFTN is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! EulerFTN is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with EulerFTN. If not, see <http://www.gnu.org/licenses/>.

program EulerFTN
  use Euler

  implicit none

  integer :: problem

  print *, 'Which Project Euler problem do you wish to solve?'
  print *, 'Eventually entering "0" should give you some sort of help.'
  read *, problem

  select case (problem)
  case (0)
    stop 'Help not yet implemented. Sorry.'
  case (1)
    call Problem_1
  case (2)
    call Problem_2
  case (3)
    call Problem_3
  case (4)
    call Problem_4
  case (5)
    call Problem_5
  case (6)
    call Problem_6
  case (7)
    call Problem_7
  case (8)
    call Problem_8
  case (10)
    call Problem_10
  case (14)
    call Problem_14
  case default
    stop 'Problem number invalid, or not solved yet.'
  end select

end program EulerFTN
