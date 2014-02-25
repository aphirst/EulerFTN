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

module Constants
  implicit none

  integer,  parameter :: dp = selected_real_kind(15, 307) ! 15 digits of precision, exponent range of 307
  integer,  parameter :: long = selected_int_kind(18) ! (signed) integer up to 10^18
  real(dp), parameter :: varphi = 0.5_dp + (0.5_dp)*sqrt(5.0_dp)

end module Constants
