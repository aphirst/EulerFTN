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

module Functions
  use Constants
  implicit none

contains

  pure function Multiples_wheel(factors)
    ! return a 'wheel' of the multiples of 'factors'
    ! TODO: consider adapting this to use the same optimisation as the new Primes_sieve()
    integer(i8), intent(in)              :: factors(:)
    integer(i8),            allocatable  :: multiples_wheel(:)
    integer(i8)                          :: i, N
    logical,                allocatable  :: is_multiple(:)

    ! the largest entry in the repeating 'wheel' is the product of the factors
    N = product(factors)
    is_multiple = [( .false., i=1,N )] ! allocate-on-assignment needs full F2003/8 semantics
    ! use array strides to generate a mask corresponding to numbers which have required factors
    do i = 1,size(factors)
      is_multiple(factors(i)::factors(i)) = .true.
    end do
    ! create array of integers inside implied do-loop
    ! condense into final array of 'multiples' of 'factors' using IS_MULTIPLE as a mask
    multiples_wheel = pack( [( i, i=1,N )] , is_multiple) ! allocate-on-assigment
  end function Multiples_wheel

  elemental function Fib(n)
    ! returns the 'n'th Fibonacci number, defined to start as 1, 1, 2, 3 etc.
    integer(i8), intent(in) :: n
    integer(i8)             :: fib

    fib = floor( ( varphi**n / sqrt(5.0_dp) ) + 0.5_dp )
  end function Fib

  elemental function Inv_fib(F)
    ! returns index 'n' of first Fibonacci number BELOW F
    integer(i8), intent(in) :: F
    integer(i8)             :: inv_fib

    inv_fib = floor( log( F * sqrt(5.0_dp) ) / log(varphi) )
  end function Inv_fib

  pure function Old_sieve(limit) result(primes)
    ! returns array of primes up to LIMIT using simple sieve
    integer(i8), intent(in)               :: limit
    integer(i8),              allocatable :: primes(:)
    integer(i8)                           :: i
    logical,                  allocatable :: is_prime(:)

    ! we initially set all numbers to be "potentially prime", then progressively eliminate them
    is_prime = [ ( .true., i=1,limit ) ] ! allocate-on-assignment
    ! 2 is the first prime, so we ignore 1 from the array straight away
    do i = 2, floor(sqrt( real(limit) ))
      ! sieve out using the array stride
      if (is_prime(i)) is_prime(2*i:limit:i) = .false.
    end do
    ! pack an array of all integers into an array of primes using the 'is_prime' mask
    primes = pack( [( i, i=2,limit )] , is_prime(2:) ) ! allocate-on-assignment
  end function Old_sieve

  pure function Primes_sieve(limit) result(primes)
    ! todo: comment this function, basically uses the optimisation mentioned in the post-solution document for the problem
    integer(i8), intent(in)              :: limit
    integer(i8),             allocatable :: primes(:)
    integer(i8)                          :: i
    logical(1),              allocatable :: is_prime(:)

    is_prime = [( .true._1, i = 1, (limit-1)/2 )]
    do i = 1, ( floor(sqrt( real(limit) )) - 1 ) / 2
      if (is_prime(i)) is_prime( 2*i*(i+1) :: (2*i)+1 ) = .false._1
    end do
    primes = [ 2_i8, pack( [( (2*i)+1, i=1,size(is_prime) )], is_prime ) ]
  end function Primes_sieve

  pure function Primes_wheel(primes)
    ! return array of potentially-prime spokes for wheel-factorisation of prime-candidate
    ! using an input array of prime numbers 'primes'
    integer(i8), intent(in)              :: primes(:)
    integer(i8),             allocatable :: primes_wheel(:)
    integer(i8)                          :: i, N
    logical(1),              allocatable :: potential_prime(:)

    ! largest possible entry in wheel is the product of the numbers used to generate it
    N = product(primes)
    potential_prime = [( .true._1, i=1,N)]
    ! use array-strides to blank out entries which divide by any previous primes
    do i = 1,size(primes)
      potential_prime(primes(i)::primes(i)) = .false._1
    end do
    ! use the potential_prime mask to pack an array of potential prime, our wheel
    primes_wheel = pack ( [( i, i=1,N )] , potential_prime )
  end function Primes_wheel

  elemental function Primes_upto(x)
    ! use a standard inequality to return the maximum number of primes less-than or equal to some real 'x'
    ! todo: change x to an integer (n?), if needed (this function hasn't been used yet)
    real(dp),    intent(in) :: x
    integer(i8)             :: primes_upto

    ! possibly change this into a more explicit 'if'
    ! the danger is wastefully computing the right-hand expression when we know it will be less than 7
    primes_upto = maxval( [7_i8, int( 1.25506*(x/log(x)) , i8 )] )
  end function Primes_upto

  elemental function Largest_Pn(n) result(pn)
    ! returns the largest possible candidate for the n'th prime number
    ! for n >= 6
    integer(i8), intent(in) :: n
    integer(i8) :: pn

    if (n < 6) then
      pn = 13
    else
      pn = int( real(n) * log( real(n) * log(real(n)) ) , i8 )
    end if
  end function Largest_Pn

  pure function Next_factor(factorising, primes, wheel, prev_factor)
    ! use wheel factorisation to find the 'next prime factor' of a given input
    ! the output is either the first integer to divide the input, or the input itself if it is prime
    ! for factorising large numbers, the previous factor is an optional input
    ! (since this method generates factors in ascending order)
    integer(i8), intent(in)           :: factorising, primes(:),  wheel(:)
    integer(i8), intent(in), optional :: prev_factor
    integer(i8)                       :: factor_min, factor_max, i, wheel_circ, N_min, N_max, j, spoke, next_factor

    ! 'prev_factor' is an optional argument
    ! with it, the minimum possible factor, 'factor_min', is this 'previous factor'
    ! without it, the minimum possible factor is just the smallest prime
    if (present(prev_factor)) then
      factor_min = prev_factor
    else
      factor_min = primes(1)
    end if
    ! the largest possible factor, 'factor_max', is the square root of the number we're factorising
    factor_max = floor( sqrt(real(factorising,dp)) , i8 )
    ! if the minimum possible factor is greater than the maximum possible,
    ! the number must already be fully factorised
    if (factor_min > factor_max) then
      next_factor = factorising
      return
    end if
    ! first we want to check if the number we're factorising is divisible by anything in the small array of 'primes'
    if ( factor_min <= primes(size(primes)) ) then
      do i = count(primes <= factor_min), count(primes <= factor_max)
        if (modulo(factorising, primes(i)) == 0) then
          next_factor = primes(i)
          return
        end if
      end do
    end if
    ! next, check if the number divides by anything from the first run of the wheel
    ! done separately so that we can avoid the division by 1
    if ( factor_min <= wheel(size(wheel)) ) then
      ! max(2, count) should ensure that wheel(1)=1 is never used with modulo()
      do i = max( 2, count(wheel <= factor_min) ), count(wheel <= factor_max)
        if (modulo(factorising, wheel(i)) == 0) then
          next_factor = wheel(i)
          return
        end if
      end do
    end if
    ! now use the modular-arithmetic wheel for trial division
    ! extend each 'spoke' by adding multiples of the wheel's circumference, 'wheel_circ'
    ! note that this 'circumference' is always 1+maxval(wheel)
    wheel_circ = wheel(size(wheel)) + 1
    ! only for as many extra rungs as it takes to include sqrt(factorising)
    ! precompute the upper and lower bounds for the additional-rung count using the maximum and minimum possible factors
    N_max = factor_max/wheel_circ ! implicitly floored by integer division
    N_min = max(1_i8, factor_min/wheel_circ) ! ensure the 0th "additional" rung is excluded (i.e. the rung we just did)
    do i = max(1_i8, N_min), N_max
      do j = 1, size(wheel)
        spoke = (i * wheel_circ) + wheel(j)
        if (modulo(factorising,spoke) == 0) then
          next_factor = spoke
          return
        end if
      end do
    end do
    ! if we got all the way here, 'factorising' must be prime
    next_factor = factorising
  end function Next_factor

  pure function Prime_factors(to_factorise, primes, wheel) result(factors)
    ! make use of the 'Next_factor' wheel-factorisation method to return (all of) the
    ! prime factors of an input integer, as an integer array, with the prime factors in ascending order
    integer(i8), intent(in)              :: to_factorise, primes(:), wheel(:)
    integer(i8),             allocatable :: factors(:)
    integer(i8)                          :: factorising

    ! the first factor we get doesn't have a 'previous factor' to start at
    ! so we just initialise 'factors' with the first factor of to_factorise
    factors = [ Next_factor(to_factorise, primes, wheel) ]
    ! then, we divide the to-factorise number by the factor, to get an 'in progress' factorising number
    factorising = to_factorise / factors(size(factors))
    do
      ! if a number divided by its first factor is 1, that means the number is its first factor
      ! i.e. it is prime, and we are done
      if (factorising == 1) then
        return
      end if
      ! may the Lord have mercy on your soul if factorising == 0 ...
      ! extend the 'factors' array using the 'next factor' of 'factorising', also giving the optional argument of the 'last factor'
      factors = [ factors, Next_factor(factorising, primes, wheel, factors( size(factors) )) ]
      factorising = factorising / factors(size(factors))
    end do
  end function Prime_factors

  elemental function Is_palindrome(num)
    ! determine whether input number is palindromic by converting it into an array
    integer(i8), intent(in)              :: num
    logical                              :: is_palindrome
    integer(i8)                          :: i, n
    integer,                 allocatable :: num_array(:)

    ! we want to know how many digits are in 'num', we use log10
    n = ceiling( log10(real(num,dp)) )
    ! break the number into an array, implicit do-loop
    ! modulo(), division, and rounding returns the i'th digit of the number
    ! the number fills the array backwards, doesn't matter for our purposes
    num_array = [( int( modulo(num,10**i)/(10**(i-1)) ), i = 1,n )] ! allocate on assign
    ! use slicing, negative stride, and all() to check either 'half' of the array
    is_palindrome = all( num_array(1:n/2) == num_array(n:1+(n/2):-1) )
  end function Is_palindrome

  elemental function Palindrome(num)
    ! generates a 2n-digit long palindrome from an n-digit input
    ! by placing the input on the 'left', and flipping it 'onto' the right
    integer(i8), intent(in)              :: num
    integer(i8)                          :: i, n, palindrome
    integer,                 allocatable :: num_array(:), pal_array(:)

    ! use log10 to work out the number of digits in input
    ! palindrome will be twice as long
    n = ceiling( log10(real(num,dp)), i8)
    ! convert input number into digitwise array, each digit can be of default integer kind rather than long (i8)
    num_array = [( int(modulo( (num / 10**(n-i)) , 10_i8 )), i = 1,n )]
    ! create palindrome array by reversing the number array and appending to itself
    pal_array = [ num_array , num_array(n:1:-1) ]
    ! use powers of 10 to convert the array back into a number
    palindrome = sum( [( pal_array(i) * 10**(2*n - i), i=1,2*n )] )
  end function Palindrome

  pure recursive function Remove_duplicates(entries) result(unique)
    ! condenses an input 1D integer array by removing duplicated adjacent entries
    ! especially useful when the input is already sorted
    integer(i8), intent(in)              :: entries(:)
    integer(i8)                          :: occurs
    integer(i8),             allocatable :: unique(:)

    if (size(entries) <= 1) then
      unique = entries
    else
      occurs = count( entries == entries(1) )
      if (occurs <= size(entries)) then
        ! occurs once => start at element 2
        ! occurs twice => start at element 3 (element 2 will be the duplicate, skip it)
        ! etc.
        unique = [ entries(1), Remove_duplicates( entries(occurs+1:) ) ]
      else
        unique = [ entries(1) ]
      end if
    end if
  end function Remove_duplicates

  pure recursive function Qsort(unsorted) result(sorted)
    ! integer 1D quicksort
    !
    ! breaks a 1D array about a pivot (the first value in the array, for code brevity)
    ! masks the entries into 2 arrays (< or >= the pivot's value) using PACK
    ! sorts the resulting arrays recursively until each contain only 1 element
    ! recombine into output array using F2003/8 allocate-on-assignment
    integer(i8), intent(in)              :: unsorted(:)
    integer(i8),             allocatable :: sorted(:)

    if (size(unsorted) <= 1) then
      sorted = unsorted
    else
      sorted = [ Qsort(pack( unsorted(2:), unsorted(2:) < unsorted(1) )), unsorted(1), &
                 Qsort(pack( unsorted(2:), unsorted(2:) >= unsorted(1) )) ]
    end if
  end function Qsort

  pure function Lowest_common_multiple(divisors)
    ! returns the lowest common multiple of all the integers in the array 'divisors'
    integer(i8), intent(in)              :: divisors(:)
    integer(i8)                          :: Lowest_common_multiple, i, j
    integer(i8),             allocatable :: primes(:), wheel(:), factors(:), powers(:)

    primes = Primes_sieve( maxval(divisors) )
    wheel = Primes_wheel( primes(1:3) )
    powers = [( 0_i8, i = 1, size(primes) )] ! currently have to specify KIND of 0, GCC bug #58750
    ! for each 'divisor', check how many times each prime factor occurs
    do i = 1, size(divisors)
      factors = Prime_factors(divisors(i), primes(1:3), wheel)
      ! for each prime factor, keep only the MAXIMUM number of times the prime factor occurs in any member of divisors
      do concurrent ( j = 1:size(primes) )
        powers(j) = max( int(count(factors == primes(j)),i8) , powers(j) )
      end do
    end do
    Lowest_common_multiple = product( primes ** powers ) ! raise to power element-wise
  end function Lowest_common_multiple

  recursive function Collatz(n, lengths, memoise) result(coll)
    ! return the length of the Collatz sequence starting at input 'n'
    ! uses a memoisation array 'lengths' to take advantage of precomputed lengths for other values of n
    ! this routine can memoise as it runs, but only if the optional 'memoise' argument is set to .true.
    ! TODO: to be brutally honest, the mere presence of the `lengths` array should indicate that we want to memoise...
    integer(i8), intent(in)              :: n
    integer(i8), intent(inout)           :: lengths(:)
    logical,     intent(in)   , optional :: memoise
    integer(i8)                          :: coll

    ! only perform this check if 'n' lies inside the range covered by the memo
    if (n <= size(lengths)) then
      ! a length of 0 means that the corresponding 'n' has not been computed yet
      ! the value of a non-zero length is (obviously!) the desired length, precomputed
      if (lengths(n) /= 0) then
        coll = lengths(n)
        return
        ! 1 is the end of the sequence
        ! we adopt the convention where getting from a start of '1' to an end of '1' has length '1'
      else if (n == 1) then
        coll = 1
        lengths(1) = 1
        return
      end if
      ! if either of these conditions passes, it means we have the result without needing to compute it
      ! i.e. we're done already
    end if
    ! the length ('coll') of the sequence starting at input n
    ! is 1 plus the length of the next value of the sequence
    ! clearly this takes advantage of the recursive definition of the sequence
    if ( modulo(n,2_i8) == 0 ) then
      ! I'll explain this additional LOGICAL argument in a second
      coll = 1 + Collatz(n/2 , lengths, .true.)
    else
      coll = 1 + Collatz(3*n + 1 , lengths, .true.)
    end if
    ! once we have the sequence length we want, memoise it; but only if we have to (and if it fits)
    ! we assume we don't have to (i.e. the outer program unit is handling the memoisation)
    ! the 'memoise' argument specifies otherwise, i.e. if we want to memoise recursively (which explains the above)
    if ( present(memoise) ) then
      if (memoise) then
        if (n <= size(lengths)) lengths(n) = coll
      end if
    end if
  end function Collatz


end module Functions
