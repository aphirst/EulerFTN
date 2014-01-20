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

module Euler
  use Functions
  implicit none

contains

  subroutine Problem_1
    ! If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9.
    ! The sum of these multiples is 23.
    !
    ! Find the sum of all the multiples of 3 or 5 below 1000.
    implicit none

    integer(i8)              :: limit, N, i
    integer(i8), allocatable :: factors(:), wheel(:), rungsums(:)
    real                     :: time(2)

    print *, 'Problem #1: Find the sum of all the multiples of <FACTORS> below <INPUT>.'
    print *, 'Defaulting to factors 3 and 5.'
    factors = [3,5]
    ! generate the modular arithmetic 'wheel' using FACTORS
    wheel = Multiples_wheel(factors)
    print *, 'Please input the EXCLUSIVE upper bound for the sum:'
    read *, limit
    call cpu_time(time(1))
    ! each run around the wheel is a 'rung'
    ! the spokes of later rungs are the spoke PLUS the number of completed rungs
    ! we will sum spokes for each rung up to (and including) the rung containing LIMIT
    ! however we want to exclude LIMIT itself and anything greater ("factors BELOW <limit>.")
    ! the N = (limit/max(wheel))th (rounded down) rung is the last necessarily-complete one
    ! excluding LIMIT is the same as including LIMIT-1
    N = (limit-1)/ maxval(wheel)
    ! for now, sum all these 'complete' rungs
    ! sum(wheel) + sum(wheel + maxval(wheel)) + sum(wheel + 2*maxval(wheel) + ...
    ! + sum(wheel + (N-1)*maxval(wheel))
    ! this is like a sum from 0 to N-1; and there'll be an extra N term for the partial rung
    allocate(rungsums(0:N))
    do concurrent (i = 0:N-1)
      rungsums(i) = sum( wheel + i*maxval(wheel) )
    end do
    ! now to handle that final rung separately, so as to not waste CPU cycles on if() statements
    ! the wheel value on that final rung which gives us LIMIT is LIMIT%maxval(wheel)
    ! we'll make use of the 'mask' argument for sum() so that we sum over the right amount of wheel
    rungsums(N) = sum( wheel + N*maxval(wheel), wheel <= modulo(limit-1,maxval(wheel)) )
    print *, 'The sum of the multiples of the desired factors is:'
    print *, sum(rungsums)
    call cpu_time(time(2))
    print '(a,f0.3,a)', 'Took',time(2)-time(1),' seconds'

  end subroutine Problem_1

  subroutine Problem_2
    ! Each new term in the Fibonacci sequence is generated by adding the previous two terms.
    ! By starting with 1 and 1, the first 10 terms will be:
    ! 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, ...
    !
    ! By considering the terms in the Fibonacci sequence whose values do not exceed four million,
    ! find the sum of the even-valued terms.
    implicit none

    integer(i8) :: fibmax, nmax, n
    real        :: time(2)

    print *, 'Problem #2: Find the sum of the even-valued terms of the Fibonacci sequence,'
    print *, 'using only the terms which do not exceed <INPUT>.'
    print *, ''
    print *, 'Please input the INCLUSIVE upper bound for the sum:'
    read *, fibmax
    call cpu_time(time(1))
    ! determine index 'nmax' of first F_n STRICTLY BELOW fibmax
    nmax = Inv_fib(fibmax)
    ! by inspection, only the (3n)th Fibonacci numbers are even
    print *, sum( [( fib(n), n=3,nmax,3 )] )
    call cpu_time(time(2))
    print '(a,f0.3,a)', 'Took ',time(2)-time(1),' seconds'

  end subroutine Problem_2

  subroutine Problem_3
    ! The prime factors of 13195 are 5, 7, 13 and 29.
    ! What is the largest prime factor of the number 600851475143 ?
    implicit none

    integer(i8)              :: max_prime, to_factorise, i ! up to 9223372036854775807 for me
    integer(i8), allocatable :: primes(:), wheel(:), factors(:)
    real(dp)                 :: time(2)

    print *, 'Problem #3: What is the largest prime factor of the number <INPUT>.'
    print *, ''
    print *, 'Please input the number to factorise:'
    read *, to_factorise
    if (to_factorise <= 0) stop 'Invalid: please input a positive integer.'
    print *, 'What is the largest (prime) number to use for the factorisation wheel?'
    print *, 'Note: any higher than 13 generates too large a wheel for most machines.'
    read *, max_prime
    call cpu_time(time(1))
    ! use simple prime sieve to generate first few primes
    primes = Primes_sieve(max_prime)
    ! generate a 'wheel' using those primes
    wheel = Primes_wheel(primes)
    ! use that wheel to determine all the prime factors of 'to_factorise'
    factors = Prime_factors(to_factorise, primes, wheel)
    call cpu_time(time(2))
    print *, 'The complete list of prime factors is:'
    ! this is a bit awkward, but it does print it out nicely
    write(*,'(a)',advance='no') '[ '
    do i = 1, size(factors)-1
      write(*,'(i0)',advance='no') factors(i)
      write(*,'(a)',advance='no') ', '
    end do
    print '(i0,a)', factors(size(factors)),' ]'
    print '(a,i0)', 'and the largest prime factor is ',factors(size(factors)) ! generated in ascending order
    print '(a,f0.3,a)', 'Took: ',time(2)-time(1),' seconds'

  end subroutine Problem_3

  subroutine Problem_4
    ! A palindromic number reads the same both ways.
    ! The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 * 99.
    ! Find the largest palindrome made from the product of two 3-digit numbers.
    implicit none

    integer(i8) :: n, half, div, min_div, max_div, current_pal
    real(dp)    :: time(2)

    print *, 'Problem #4: What is the largest palindromic number made from the product of two <INPUT>-digit numbers?'
    print *, ''
    print *, 'Please input the number of digits for both these factors:'
    read *, n
    ! make sure that the resulting palindrome will fit in the integer KIND being used
    if ( real(2*n) >= log10(real( huge(n) )) ) then
      stop 'Too many digits, product irrepresentable as current integer'
    end if
    ! start the computation proper
    call cpu_time(time(1))
    max_div = (10**n) - 1
    outer: do half = (10**n)-3, 10**(n-1), -1
      current_pal = Palindrome(half)
      ! we only have to check against divisors that could give n-digit co-divisors
      min_div = current_pal / max_div
      ! even-digit (2n) palindromes are all divisible by 11
      ! therefore we only have to check divisors also divisible by 11
      inner: do div = max_div - modulo(max_div,11_i8), min_div, -11
        !print *, div
        if ( modulo(current_pal,div) == 0 ) then
          ! we're done
          exit outer ! so we break out of the outer loop as well
        end if
      end do inner
    end do outer
    call cpu_time(time(2))
    !print *, 'max:',max_div - modulo(max_div,11_i8),' min:', min_div, div
    print *, 'The largest such palindrome is:'
    print '(i0,a,i0,a,i0)', current_pal,' = ',div,' * ',current_pal/div
    print '(a, f0.3, a)', 'Took: ',time(2)-time(1),' seconds'

  end subroutine Problem_4

  subroutine Problem_5
    ! 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
    ! What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
    implicit none

    integer(i8)              :: i, limit
    integer(i8), allocatable :: divisors(:)
    real(dp)                 :: time(2)

    print *, 'Problem #5: What is the smallest positive number that is evently divisible by all the numbers from 1 to <INPUT>?'
    print *, ''
    print *, 'Upper bound (inclusive)?'
    read *, limit
    divisors = [( i, i=1,limit )]
    call cpu_time(time(1))
    print '(i0)', Lowest_common_multiple( divisors ) ! 232792560
    call cpu_time(time(2))
    print '(a,f0.3,a)', 'Took: ',time(2)-time(1),' seconds'

  end subroutine Problem_5

  subroutine Problem_6
    ! The difference between the sum of the squares and the square of the sums of the first ten natural numbers is 2640.
    ! Find the difference between the sum of the squares and the square of the sums for the first hundred natural numbers.
    implicit none

    integer(i8) :: n

    print *, 'Problem #6: Find the difference between the sum of the squares and the square of the sums &
      &for the first <INPUT> natural numbers.'
    print *, ' '
    print *, 'Upper bound (inclusive)?'
    read *, n
    ! the order here ensures no division rounding, and minimises the chance of integer overflow
    print '(i0)', ( ( (n*(n+1))/2 ) * ( ((n-1)*(3*n + 2))/2 ) ) / 3 ! fails somewhere over N = 50000

  end subroutine Problem_6

  subroutine Problem_7
    ! By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
    ! What is the 10001st prime number?
    implicit none

    integer(i8), allocatable :: primes(:)
    integer(i8)              :: n
    real                     :: time(2)

    print *, 'Problem #7: Find the <INPUT>th prime number.'
    print *, ' '
    print *, 'Which prime number should we find?'
    read *, n
    call cpu_time(time(1))
    primes = Primes_sieve(Largest_pn(n))
    call cpu_time(time(2))
    print '(a,i0,a,i0)', 'The ',n,'th prime number is ',primes(n)
    print '(a,f0.3,a)', 'Took: ',time(2)-time(1),' seconds'

  end subroutine Problem_7

  subroutine Problem_8
    implicit none
  end subroutine Problem_8

  subroutine Problem_10
    ! The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
    ! Find the sum of all the primes below two million.
    implicit none

    integer(i8) :: n, the_sum
    real        :: time(2)

    print *, 'Problem #10: Find the sum of all the primes below <INPUT>.'
    print *, ''
    print *, 'Upper bound (exclusive)?'
    read *, n
    call cpu_time(time(1))
    the_sum = sum(Primes_sieve(n-1))
    call cpu_time(time(2))
    print '(a,i0)', 'The sum is ',the_sum
    print '(a,f0.3,a)', 'Time: ',time(2)-time(1),' seconds'

  end subroutine Problem_10

  subroutine Problem_14
    implicit none

    integer(i8)              :: i, n, max_n, max_length
    integer(i8), allocatable :: lengths(:)
    real(dp)                 :: time(2)

    print *, 'How high?'
    read *, n
    lengths = [( 0_i8, i=1,n )]
    call cpu_time(time(1))
    do i = 1, n
      if (lengths(i) == 0) lengths(i) = Collatz(i,lengths)
    end do
    max_length = maxval(lengths)
    max_n = sum(maxloc(lengths))
    call cpu_time(time(2))

    print '(a,i0,a,i0,a)', 'Longest sequence starts at ',max_n,', and is ',max_length,' long.'
    print '(a,f0.3,a)', 'Took: ',time(2) - time(1),' seconds'

  end subroutine Problem_14

end module Euler