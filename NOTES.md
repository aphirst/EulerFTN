Solution Notes
==============


Problem 1
---------

I think I overengineered this one a little. I'd just learned about wheel factorisation, so I thought I'd apply a similar principle here. Rather than requiring tons of multiplications or divisions, you just loop over a little look-up "wheel" and add stuff together. Well, OK, there's the edge case of the final "rung" of the wheel, but all in all it's just liberal use of SUM.

As such, the runtime is negligible.


Problem 2
---------

The two tricks here are

1. Use an analytic expression for the nth Fibonacci number (taking care of at what point in the sequence you define ```Fib(n) == 2```)
2. Spot that only every 3rd Fibonacci number is even (think about whether the sums of odd and even numbers will be even, and how that carries-over to the way the Fibonacci sequence is defined)


Problem 3
---------

Long story short, I implemented a wheel factorisation algorithm. The clever part is arranging it all in terms of a function which gets the "next" factor, as if you're progressively finding factors and dividing by the new factor until you reach 1. From there, the optimisations are focused on placing strict mathematical bounds on the "potential factors" you need to bother to test against, beyond the trivial sqrt(N). For instance, since you're generating factors in ascending order, once your "to factorise" number is lower than the most recent (i.e. largest so far) factor, clearly there are no more factors to be found.

This one's a little involved, but the comments in the relevant parts of ```functions.f90``` and ```euler.f90``` should make things crystal clear. I'm pretty satisfied with how fast I've gotten this though, and the "next_factor" routine even doubles as a reasonably efficient "is prime?" function.


Problem 4
---------

I initially did this by generating products and seeing if they were palindromic. Only after a lot of (unnecessary) testing, and a little (belated) thought, did I realise that this should be done the other way round (i.e. generate 2n-digit palindromes and then test by division). Otherwise, the only real improvements were making sure to generate in *descending* order, and to spot that any even-digit palindromic number had to be divisible by 11 (and therefore, that at least one of the divisors has to be, eliminating 10/11 tests most of the time).


Problem 5
---------

It took a bit of pratting to decide exactly how I wanted to do this one, and if I'm honest, I want to rewrite it. But what it does at the moment is, basically
* generates an array of all the prime factors of the numbers to work out the LCM of
* determines the maximum multiplicity of each prime factor with respect to the set of the numbers to work out the LCM of

Apologies for the awkward wording there. The generation of the prime-factors also needed a quick-and-dirty Quicksort and duplicate-removing function. The logic for this is much clearer in the code than I could explain briefly.

The reason I want to re-implement this is that I want to generate the multiplicity at the same time as working out the prime factors of each "to LCM" number, rather than having to essentially work out the prime factors twice. I'm sure I'll get round to it eventually.


Problem 6
---------

You can work out a closed-form expression for this. *Spotting* it for n and n^2 isn't too hard (comparing the differences along the sequence, for instance), but actually doing it using formal summation notation is [somewhat](http://www.trans4mind.com/personal_development/mathematics/series/sumNaturalNumbers.htm#mozTocId571647) [harder](http://www.trans4mind.com/personal_development/mathematics/series/sumNaturalSquares.htm#summation). You can then simply combine and rearrange.

The other trick is explicitly arranging the computation so that you avoid rounding (e.g. ensuring that divisions by 2 only occur on expressions which you can prove will always have no remainder), and avoid overflow (e.g. making sure the expressions which have *large* results are evaluated on sub-expressions which are first made as small as possible).


Problem 7
---------

I looked up a closed-form expression for the upper-bound of the nth prime number; and all the code has to do is sieve up to that, and just return the nth remaining entry. The hard work here was optimising the prime sieve. I've not done anything too outlandish, but I did re-map the "meaning" of the *indices* of the sieve to automatically account for the fact that you can ignore all even *numbers*, along with the corresponding logic behind the array-index notation used to concisely perform the actual sieveing.

I don't look forward to the day I need to implement segmented sieves or the like.


Problem 10
---------

This is quite literally as simple as sieving up to your upper bound, and SUMming the resulting array. Trivial.


Problem 14
----------

The recursive Collatz function memoises past results to make things faster. I should probably rethink the logic behind the OPTIONAL boolean argument (I suspect that simply the presence of a memo array should be enough to indicate that we want to use it), and how that ends up affecting how and when you need to store results.
