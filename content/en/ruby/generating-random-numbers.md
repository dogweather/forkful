---
title:    "Ruby recipe: Generating random numbers"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Why

Generating random numbers may seem like a trivial task, but it is actually quite important in many programming applications. Randomization is a key component in developing algorithms, creating simulations, and implementing data security measures. With Ruby, generating random numbers is made easy with built-in methods and libraries, making it an important skill for any Ruby programmer to have.

## How To

The most basic way to generate a random number in Ruby is by using the `rand` method. This method takes an optional argument as the maximum value for the range of numbers to generate. For example, to generate a random number between 1 and 10, we can use the following code:

```Ruby
rand(10)
```

This will produce a random integer between 0 and 9. If we want to include 10 in the range, we can use the `+1` modifier:

```Ruby
rand(10) + 1
```

We can also generate random decimals by using `rand` with floats as arguments. For example:

```Ruby
rand(5.0)
```

This will generate a random decimal between 0 and 5.

For more complex randomization needs, Ruby also has the `Random` class, which allows for more control over the generated numbers. We can use the `Random` class to set a seed for our random numbers, making them reproducible. We can also specify the range of numbers and the type (integer or float) to be generated. For example, to generate a random integer between 1 and 100, we can use the following code:

```Ruby
Random.new_seed
=> 84837152292236377712433507176753195020279612392060001590259070228014036109730

Random.new.rand(1..100)
=> 51
```

For more information and examples on using the `rand` method and the `Random` class, be sure to check out the [Ruby documentation](https://ruby-doc.org/core-2.7.1/Random.html).

## Deep Dive

Now that we have some basic knowledge on how to generate random numbers in Ruby, let's take a deeper look at what is happening behind the scenes. The `rand` method uses a pseudo-random number generator (PRNG) algorithm to generate the random numbers. This means that the numbers are not truly random but are determined by a fixed sequence of calculations.

One common PRNG algorithm used in Ruby is the Mersenne Twister algorithm, which is known for producing high-quality random sequences. This algorithm uses a seed value to produce a sequence of numbers that appear to be random. However, since the algorithm is deterministic, the same seed value will always produce the same sequence of numbers.

It is important to keep in mind that for some applications, such as cryptography, a truly random sequence of numbers is needed. In these cases, Ruby provides a `SecureRandom` library that uses a cryptographically-secure PRNG algorithm to generate random numbers. This ensures that the numbers cannot be predicted or influenced by outside factors.

## See Also

For more information on using random numbers in Ruby, check out these resources:

- [Ruby documentation on the rand method](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-rand)
- [Ruby documentation on the Random class](https://ruby-doc.org/core-2.7.1/Random.html)
- [Ruby documentation on SecureRandom library](https://ruby-doc.org/stdlib-2.7.1/libdoc/securerandom/rdoc/SecureRandom.html)