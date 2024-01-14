---
title:    "Ruby recipe: Generating random numbers"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may wonder why anyone would want to generate random numbers. The truth is, random numbers are an essential part of many applications and can have a variety of uses. Whether you're creating a game, performing statistical analysis, or simply creating unique IDs, random numbers have a wide range of applications.

## How To

Generating random numbers in Ruby is a simple and straightforward process. You can use the built-in `rand` method to generate random numbers and specify a range of values. For example:

```Ruby
rand(1..100) #=> will return a random number between 1 and 100
rand(50..200) #=> will return a random number between 50 and 200
```

You can also use the `rand` method to generate random numbers with decimal places. For this, you can specify a floating-point range like so:

```Ruby
rand(0.0..1.0) #=> will return a random decimal between 0 and 1
rand(5.5..10.5) #=> will return a random decimal between 5.5 and 10.5
```

It's worth noting that the `rand` method always generates a pseudo-random number, meaning that the sequence of numbers it produces is deterministic and can be repeated.

You can also use the `srand` method to set a specific seed value for the random number generator. This can be useful if you need to generate the same sequence of random numbers in multiple instances of your program.

## Deep Dive

Behind the scenes, Ruby uses a mathematical formula called a *pseudo-random number generator* (PRNG) to generate seemingly random numbers. This PRNG uses an algorithm to produce a stream of numbers that appear unrelated and unpredictable. However, because the algorithm is deterministic, the numbers it produces are not truly random.

In Ruby, the default PRNG is the Mersenne Twister algorithm, which has a very long period and produces high-quality random numbers. However, as mentioned earlier, the sequence of numbers it produces is not truly random and can be repeated if the same seed value is used.

If you need truly random numbers in your application, you can use external libraries that use physical sources of randomness, such as radioactive decay or atmospheric noise, to generate random numbers.

## See Also

Here are some additional resources for learning more about generating random numbers in Ruby:

- [Ruby `rand` documentation](https://ruby-doc.org/core-3.0.2/Kernel.html#method-i-rand)
- [Ruby `srand` documentation](https://ruby-doc.org/core-3.0.2/Kernel.html#method-i-srand)
- [PRNGs in Ruby from Programming Ruby: The Pragmatic Programmer's Guide](https://ruby-doc.com/docs/ProgrammingRuby/html_toc.html)