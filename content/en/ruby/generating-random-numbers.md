---
title:                "Generating random numbers"
html_title:           "Ruby recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is a core concept in programming that involves producing a sequence of numbers that appear to be random. Programmers use this to create unpredictability in code, such as randomized outcomes in games or simulations, or to generate unique identifiers.

## How to:

To generate a random number in Ruby, we can use the rand() method. Simply include a range or set of numbers within parentheses to specify the range from which the number will be generated. For example:

```ruby
rand(1..10) # this generates a random number between 1 and 10
```

We can also set a seed value using srand() to ensure that the same sequence of random numbers is generated each time the code is run. For example:

```ruby
srand(1234) # sets the seed value to 1234
rand(1..10) # this will always generate the number 6
```

For more complex randomization, we can use the Random class and its methods. A new instance of Random can be created with:

```ruby
random = Random.new
```

And then we can use methods such as:

```ruby
random.rand(100) # this generates a random number between 0 and 100
```

## Deep Dive:

The concept of generating random numbers dates back to ancient Greece, where they were used in games of chance. In modern programming, there are multiple methods and algorithms for generating random numbers, each with their own advantages and potential flaws.

An alternative to using Ruby's built-in methods is to use a third-party gem, such as Faker, which can generate not only random numbers, but also words, names, and other types of data commonly used in applications.

The implementation of random number generation in Ruby uses the Mersenne Twister algorithm, which is a pseudorandom number generator that is faster and more efficient than other methods. However, this also means that the numbers generated are not truly random, but rather appear to be random.

## See Also:

To learn more about generating random numbers in Ruby, check out the official documentation:
https://ruby-doc.org/core-2.7.1/Random.html

To explore alternative methods for randomization, check out this article:
https://www.freecodecamp.org/news/random-number-generator-in-ruby/