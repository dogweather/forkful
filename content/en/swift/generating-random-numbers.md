---
title:                "Swift recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

📝 Blog Post: Why Generating Random Numbers is Important in Programming

## Why

Generating random numbers is an important task in programming as it allows us to introduce unpredictability and variability in our code. This can be especially useful in applications such as games, simulations, and algorithms.

## How To

To generate random numbers in Swift, we can use the `random` method from the `RandomNumberGenerator` protocol. For example, if we want to generate a random integer between 1 and 100, we can use the following code:

```Swift
let randomNumber = Int.random(in: 1...100)
print(randomNumber)
```

This will produce a different random number each time the code is executed. We can also generate random numbers of different data types, such as `Float`, `Double`, and `Bool`, using the same method.

Another way to generate random numbers is by using the `arc4random` function, which is available in both Swift and Objective-C. This function generates a uniformly distributed random number within a given range. Here's an example of generating a random number between 1 and 10:

```Swift
let randomNumber = Int(arc4random_uniform(10)) + 1
print(randomNumber)
```

## Deep Dive

There are different ways of generating random numbers, each with their own advantages and limitations. One common method is the pseudorandom number generator, which uses a mathematical algorithm to produce seemingly random numbers. However, these numbers are not truly random as the algorithm can be predicted.

Another method is the hardware-based random number generator, which uses physical processes such as thermal noise to generate truly random numbers. This is considered to be more secure and unpredictable compared to pseudorandom generators.

It is important to understand how random numbers are generated in order to properly use them in our code and avoid any security vulnerabilities.

## See Also

- [Official Apple Documentation on Generating Random Numbers in Swift](https://developer.apple.com/documentation/swift/randomnumbergenerating)
- [Tutorial on Generating Random Numbers in Swift 5](https://medium.com/@mallowigi/generating-random-numbers-in-swift-5-fa566336297c)
- [Comparison of Different Methods for Generating Random Numbers](https://medium.com/@phantomofmace/comparing-random-number-generation-methods-7dc7e8a21749)