---
title:                "Swift recipe: Generating random numbers"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why 
Generating random numbers is a fundamental aspect of programming and can be useful in a variety of applications. It can add an element of randomness to games, simulations, and data analysis, making them more realistic and accurate.

## How To 
To generate random numbers in Swift, we can use the built-in `random()` method from the `Int` or `Double` data types. For example, to generate a random integer between 1 and 10, we can use the following code: 

```Swift
let randomNumber = Int.random(in: 1...10) 
print(randomNumber) //output: 7
```

To generate a random decimal number between 0 and 1, we can use the `Double.random()` method:

```Swift
let randomDecimal = Double.random(in: 0...1) 
print(randomDecimal) //output: 0.6270913786
```

We can also use the `arc4random()` method from the `UInt32` data type to generate random numbers within a range:

```Swift
let randomNumber = Int(arc4random_uniform(100)) 
print(randomNumber) //output: 42
```

## Deep Dive 
Behind the scenes, the `random()` and `arc4random()` methods use algorithms to generate pseudorandom numbers. This means that while the numbers may appear random, they are actually determined by a predetermined sequence. 

To ensure a truly random sequence, we can use the `GKRandom` class from the `GameplayKit` framework. It uses a more complex algorithm and can generate numbers based on various distributions, such as Gaussian and binomial. 

```Swift
let randomGenerator = GKMersenneTwisterRandomSource() 
let randomNumber = randomGenerator.nextInt(upperBound: 50) 
print(randomNumber) //output: 28
```

## See Also 
For more information on generating random numbers in Swift, check out the official documentation and the following resources:

- [Generating Random Numbers in Swift](https://learnappmaking.com/random-numbers-swift-app-development/)
- [Intro to Random Numbers in Swift: Part 1](https://blog.usejournal.com/intro-to-random-numbers-in-swift-part-1-c830873672f8)
- [Randomness in Swift](https://www.swiftbysundell.com/basics/randomness/)