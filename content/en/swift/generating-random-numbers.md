---
title:    "Swift recipe: Generating random numbers"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why

Generating random numbers is a crucial aspect of programming, especially in game development, cryptography, and simulations. Random numbers help in introducing unpredictability and variety into our code, making it more versatile and realistic.

## How To

To generate random numbers in Swift, we use the `arc4random_uniform()` function. This function takes in an upper limit and produces a random number between 0 (inclusive) and the upper limit (exclusive). 

```Swift 
let number = arc4random_uniform(100) 
print("Random number between 0 and 99: \(number)") 
```

We can also generate random numbers within a specific range by using the `arc4random()` function and the `%` modulo operator. The `arc4random()` function produces a random number between 0 and `UInt32.max` (the maximum value for an unsigned 32-bit integer). The `%` operator then helps us to limit the range to our desired values.

```Swift 
let number = arc4random() % 50 + 10 
print("Random number between 10 and 59: \(number)") 
```

Another way to generate random numbers in Swift is by using the `random()` method. This method is part of the `RandomNumberGenerator` protocol and provides more flexibility in generating random numbers. 

```Swift 
import Foundation 

let number = Int.random(in: 1...10) 
print("Random number between 1 and 10: \(number)") 
```

## Deep Dive 

Random numbers are not truly random but rather pseudo-random. This means that they appear to be random, but are actually generated based on a specific algorithm and a seed value. In Swift, the `arc4random()` function uses the Xoroshiro128+ algorithm to generate pseudo-random numbers. This algorithm is known for its speed and quality of randomness.

It is important to note that for security and cryptographic purposes, we should use the `arc4random_buf()` function instead of `arc4random()` as it uses a more secure algorithm.

## See Also 

- [Swift Documentation on Random Numbers](https://developer.apple.com/documentation/swift/random) 
- [NSHipster Article on arc4random](https://nshipster.com/arc4random/) 
- [Ray Wenderlich Tutorial on Random Number Generation in Swift](https://www.raywenderlich.com/3293-random-numbers-in-swift)