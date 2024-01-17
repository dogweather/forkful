---
title:                "Generating random numbers"
html_title:           "Swift recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers is a common task in programming where a computer is instructed to produce a series of seemingly unpredictable and unplanned values. Programmers use random numbers for various purposes such as simulations, modeling, and encryption, among others.

## How to:
Random numbers can be generated in Swift using the built-in ```random``` and ```arc4random_uniform``` functions. The ```random``` function generates a random floating-point number between 0.0 and 1.0, while the ```arc4random_uniform``` function generates a random integer within a specified range. For example:
```
let randomFloat = Float.random(in: 0..<10)
//output: 6.973317 

let randomInt = Int(arc4random_uniform(100))
//output: 54
```
## Deep Dive:
Random number generation dates back to the mid-20th century with the development of the first electronic computer. The earliest methods involved using physical devices such as dice, decks of cards, and roulette wheels. Today, computers use algorithms and seed values to produce pseudo-random numbers, which appear random but are actually generated through mathematical calculations.

Apart from the built-in functions in Swift, developers can also use open-source libraries such as SwiftRandom and CryptoSwift for more advanced random number generation. Additionally, for cryptography purposes, cryptographically secure random number generators (CSPRNGs) are recommended.

The implementation of random number generation in Swift is based on the C programming language, which uses the linear congruential generator (LCG) algorithm. However, in Swift 4.2, the implementation was changed to use the PCG-XSH-RR algorithm for improved randomness and performance.

## See Also:
- [Swift documentation on random number generation](https://developer.apple.com/documentation/swift/random)
- [SwiftRandom library](https://github.com/thellimist/SwiftRandom)
- [CryptoSwift library](https://github.com/krzyzanowskim/CryptoSwift)