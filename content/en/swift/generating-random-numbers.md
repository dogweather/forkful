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

## Why
Random numbers are an essential tool in programming and are often used for tasks such as generating unique identifiers or simulating unpredictable events. Swift provides built-in functionality for generating random numbers, making it an easily accessible and convenient tool for developers.

## How To
Generating a random number in Swift is a straightforward process. We can use the ```Int``` or ```Double``` data types to store whole numbers or floating-point numbers, respectively.

To generate a random ```Int``` between a given range, we can use the ```random(in:)``` method. For example, if we want to generate a random number between 1 and 100, we can use the following code:

```Swift
let randomNumber = Int.random(in: 1...100)
```

To generate a random ```Double```, we can use the ```Double.random(in:)``` method. For example, if we want a random decimal number between 0 and 1, we can use the following code:

```Swift
let randomDecimal = Double.random(in: 0...1)
```

We can also use the built-in ```arc4random_uniform(_:)``` function to generate a random ```UInt32``` number within a given range. This function returns a randomness level of n/1. Each possible result has an equal chance of being chosen. For example, to generate a random number between 1 and 10, we can use the following code:

```Swift
let randomInteger = arc4random_uniform(10) + 1
```

This will return a random number between 1 and 10, inclusive.

## Deep Dive
Behind the scenes, Swift uses the Mersenne Twister algorithm to generate random numbers. This algorithm is a pseudorandom number generator, meaning that while the numbers may appear random, they are actually based on a predetermined sequence. However, this sequence is so long and complex that it is virtually impossible to predict or determine the next number in the sequence, making it a reliable source for generating random numbers.

It is also important to note that the default random number generator in Swift is a global one, meaning that it will generate the same set of random numbers each time the program is run. If you want to have a different set of numbers every time, you can create a custom instance of ```RandomNumberGenerator``` and use it to generate random numbers.

## See Also
- [Apple Developer Documentation: Generating Random Numbers](https://developer.apple.com/documentation/swift/int/2995642-random)
- [Swift by Sundell: Generating Random Values in Swift](https://www.swiftbysundell.com/basics/random-values/)
- [Fluffy.es: Understanding Random Numbers in Swift](https://fluffy.es/understanding-random-numbers/)
- [Hacking with Swift: Random numbers in Swift](https://www.hackingwithswift.com/articles/189/how-to-use-random-in-swift-4)