---
title:    "Swift recipe: Generating random numbers"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why

Generating random numbers is a crucial feature in many applications, whether it's for generating random passwords or creating randomized gameplay experiences. In Swift, there are various methods to generate random numbers that are both efficient and easy to implement.

## How To

To generate a random number in Swift, we can use the `arc4random_uniform()` function. This function takes in an upper limit and returns a random number between 0 and that limit (not including the limit itself). For example, if we want to generate a random number between 1 and 10, we would write the following code:

```Swift
let randomNumber = Int(arc4random_uniform(10)) + 1
print(randomNumber)
```

This would print a random number between 1 and 10.

We can also generate random numbers within a specific range using the `arc4random()` function. This function takes in a range and returns a random number within that range. For example, if we want to generate a random number between 50 and 100, we would write the following code:

```Swift
let randomNumber = Int(arc4random() % 51) + 50
print(randomNumber)
```

## Deep Dive

The `arc4random()` function is a pseudo-random number generator, meaning that it uses a mathematical algorithm to generate seemingly random numbers. However, since it's based on an algorithm, the sequence of numbers it produces can be predicted if the initial state of the algorithm is known. To mitigate this issue, it's best to use cryptographic random number generators in sensitive applications.

Swift provides the `arc4random_buf()` function, which uses the `/dev/random` device on macOS and the `/dev/urandom` device on Linux to generate cryptographically secure random numbers.

## See Also
- [Apple Developer Documentation on Randomization](https://developer.apple.com/documentation/swift/random-numbers)
- [Stack Overflow post on Generating Random Numbers in Swift](https://stackoverflow.com/questions/24007129/how-does-one-generate-a-random-number-in-apples-swift-language)