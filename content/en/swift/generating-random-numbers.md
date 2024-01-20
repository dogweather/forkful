---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Random Numbers in Swift: A Concise Guide

## What & Why?

Generating random numbers is about producing unpredictable values. Programmers often use random values to build dynamic effects, run simulations, test systems with diverse data, or create elements of chance in games.

## How to:

In Swift, it's straightforward to generate random numbers. Below are few examples:

1. Random Integers:

```swift
let randomNumber = Int.random(in: 1..<10)
print(randomNumber)
// Output: An unpredictable integer between 1 and 9.
```
2. Random Floating-Point Number:

```swift
let randomFloat = Float.random(in: 1..<2)
print(randomFloat)
// Output: An unpredictable float between 1 and less than 2.
```
3. Random Boolean:

```swift
let randomBool = Bool.random()
print(randomBool)
// Output: Either true or false.
```
## Deep Dive

Historically, generating random numbers in programming languages was less standardized and often required external libraries. However, Swift makes it easy without any add-ons.

There are alternatives to `random(in:)`. For example, using `arc4random_uniform(_:)` can specify an upper limit.

```swift
let randomArc = Int(arc4random_uniform(10))
print(randomArc)
// Output: An unpredictable integer between 0 and 9.
```
Note that `arc4random_uniform(_:)` creates a bias when the upper limit isn't a power of two. For balanced randomness, use Swift's native `random(in:)`.

About implementation, Swift's randomness is cryptographically secure, using a source that's hard to predict and reproduce.

## See Also

- Understanding random number generation and Swift’s new random API, [hackingwithswift.com](https://www.hackingwithswift.com/articles/73/understanding-random-numbers-in-swift)