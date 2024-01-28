---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:06.807600-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in Swift involves creating numbers that cannot be predicted logically. Programmers use random numbers for a variety of purposes, from adding unpredictability to games and simulations to selecting random elements from collections or generating secure cryptographic keys.

## How to:

Swift provides a straightforward approach to generate random numbers using its standard library. Here’s how to work with different types of random values:

```Swift
// Generate a random Int
let randomInt = Int.random(in: 1...100)
print("Random Int: \(randomInt)")

// Generate a random Double
let randomDouble = Double.random(in: 1.0...100.0)
print("Random Double: \(randomDouble)")

// Generate a random Bool
let randomBool = Bool.random()
print("Random Bool: \(randomBool)")

// Generating a random element from an array
let fruits = ["Apple", "Banana", "Cherry", "Date"]
if let randomFruit = fruits.randomElement() {
    print("Random Fruit: \(randomFruit)")
} else {
    print("Fruits array is empty.")
}
```

Expected output (Note: Your output will vary as these are random values):
```
Random Int: 42
Random Double: 57.890123
Random Bool: true
Random Fruit: Banana
```
The above examples show generating random integers, doubles, bools, and selecting a random element from an array. The range for integers and doubles is specified using `in`, and the `randomElement()` method is a handy way to select a random item from a collection.

## Deep Dive

Historically, generating random numbers in programming languages was often dependent on platform-specific implementations, which varied in quality and performance. In Swift, the introduction of the `RandomNumberGenerator` protocol standardized the approach, offering a flexible and extensible way to generate random numbers. It works under the hood when you use the `random()` methods on data types like `Int`, `Double`, and others.

One of the advantages of Swift's approach is that it abstracts away the complexity of random number generation, making it easy and safe to generate random numbers across different platforms. However, for applications requiring cryptographically secure random numbers, Swift offers the `SecRandomCopyBytes` function from the Security framework, which is more suitable than the standard library methods.

Swift continues to provide robust and convenient methods for randomness, offering a balance between ease of use for general purposes and access to more secure methods for sensitive applications. While the standard library fulfills most needs, exploring third-party libraries or system-specific APIs can be beneficial for specialized requirements.

## See also

### Official Swift Documentation
- [Swift Standard Library - RandomNumberGenerator](https://developer.apple.com/documentation/swift/randomnumbergenerator)

### Tutorials and Guides
- **Ray Wenderlich**: [Random Numbers in Swift](https://www.raywenderlich.com/802-random-numbers-in-swift)
- **Hacking with Swift**: [How to generate random numbers in Swift](https://www.hackingwithswift.com/example-code/language/how-to-generate-random-numbers-in-swift)
