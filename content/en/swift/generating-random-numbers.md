---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:57.474894-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers is a way to produce unpredictable values which is essential in gaming, simulations, and tests. Programmers do it for tasks like shuffling a playlist or simulating dice rolls.

## How to:

Swift makes generating random numbers a breeze. Here's how you do it:

```Swift
// Generate a random number between 0 and 10
let randomNum = Int.random(in: 0...10)
print(randomNum)

// Generate a random Double between 0 and 1
let randomDouble = Double.random(in: 0..<1)
print(randomDouble)
```

Sample output:
```
5
0.874523190457
```

## Deep Dive

Way back when, random number generation was a math puzzle. Today, it's built right into programming languages like Swift. Under the hood, `random(in:)` uses a pseudo-random number generator (PRNG). It's "pseudo" because if you know the algorithm and the seed, you can predict the output — not something you'd want for security-related tasks.

For true randomness, you'd look into cryptographically secure random numbers, something beyond Swift's basic `random(in:)`. On Apple platforms, `SecRandomCopyBytes` provides this level of randomness.

It's worth mentioning that older languages or systems might use functions like `rand()` or `random()`, which have their own quirks and usage patterns.

Alternatives in Swift include using GameplayKit for more control over randomization, with classes like `GKRandomDistribution` and `GKShuffledDistribution`.

## See Also

- The Swift Programming Language guide to random numbers: 
  https://docs.swift.org/swift-book/LanguageGuide/Numbers.html
  
- Apple's documentation on GameplayKit randomization: 
  https://developer.apple.com/documentation/gameplaykit/randomization

- A deeper dive into `SecRandomCopyBytes` for cryptographic randomness: 
  https://developer.apple.com/documentation/security/1399291-secrandomcopybytes