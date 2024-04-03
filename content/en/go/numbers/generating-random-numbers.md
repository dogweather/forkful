---
date: 2024-02-03 17:50:07.025730-07:00
description: "Generating random numbers in programming is about creating a sequence\
  \ of numbers that cannot be reasonably predicted better than by chance. Programmers\
  \ do\u2026"
lastmod: '2024-03-13T22:44:59.627263-06:00'
model: gpt-4-0125-preview
summary: Generating random numbers in programming is about creating a sequence of
  numbers that cannot be reasonably predicted better than by chance.
title: Generating random numbers
weight: 12
---

## How to:
In Go, random numbers are generated using the `math/rand` package for pseudo-random numbers or `crypto/rand` for cryptographically secure pseudo-random numbers. Let's explore both.

### Using `math/rand` for Pseudo-random Numbers
First, import the `math/rand` package and the `time` package to seed the generator. Seeding ensures that you get a different sequence of numbers each run.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("A random number:", rand.Intn(100)) // Generates a number between 0 and 99
}
```

Sample output: `A random number: 42`

### Using `crypto/rand` for Cryptographically Secure Pseudo-random Numbers
For more security-sensitive applications, the `crypto/rand` package is suitable as it generates random numbers that are difficult to predict, making them suitable for cryptographic operations.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("A secure random number:", n)
}
```

Sample output: `A secure random number: 81`

## Deep Dive
The core difference between the `math/rand` and `crypto/rand` packages in Go stems from their source of entropy and their intended use cases. `math/rand` generates pseudo-random numbers based on an initial seed; thus, the sequence is deterministic and can be predicted if the seed is known. This is suitable for scenarios where high performance and not absolute unpredictability is the key concern, like simulations or games.

On the other hand, `crypto/rand` derives randomness from the underlying operating system, making it suitable for cryptographic uses where unpredictability is crucial. However, this comes at the cost of performance and complexity in handling the numbers it generates (like dealing with the `*big.Int` type for integers).

Historically, the notion of random number generation in computers has always danced on the edge of true "randomness," with early systems depending heavily on deterministic algorithms that mimicked randomness. As computers evolved, so did these algorithms, incorporating more sophisticated sources of entropy from their environments.

Despite these advancements, the quest for perfect randomness in computing is inherently paradoxical, given the deterministic nature of computers themselves. This is why, for most applications where predictability would be detrimental, cryptographically secure pseudo-random numbers from sources like `crypto/rand` are the better alternative, despite their overhead.

In essence, Go's approach with two distinct packages for random number generation elegantly addresses the trade-offs between performance and security, allowing developers to choose based on their specific needs.
