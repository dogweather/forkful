---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:02.715331-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers in Go involves creating numbers that cannot be predicted, serving crucial functions ranging from simulations to security measures. Programmers harness this feature for tasks like generating unique identifiers, passwords, or simulating real-world phenomena in a controlled environment.

## How to:
Go's standard library offers robust support for generating random numbers through the `math/rand` package for pseudo-random numbers and `crypto/rand` for cryptographic secure random numbers. Below, we present examples of how to generate random numbers using both approaches.

### Generating Pseudo-Random Numbers
First, import the `math/rand` package and the `time` package to seed the random number generator, which ensures the numbers are less predictable.

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Seed the generator.
	rand.Seed(time.Now().UnixNano())

	// Generate a pseudo-random number between 0 and 99.
	randNum := rand.Intn(100)
	fmt.Println(randNum)
}
```
Sample output might be: `42`

### Generating Cryptographically Secure Random Numbers
For more security-sensitive applications, use the `crypto/rand` package.

```Go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	// Generate a cryptographically secure random number between 0 and 99.
	num, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println(num)
}
```
Sample output could be: `17`

## Deep Dive
The `math/rand` package in Go implements a pseudo-random number generator (PRNG) based on Donald Knuth's subtractive random number generator algorithm. It's suitable for applications where security isn't a primary concern, such as simulations or games. However, since the sequence can eventually repeat and is determined by the seed value, it's not advised for generating encryption keys or tokens.

On the other hand, `crypto/rand` sources its randomness from the underlying system's entropy pool, offering cryptographic security. This makes it suitable for applications where unpredictability is paramount, such as in cryptographic key generation, secure tokens, or any other use case where a high degree of security is essential.

While `math/rand` is significantly faster and more efficient for generating large quantities of random numbers, `crypto/rand` should be the go-to choice for any security-sensitive operations. Knowing when to use one over the other is crucial in developing secure, efficient, and effective applications in Go.

## See also

### Official Go Documentation
- [Go `math/rand` Package](https://pkg.go.dev/math/rand)

### Tutorials and Guides
- **Medium**: [Generating Random Numbers in Go](https://medium.com/@kpbird/generating-random-numbers-in-go-5bfeae4eaefb)
- **Go by Example**: [Random Numbers](https://gobyexample.com/random-numbers)

### Example Projects and Codes
- **GitHub Repositories**: 
  - [Random Number Generator in Go](https://github.com/aQuaYi/Go-Algorithm/tree/master/Random)
  
- **Stack Overflow Discussions**:
  - [How to Generate a Random Number in Go?](https://stackoverflow.com/questions/12321133/how-to-properly-seed-random-number-generator)
