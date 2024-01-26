---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:22.261110-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers in programming means creating numbers that are unpredictable and lack any discernible pattern. We do it for stuff like games, simulations, and security where you need unpredictability to shake things up or keep things secret.

## How to:
To generate a random number in Go, pull in the `math/rand` package for pseudorandom numbers, or `crypto/rand` for more secure, cryptographically strong random numbers. Here’s the scoop on using both.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
	"crypto/rand"
)

func main() {
	// Seed the random number generator for math/rand
	rand.Seed(time.Now().UnixNano())

	// Generate a random number between 0 and 99
	myRandNumber := rand.Intn(100)
	fmt.Println(myRandNumber)

	// Generate a cryptographically secure random number
	buff := make([]byte, 1)
	_, err := rand.Read(buff)
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	secureNumber := int(buff[0])
	fmt.Println(secureNumber)
}
```

Sample Output:
```
42
153
```
The numbers you'll see will vary—pretty much the point of this random number shindig.

## Deep Dive
Back in the day, random numbers were literally plucked from hat-draws or dice rolls. In computers, true randomness is tough—hence we often use algorithms for pseudorandomness. Pseudorandom means they look random if you squint, but they’re not truly without pattern. Go’s `math/rand` uses such a deterministic approach, quite alright for basic shuffling or generating a random username.

However, for password generation or encryption keys, we go with `crypto/rand`. These numbers are more unpredictable and safe from patterns that could be exploited. `crypto/rand` uses system sources of entropy such as mouse movement or keyboard timings, making the numbers it generates 'good enough' for cryptography.

An alternative to Go's built-in libraries includes using external randomness sources like random.org, which uses atmospheric noise to generate true random numbers. But for most coding tasks, Go's rand packages will have you covered.

## See Also
Here are some resources for those hungering for more:

- Go’s official package documentation:
  - `math/rand`: [https://pkg.go.dev/math/rand](https://pkg.go.dev/math/rand)
  - `crypto/rand`: [https://pkg.go.dev/crypto/rand](https://pkg.go.dev/crypto/rand)
- For a deep dive on entropy and randomness, check out [Randomness Guide for the Perplexed](https://www.random.org/randomness/)
- If you’re into the mathsy side, have a gander at [Pseudorandom number generation](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
