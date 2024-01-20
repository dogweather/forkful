---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers is the process of producing numbers in an unpredictable pattern. Programmers do this primarily for tasks like creating unique identifiers, adding unpredictability in games, or for security and encryption purposes.

## How to:
Here's a simple example of generating random integers and floats in Go.

```Go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // Seed the random number generator
    rand.Seed(time.Now().UnixNano())
    
    // Generate a random integer
    randomInt := rand.Int()
    fmt.Println(randomInt)
    
    // Generate a random float
    randomFloat := rand.Float64()
    fmt.Println(randomFloat)
}
```
This will generate and print out one random integer and one random float every time you run the program.

## Deep Dive
Historically, pseudo-random number generation was a common strategy. But these are detereministic and repeat after a period. Modern times require more randomness, hence systems like `/dev/random` in Unix-based systems or CryptGenRandom in Windows were born. 

In Go, `math/rand` package is often used. The function `rand.Int()` generates a pseudo-random number but the sequence would be the same every time you run the program. Hence, always use `rand.Seed()` to initialize the generator to a distinct state. The seed value usually comes from the current time, via `time.Now().UnixNano()`.

For crypto-strong random numbers, we use the `crypto/rand` package. But this comes with a performance trade-off.

Alternatives include using third-party libraries such as `gosecure/vose`.

## See Also
- The math/rand package https://pkg.go.dev/math/rand
- A good article on Go rand: https://yourbasic.org/golang/generate-random-number/
- For crypto-strong randomness: https://pkg.go.dev/crypto/rand
- The gosecure/vose library: https://github.com/gosecure/vose