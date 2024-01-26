---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:19.750653-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
Random numbers in programming are sequences that can't be predicted logically. They're essential for security (think encryption), games, simulations, and tests. They make things unpredictable, fair, or simulate real-world chaos.

## How to: (Як це зробити:)
Here's how to generate random numbers in Go. Go has a built-in "math/rand" package for this.

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano()) // Ensure different outcomes
	fmt.Println("Random Int:", rand.Intn(100)) // Random int up to 100
	fmt.Println("Random Float:", rand.Float64()) // Random float between 0.0 and 1.0
}
```

Sample output might look like this:

```
Random Int: 42
Random Float: 0.812428
```

Numbers will vary every time you run the program.

## Deep Dive (Поглиблений аналіз):
Historically, computing random numbers started as a manual table lookup. Now, we use algorithms like Mersenne Twister (not in Go by default). Go's "math/rand" is fast and good enough for most non-cryptographic tasks. But for crypto, use "crypto/rand" for more unpredictability.

Why not always "crypto/rand"? It's slower, consumes more resources, and often overkill for simple tasks. Seeds (like Unix timestamp we used) shape the sequence, making it seem random. Without a changing seed, you get the same "random" numbers each run – that's pseudo-random for you!

## See Also (Дивіться також):
- Go's "math/rand" package: https://pkg.go.dev/math/rand
- Go's "crypto/rand" for secure randomness: https://pkg.go.dev/crypto/rand
- A deeper dive into pseudo-random vs true random: https://www.random.org/randomness/
- Understanding seeds in random number generation: https://en.wikipedia.org/wiki/Random_seed
