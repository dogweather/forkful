---
title:                "Rounding numbers"
date:                  2024-01-25T03:00:04.583051-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rounding numbers"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Rounding numbers means tweaking a number to its nearest whole or specified decimal place. It's done to simplify values, make them more readable, or fit them into certain constraints, like when working with currencies.

## How to:
Go's `math` package is your buddy for rounding. Use `math.Round`, `math.Floor`, and `math.Ceil` for simplicity:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // Round to nearest whole number
	fmt.Println("Floor:", math.Floor(number)) // Round down
	fmt.Println("Ceil: ", math.Ceil(number))  // Round up
}
```

Sample output:
```
Round: 3
Floor: 3
Ceil: 4
```

For specific decimal places, multiply, round, then divide:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Rounded to 2 decimal places:", roundToDecimalPlace(number, 2))
}
```

Sample output:
```
Rounded to 2 decimal places: 3.14
```

## Deep Dive
Rounding numbers isn't new—it dates back to ancient math, always aiming for simplicity. The `math.Round` in Go uses [bankers' rounding](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even), meaning 0.5 rounds to the nearest even number, reducing a bias that could affect sums.

Floating-point numbers can be tricky due to their binary representation, which might not exactly represent all decimals. Go's approach, however, maintains expected behavior most of the time.

Other rounding methods exist, like "round half up" or "round half away from zero," but Go's standard library is what's readily available. For more complex needs, you might need a third-party library or roll your own solution.

## See Also
- Go's `math` package: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- IEEE 754 standard for floating-point arithmetic (Go's basis for handling floats): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Understanding floating-point: ["What Every Computer Scientist Should Know About Floating-Point Arithmetic"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
