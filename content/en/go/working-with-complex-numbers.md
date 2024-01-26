---
title:                "Working with complex numbers"
date:                  2024-01-25T03:00:11.131916-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with complex numbers"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Complex numbers, made up of a real and an imaginary part (like 5 + 7i), are crucial in fields like engineering, physics, and signal processing. Programmers work with them to solve problems in these domains that would be tough to crack with just real numbers.

## How to:
Go has built-in support for complex numbers. Here’s a quick run-through:

```go
package main

import (
	"fmt"
	"math/cmplx"
)

func main() {
	// Creating complex numbers
	a := complex(2, 3)
	b := 4 + 5i

	// Basic operations
	fmt.Println("Addition:", a+b)
	fmt.Println("Subtraction:", a-b)
	fmt.Println("Multiplication:", a*b)
	fmt.Println("Division:", a/b)

	// Complex number properties
	fmt.Println("Real part:", real(b))
	fmt.Println("Imaginary part:", imag(b))
	fmt.Println("Conjugate:", cmplx.Conj(b))
	fmt.Println("Magnitude:", cmplx.Abs(b))
	fmt.Println("Phase angle (radians):", cmplx.Phase(b))
}

```

Sample output:

```
Addition: (6+8i)
Subtraction: (-2-2i)
Multiplication: (-7+22i)
Division: (0.5609756097560976+0.0487804878048781i)
Real part: 4
Imaginary part: 5
Conjugate: (4-5i)
Magnitude: 6.4031242374328485
Phase angle (radians): 0.8960553845713439
```

## Deep Dive
Way back, complex numbers were viewed with suspicion—some thought they were useless! Over time, their power in describing physical phenomena became clear. They're fundamental in quantum physics, control theory, and electrical engineering, to name a few areas. 

In Go, complex numbers are represented using a data type called `complex128` (64 bits for real and imaginary part each) or `complex64` (32 bits each). Under the hood, these are really just two `float64`s or `float32`s stuck together. Go's standard library, `math/cmplx`, offers functions for complex math operations. This saves you from the gritty math and lets you focus on solving problems.

Alternatives to Go's built-in support include using external libraries or rolling your own complex number handling. But these are rarely needed because Go's native support is efficient and well-integrated into the language.

## See Also
Check out these links for more on Go’s complex number capabilities:
- Go's official documentation: https://golang.org/pkg/math/cmplx/
- A deeper math refresher on complex numbers: https://www.mathsisfun.com/numbers/complex-numbers.html
- Practical applications of complex numbers in engineering: https://ieeexplore.ieee.org/document/528dunno
