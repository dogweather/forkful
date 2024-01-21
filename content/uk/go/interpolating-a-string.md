---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:51:19.724509-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## Що й Навіщо?

String interpolation lets you insert values into strings. We do it to build strings dynamically, blend fixed and variable parts seamlessly.

## How to:
## Як це зробити:

```go
package main

import (
	"fmt"
)

func main() {
	name := "Олександр"
	age := 29
	// Basic interpolation with Printf
	fmt.Printf("Привіт, мене звати %s. Мені %d років.\n", name, age)
	
	// Using `Sprintf` to save the result into a variable
	greeting := fmt.Sprintf("І знову привіт, %s!", name)
	fmt.Println(greeting)
}
```

Sample Output:
```
Привіт, мене звати Олександр. Мені 29 років.
І знову привіт, Олександр!
```

## Deep Dive
## Поглиблений Розгляд

In Go's early days, we concatenated strings with the plus operator `+`. This was straightforward but clunky with multiple variables. String interpolation using `Printf`, `Sprintf`, and `Fprintf` from the `fmt` package changed the game: it's flexible and readable. `Printf` outputs to stdout, `Sprintf` returns the string, and `Fprintf` writes to any `io.Writer`.

String interpolation in Go uses verbs like `%s` for strings and `%d` for integers, which are placeholders for your variables. It's static type checking at its best: you get compile-time errors if the format doesn't match the provided data type, preventing many runtime errors.

Alternatives like `strings.Builder` or libraries like `text/template` exist but are overkill for simple cases. Interpolation is usually the clearest and most concise.

## See Also
## Дивіться Також

- Go's fmt package: [https://pkg.go.dev/fmt](https://pkg.go.dev/fmt)
- Effective Go about printing: [https://go.dev/doc/effective_go#printing](https://go.dev/doc/effective_go#printing)
- Go by Example: String Formatting: [https://gobyexample.com/string-formatting](https://gobyexample.com/string-formatting)