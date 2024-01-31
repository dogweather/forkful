---
title:                "Interpolating a string"
date:                  2024-01-20T17:50:49.728925-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolating a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation lets you embed variables into strings. It's handy for creating messages, formatting data, and building SQL queries without a lot of pluses and quotes.

## How to:

In Go, you use the `fmt` package for string interpolation.

```Go
package main

import (
    "fmt"
)

func main() {
    name := "Morgan"
    age := 28
    message := fmt.Sprintf("Hi, my name is %s and I am %d years old.", name, age)
    fmt.Println(message)
}

// Output: Hi, my name is Morgan and I am 28 years old.
```

Use `%s` for strings, `%d` for integers, `%f` for floats. There are more verbs for other types.

## Deep Dive

String interpolation has been a core feature in many languages—Python, Ruby, and more. In Go, it's not part of the language per se but provided via the `fmt` package. This approach gives you better control and safety, especially with type-specific verbs.

Alternatives? Yes—besides `fmt.Sprintf`, there's `fmt.Fprintf` to write to any writer, and `fmt.Printf` to print directly. Pre-Go 1.10 days saw people concatenating strings with `+` or using `bytes.Buffer`. These are still valid but less convenient.

Implementation details? The `fmt` package uses reflection to handle formatting based on the verbs and type of variable. It's efficient but remember that using the wrong verb for a type can lead to runtime errors.

## See Also

- Go's `fmt` package documentation: https://pkg.go.dev/fmt
- Go by Example’s take on string formatting: https://gobyexample.com/string-formatting
- A blog post on string concatenation strategies in Go: https://blog.golang.org/strings
