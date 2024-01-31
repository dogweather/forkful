---
title:                "Concatenating strings"
date:                  2024-01-20T17:34:38.985097-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings is the process of joining two or more strings end-to-end. Programmers do it to build new strings from existing ones, whether for constructing messages, generating dynamic content, or just shaping text to fit the situation.

## How to:
Here's the straightforward way to get strings to stick together in Go.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Using the + operator
	hello := "Hello"
	world := "World"
	result := hello + ", " + world + "!"

	fmt.Println(result) // Output: Hello, World!
	
	// Using fmt.Sprintf
	message := fmt.Sprintf("%s, %s!", hello, world)
	
	fmt.Println(message) // Output: Hello, World!
	
	// Using strings.Builder
	var sb strings.Builder
	sb.WriteString(hello)
	sb.WriteString(", ")
	sb.WriteString(world)
	sb.WriteString("!")
	
	fmt.Println(sb.String()) // Output: Hello, World!
	
	// Using strings.Join for slices
	parts := []string{hello, world}
	combined := strings.Join(parts, ", ")

	fmt.Println(combined + "!") // Output: Hello, World!
}
```

## Deep Dive
Concatenating strings is fairly simple but crucial in programming. Historically, the need for string concatenation has been around since the early days of programming. As languages evolved, so did the methods of string concatenation. In Go, using the `+` operator is the most direct method, but not always the most efficient, especially in a loop.

Alternatives like `fmt.Sprintf` and `strings.Builder` offer more control and efficiency. `fmt.Sprintf` is flexible for formatting, but `strings.Builder` is the go-to for performance, especially when building longer strings from many pieces. Prior to `strings.Builder` (added in Go 1.10), concatenation in loops often led to performance issues due to memory allocation and garbage collection.

Go strings are immutable, and when you use the `+` operator, a new string is created every time. This can lead to memory inefficiency. The advantage of using `strings.Builder` is that it writes to an expandable buffer, minimizing memory allocations.

## See Also
- Official Go blog on strings: https://blog.golang.org/strings
- The `strings` package docs: https://pkg.go.dev/strings
- The `fmt` package docs: https://pkg.go.dev/fmt
- Go Wiki: https://github.com/golang/go/wiki
