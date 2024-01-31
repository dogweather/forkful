---
title:                "Extracting substrings"
date:                  2024-01-20T17:45:43.612240-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracting substrings"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings means snipping out pieces of a string. Programmers do it to isolate, analyze, or manipulate specific bits of data within a larger string.

## How to:
Go makes it a breeze with the standard library and slicing. Here's the low-down:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	phrase := "The quick brown fox jumps over the lazy dog"
	
	// Using slicing
	part := phrase[4:9]
	fmt.Println(part) // Output: quick
	
	// Using strings package
	start := strings.Index(phrase, "brown")
	end := start + len("brown")
	substring := phrase[start:end]
	fmt.Println(substring) // Output: brown
}
```

## Deep Dive
A quick history lesson: Go hit the scene in 2009 as an open-source project to make programming more fun and productive. It kept string manipulation straightforward—no regular expressions needed for simple tasks. Other languages like Python have similar slicing mechanisms.

Sure, there are alternatives like `regexp` and `bytes` package for heavy lifting. However, the basic `Index` function and slicing cover most needs without complication. Under the hood, strings in Go are just slices of bytes. So when you slice a string, you're actually creating a new slice header pointing to the original string's underlying array. This makes substring extraction in Go fast and memory-efficient.

## See Also
- Go's `strings` package: https://pkg.go.dev/strings
- Go Slices: usage and internals: https://blog.golang.org/slices
- Go by Example: Strings: https://gobyexample.com/strings
