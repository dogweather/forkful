---
title:                "Deleting characters matching a pattern"
date:                  2024-01-20T17:42:05.772095-07:00
model:                 gpt-4-1106-preview
simple_title:         "Deleting characters matching a pattern"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern is about yanking out specific bits of a string—like pulling weeds from your text garden. Programmers do this for cleanup, formatting, or parsing data, making sure the text is pristine and useful.

## How to:

In Go, we use the `strings` and `regexp` packages. Here's the lowdown with code:

```go
package main

import (
	"fmt"
	"regexp"
	"strings"
)

func main() {
	// Using strings package to remove a set of characters
	str1 := "Hello, 123 World!"
	cleanStr1 := strings.Map(func(r rune) rune {
		if r >= '0' && r <= '9' {
			return -1 // Delete char
		}
		return r // Keep char
	}, str1)

	fmt.Println(cleanStr1) // Output: Hello,  World!

	// Using regexp package to delete characters matching a pattern
	str2 := "Go 1.18 is the current version!"
	re := regexp.MustCompile(`[0-9]+`)
	cleanStr2 := re.ReplaceAllString(str2, "")

	fmt.Println(cleanStr2) // Output: Go . is the current version!
}
```

## Deep Dive

Back in the days of yore, when programming languages were more like arcane spells, pattern matching was a coveted skill. Regular expressions (regex) were the swiss army knife for this job. Go, however, made it easy and efficient, integrating this prowess with the `regexp` package.

Now, why might you not just use `strings.Replace` or `strings.ReplaceAll`? Well, those are fine for simple, static replacements. But when your patterns are as wild as vines in the jungle, regex is where you turn to.

Under the hood, `regexp` compiles a pattern into a state machine. Each character is checked against this machine, and matches are weeded out. This means heavy lifting the first compile, but lightning-fast after.

Alternative methods? You got `bytes.Buffer` for building strings without patterns and `strings.Builder` in newer versions for when you're allergic to unnecessary allocations.

## See Also

The go-to places to further your knowledge:
- Go by Example: Regular Expressions - https://gobyexample.com/regular-expressions
- Go Doc: Package strings - https://pkg.go.dev/strings
- Go Doc: Package regexp - https://pkg.go.dev/regexp
- Regular Expression Playground - https://regex101.com/ (Not Go-specific, but super handy)
