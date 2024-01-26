---
title:                "Searching and replacing text"
date:                  2024-01-20T17:57:46.014842-07:00
model:                 gpt-4-1106-preview
simple_title:         "Searching and replacing text"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text involves finding specific sequences of characters in a string and swapping them with different characters. Programmers do this for everything from fixing typos in huge datasets to automating code refactoring across many files.

## How to:

Go's standard library `strings` has what you need. Here's how to use `strings.Replace`:

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	replacedString := strings.Replace("Hello, Go!", "Go", "World", -1)
	fmt.Println(replacedString) // Output: Hello, World!
}
```

`-1` means replace all instances. To replace only the first instance, use `1` instead.

If you want to do more complex replacements involving patterns, you'll likely use `regexp`:

```go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	regex := regexp.MustCompile(`(Go)`)
	replacedString := regex.ReplaceAllString("Hello, Go! Go is great.", "Gopher")
	fmt.Println(replacedString) // Output: Hello, Gopher! Gopher is great.
}
```

Regex is powerful, but don't overuse it. For simple things, stick with `strings`.

## Deep Dive

Go wasn't the first language to do text replacement, but its standard library is user-friendly. Unix tools like `sed` were handling search-and-replace long before, using regular expressions. Go's `regexp` package gives that power programmatically. 

Compared to other languages, Go trades a bit of raw speed for safety and readability. Other tools and languages might be faster for text processing (such as Perl), but Go's balance between ease of use and performance is a strong suit.

When you're doing search-and-replace in Go, remember:
- `strings` for simple stuff.
- `regexp` for patterns.
- Last argument in `strings.Replace` determines the number of replacements.

## See Also

- Go by Example: String Functions - https://gobyexample.com/strings
- Go by Example: Regular Expressions - https://gobyexample.com/regular-expressions
- Go Package strings - https://pkg.go.dev/strings
- Go Package regexp - https://pkg.go.dev/regexp
