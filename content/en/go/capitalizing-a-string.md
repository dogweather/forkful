---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string turns the first letter of a given string into an uppercase letter. Programmers do this for formatting output, adhering to grammatical rules, or making text more readable.

## How to:
In Go, strings are immutable, so you need to create a new capitalized version of the string. We use the `strings` package and its `Title` function or manipulate the string runes directly:

```Go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	// Example 1: Using strings.Title to capitalize every word
	fmt.Println(strings.Title("hello world!")) // Outputs: Hello World!

	// Example 2: Capitalizing only the first character
	input := "hello again!"
	if len(input) > 0 {
		fmt.Println(strings.ToUpper(string(input[0])) + input[1:]) // Outputs: Hello again!
	}

	// Example 3: More robust capitalization, handling multi-byte characters
	capitalizeFirst := func(s string) string {
		for i, v := range s {
			return string(unicode.ToUpper(v)) + s[i+utf8.RuneLen(v):]
		}
		return ""
	}

	fmt.Println(capitalizeFirst("привет мир!")) // Outputs: Привет мир!
}
```

## Deep Dive
String capitalization isn't a complicated process, but there's a lot going on under the hood. Before the `strings.Title` function existed, you had to manipulate strings at the rune level for proper capitalization.

In older programming languages, handling non-ASCII characters while capitalizing was tricky due to the lack of proper Unicode support. Go makes it easier with built-in support for UTF-8 encoding in the `unicode` and `utf8` standard packages.

When manually capitalizing strings in Go, remember to handle multi-byte characters. That’s why we loop through the string using `range` in the robust example, which iterates over runes instead of bytes.

There are alternatives to the built-in Go methods, such as using third-party libraries for more complex text manipulation needs. However, for simple capitalization, Go's standard library is usually sufficient.

## See Also
- Go strings package: [https://golang.org/pkg/strings/](https://golang.org/pkg/strings/)
- Go unicode package: [https://golang.org/pkg/unicode/](https://golang.org/pkg/unicode/)
- Go utf8 package: [https://golang.org/pkg/unicode/utf8/](https://golang.org/pkg/unicode/utf8/)
- A cool article on strings and runes in Go: [https://blog.golang.org/strings](https://blog.golang.org/strings)
