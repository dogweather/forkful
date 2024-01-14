---
title:                "Go recipe: Extracting substrings"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a common task in programming, and understanding how it works can greatly enhance your skills as a developer. By learning how to extract substrings, you can manipulate strings more efficiently and create more complex programs.

## How To

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Defining a string to extract substrings from
	s := "Hello, world!"

	// Extracting substring from index 0 to 5
	fmt.Println(s[0:5]) // Output: Hello

	// Extracting substring from index 7 to the end of the string
	fmt.Println(s[7:]) // Output: world!

	// Extracting substring from index 7 to 11
	fmt.Println(s[7:11]) // Output: worl
}
```

The code above shows three different ways to extract substrings from a string in Go. The first `Println` statement extracts the substring from index 0 to 5, which includes the letter "l" at index 5. The second `Println` statement extracts the substring from index 7 to the end of the string. Finally, the last `Println` statement extracts the substring from index 7 to 11, excluding the letter "d" at index 11.

It's important to note that strings in Go are immutable, meaning they cannot be changed. Therefore, extracting substrings does not modify the original string, but rather creates a new string.

## Deep Dive

The syntax for extracting substrings in Go is `string[startIndex:endIndex]`. The `startIndex` is inclusive, meaning it will include the character at that index, while the `endIndex` is exclusive, meaning it will not include the character at that index.

If the `startIndex` is omitted, the substring will start from the beginning of the string. Similarly, if the `endIndex` is omitted, the substring will go until the end of the string.

In addition to extracting substrings from a specific index, you can also use the `strings` package in Go to extract substrings based on a specific character or pattern. For example, you can use `strings.Split` to split a string into substrings based on a delimiter.

## See Also

- [Strings and Characters in Go](https://golang.org/ref/spec#Characters)
- [The Go Programming Language Specification](https://golang.org/ref/spec)
- [The Strings package in Go](https://golang.org/pkg/strings/)