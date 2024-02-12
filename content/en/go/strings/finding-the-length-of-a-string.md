---
title:                "Finding the length of a string"
aliases:
- /en/go/finding-the-length-of-a-string.md
date:                  2024-02-03T17:50:05.140580-07:00
model:                 gpt-4-0125-preview
simple_title:         "Finding the length of a string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string in Go is about determining the number of characters it contains. Programmers routinely perform this operation to manipulate strings effectively, be it for validation, substring extraction, or simply to enforce constraints in user inputs.

## How to:
In Go, strings are treated as immutable bytes sequences. You can find the length of a string using the built-in `len()` function which returns the number of bytes, not necessarily the number of characters. Here's how to use it:

```go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	// Using len() to find the byte length
	str := "Hello, 世界"
	byteLength := len(str)
	fmt.Println("Byte Length:", byteLength) // Output: Byte Length: 13

	// To accurately get the number of characters or runes in a string
	runeLength := utf8.RuneCountInString(str)
	fmt.Println("Rune Length:", runeLength) // Output: Rune Length: 9
}
```
The first method using `len()` might not always give the expected result since it counts bytes. For strings containing non-ASCII characters (like "世界"), `RuneCountInString` from the `unicode/utf8` package should be used instead to count Unicode code points accurately.

## Deep Dive
Before Go 1, there was no strict demarcation for handling strings as sequences of bytes versus sequences of characters. Post Go 1, the adoption of UTF-8 as the standard encoding scheme for strings necessitated clearer approaches. The `len()` function works perfectly for ASCII strings, where characters are represented in a single byte. However, as Go applications became more global, and the need to support a plethora of languages and character sets grew, the simplistic approach of `len()` showed limitations.

The introduction and use of `utf8.RuneCountInString()` answer these limitations by providing a way to count actual Unicode characters (runes in Go terminology). This method ensures that the length calculation is independent of the encoding specifics of UTF-8, where characters might span multiple bytes.

An alternative approach for traversing and manipulating strings, more in line with Go’s concurrency and efficiency ethos, might involve treating strings as slices of runes. However, this method necessitates a conversion step and doesn't instantly solve all the intricacies of Unicode (e.g., combining characters).

In summary, while `len()` is suitable for byte length and is efficient for ASCII text, `utf8.RuneCountInString()` is a more reliable choice for a globally compatible application. Yet, developers are encouraged to understand the trade-offs in performance and memory usage that these choices entail.
