---
title:                "Finding the length of a string"
date:                  2024-01-20T17:47:23.031162-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finding the length of a string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string means figuring out how many characters it contains. Programmers do this to validate input, loop through characters, limit output, and more.

## How to:

To get a string length, use `len()`:

```Go
package main

import "fmt"

func main() {
    exampleStr := "Hello, Gophers!"
    length := len(exampleStr)
    fmt.Println(length)  // Output: 14
}
```

For Unicode characters or emojis, `utf8.RuneCountInString()` is your friend:

```Go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    exampleStr := "Hello, 世界!"
    length := utf8.RuneCountInString(exampleStr)
    fmt.Println(length)  // Output: 9
}
```

## Deep Dive
Simply put, Go uses UTF-8 encoded strings. The built-in `len()` function returns the number of bytes, not the number of characters. This is fast but can lead to surprises with multi-byte characters. For accurate character counts, especially in global applications, use `utf8.RuneCountInString()` to correctly handle Unicode. Historically, different languages and libraries counted characters differently, but Unicode has become the standard, and Go's support for it is mandatory in today's diverse coding ecosystem.

As for alternatives, libraries like `unicode/utf8` provide robust handling of runes, which represent Unicode code points. Before Go standardized Unicode handling, programmers had to implement custom solutions, which was error-prone and complex.

In implementation details, strings in Go are immutable sequences of bytes. When handling strings, programmers should be aware of potential performance hits when processing very large strings or when using `utf8.RuneCountInString()` excessively in performance-critical code since it has to decode each rune to count accurately.

## See Also
- The Go Blog on Strings: https://blog.golang.org/strings
- Go `unicode/utf8` package documentation: https://golang.org/pkg/unicode/utf8/
- Go `len` function specification: https://golang.org/ref/spec#Length_and_capacity