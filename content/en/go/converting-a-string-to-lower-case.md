---
title:                "Converting a string to lower case"
date:                  2024-01-20T17:38:23.521093-07:00
model:                 gpt-4-1106-preview
simple_title:         "Converting a string to lower case"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lower case means transforming all alphabetical characters in the text to their lower case equivalent. Programmers do this for consistency, especially in case-insensitive comparisons, data normalization, and to prevent duplicate entries differing only in case.

## How to:
In Go, use `strings.ToLower` to convert a string to lower case. Here's how:

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "Hello, World!"
	lower := strings.ToLower(original)
	fmt.Println(lower) // Output: hello, world!
}
```
Run the code. The output is the lowercased version of the string.

## Deep Dive
The concept of case conversion has been around as long as there have been upper and lower case letters. Go handles this with the `strings` package, providing a simple, efficient way to transform strings.

Alternatives? Sure. You could iterate over each character and check its case manually, but why reinvent the wheel?

Implementation wise, `ToLower` is more complex under the hood than it appears. It's aware of Unicode and correctly handles characters beyond the basic ASCII set. This means it will lower case characters from Greek, Cyrillic, etc., not just the English alphabet.

## See Also
For more, check out:
- The Go `strings` package documentation: https://pkg.go.dev/strings
- The Unicode Standard: https://www.unicode.org/standard/standard.html
- Go by Example: Strings - https://gobyexample.com/strings
