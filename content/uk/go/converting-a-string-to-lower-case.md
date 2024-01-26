---
title:                "Перетворення рядка у нижній регістр"
date:                  2024-01-20T17:38:19.883453-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
Converting a string to lower case means changing all uppercase letters within the string to their lower case counterparts. Developers use this for consistency in data processing, easier comparisons, and search optimization.

## How to: (Як це зробити:)
```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalString := "Привіт, Як справи?"
	lowerCaseString := strings.ToLower(originalString)
	fmt.Println(lowerCaseString) // "привіт, як справи?"
}
```

## Deep Dive (Поглиблений Аналіз)
Converting to lower case has always been a staple in text processing. Historically, it facilitated database searches and user input normalization. As for alternatives, you could manually map each upper case character to its lower case equivalent - tedious and not recommended.

The `strings.ToLower` function in Go under the hood uses Unicode's case mapping to properly handle different languages, ensuring your Ukrainian characters are correctly transformed.

## See Also (Дивіться також)
- Go documentation on `strings` package: https://pkg.go.dev/strings
- Unicode standard for case mapping: https://www.unicode.org/reports/tr21/tr21-5.html
- More text operations in Go: https://golang.org/doc/articles/strings
