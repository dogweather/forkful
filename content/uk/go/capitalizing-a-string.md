---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
simple_title:         "Перетворення рядка на великі літери"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## Що і чому?

Capitalizing a string means making the first character in each word uppercase. Programmers often use this to format text for a human-friendly display - like turning "john doe" into "John Doe."

## How to:
## Як це зробити:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "це приклад рядка"
	capitalized := strings.Title(strings.ToLower(text))
	fmt.Println(capitalized)
}
```

Sample output:
```
Це Приклад Рядка
```

## Deep Dive
## Поглиблений огляд

In Go, strings are immutable sequences of bytes. Capitalizing strings has been around since people started computerizing records - helping sort and present data neatly. The `strings` package in Go provides the `Title` function, which capitalizes the first letter of each word. But beware, `Title` is quite simple minded - it doesn't properly handle cases like "McDonald" or "O'Neil" or some non-English letter capitalization rules. For these cases, custom logic or libraries like `golang.org/x/text` are used.

## See Also
## Дивись також

- Go strings package: https://pkg.go.dev/strings
- Go `golang.org/x/text` package: https://pkg.go.dev/golang.org/x/text
- Unicode Standard Annex #29 on text boundaries (for complex capitalization logic): https://unicode.org/reports/tr29/
