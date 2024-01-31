---
title:                "Att göra en sträng versal"
date:                  2024-01-19
simple_title:         "Att göra en sträng versal"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att göra en sträng versal innebär att omvandla bokstäverna i en textsträng till stora bokstäver. Programmerare gör detta för läsbarhet, för att framhäva element, eller följa datanormer.

## How to:
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "hallå, världen!"
	capitalizedText := strings.ToUpper(text)
	fmt.Println(capitalizedText)
}
```
Sample output:
```
HALLÅ, VÄRLDEN!
```

## Deep Dive
Att göra en sträng versal är standard i många programmeringsspråk. I Go använder vi `strings.ToUpper()` för detta. Historiskt sett handlar det om enkelhet; vi gör ofta om text till versaler för titlar, namn eller när vi definierar konstanter. Alternativ till `ToUpper()` inkluderar iteration över strängen och konvertering av varje karaktär, men prestandamässigt vinner standardbiblioteksfunktionen. Implementeringsmässigt hanterar `ToUpper()` Unicode och är därmed kulturellt omsorgsfullt, vilket är viktigt i ett globaliserat samhälle.

## See Also
- Go standard library documentation for strings package: [https://pkg.go.dev/strings](https://pkg.go.dev/strings)
- Unicode standard for case mapping: [https://www.unicode.org/reports/tr21/tr21-5.html](https://www.unicode.org/reports/tr21/tr21-5.html)
- Go blog about strings, bytes, runes and characters: [https://blog.golang.org/strings](https://blog.golang.org/strings)
