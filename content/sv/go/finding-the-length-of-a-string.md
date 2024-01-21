---
title:                "Hitta längden på en sträng"
date:                  2024-01-20T17:47:28.641300-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng betyder att räkna antalet karaktärer i den. Programmerare gör det för att validera indata, loopa genom innehåll eller hämta specifika delar av texten.

## Så här gör man:
Här är en kodsnutt för att hitta längden på en sträng i Go:

```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "Hejsan!"
	fmt.Println("Antal bytes:", len(str)) // Bytes, inte alltid lika med antal karaktärer
	fmt.Println("Antal runor:", utf8.RuneCountInString(str)) // Korrekt antal karaktärer
}
```
Exempel utdata:
```
Antal bytes: 7
Antal runor: 6
```

Observera att `len()` ger antalet bytes, vilket kan skilja sig från antalet faktiska karaktärer om strängen innehåller multibyte-tecken, till exempel emoji eller annan Unicode-text.

## Fördjupning
I tidiga datasystem räknade man oftast karaktärer som lika med bytes. Men i moderna språk som Go, med Unicode-stöd, är inte längre en karaktär nödvändigtvis samma som en byte. UTF-8, som Go använder för strängar, är en variabel bredd encoding, och det betyder att olika karaktärer kan ta upp ett olika antal bytes.

Ett alternativ till `utf8.RuneCountInString()` är att iterera över strängen med en range-loop, som hanterar runor snarare än bytes. Här är ett exempel på det:

```Go
str := "Hejsan!"
count := 0
for range str {
	count++
}
fmt.Println("Antal runor med range-loop:", count)
```

Detta kommer också ge det korrekta antalet karaktärer. Valet av metod kan bero på specifika prestandakrav eller stilpreferenser.

## Se även
- Go dokumentation om strängar och runor: https://golang.org/pkg/strings/
- UTF-8 och Go: https://blog.golang.org/strings
- The Go Programming Language Specification (speciellt avsnitt om "String literals"): https://golang.org/ref/spec#String_literals