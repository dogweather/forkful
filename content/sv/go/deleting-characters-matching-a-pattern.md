---
title:                "Borttagning av tecken som matchar ett mönster"
html_title:           "Go: Borttagning av tecken som matchar ett mönster"
simple_title:         "Borttagning av tecken som matchar ett mönster"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort tecken som matchar ett mönster är en vanlig operation inom programmering. Genom att använda Go's inbyggda funktioner för stränghantering kan vi enkelt och effektivt rensa bort oönskade tecken i en sträng. Detta kan vara användbart när vi vill filtrera data eller konvertera en sträng till ett annat format.

## Hur gör man:
För att ta bort tecken som matchar ett specifikt mönster i en sträng, kan vi använda funktionen `Trim*` från paketet `strings`. Det finns olika Trim-funktioner som vi kan använda beroende på vilken typ av tecken vi vill ta bort. Här är en enkel kodsnutt som visar hur man tar bort alla Whitespace-tecken från en sträng:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := " Hej  världen! "
	trimmed := strings.TrimSpace(str)
	fmt.Println(trimmed)
}
```

Resultatet av koden ovan blir:

```
Hej världen!
```

## Djupdykning:
Att ta bort tecken som matchar ett mönster är en vanlig operation inom programmering, och att kunna hantera strängar effektivt är viktigt för många program. Go har inbyggda funktioner för stränghantering som gör det enkelt att ta bort oönskade tecken från en sträng. Innan Go 1.1 fanns inte Trim-funktionerna i standardbiblioteket, men de lades senare till för att göra stränghanteringen enklare för utvecklare.

Det finns också andra metoder för att ta bort tecken från en sträng, som användning av reguljära uttryck eller loopar. Men genom att använda Trim-funktionerna i Go, behöver vi inte implementera dessa komplexa metoder själva.

## Se även:
- [Go's officiella dokumentation om strängar](https://golang.org/pkg/strings/)
- [Reguljära uttryck i Go](https://gobyexample.com/regular-expressions)
- [En introduktion till strängar i Go](https://www.digitalocean.com/community/tutorials/an-introduction-to-working-with-strings-in-go-sv)