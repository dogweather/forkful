---
title:                "Gör en sträng versal"
html_title:           "Go: Gör en sträng versal"
simple_title:         "Gör en sträng versal"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att förstora en sträng i programmering innebär att omvandla alla bokstäver i en textsträng till stora bokstäver. Det är användbart för att standardisera datainslag, så att olikheter i textinmatning inte stör dataanalys eller -bearbetning.

## Hur man gör:

Med Go språket kan du använda `strings.ToUpper` funktionen som tar in en sträng och returnerar en ny sträng med alla bokstäver omvandlade till stora bokstäver. Här är ett kort exempel:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "hej, världen!"
	capText := strings.ToUpper(text)
	fmt.Println(capText)
}
```
När du kör detta, skulle utmatningen se ut så här:

```
HEJ, VÄRLDEN!
```

## Djupdykning

Möjligheten att förstora strängar går tillbaka till de tidiga dagarna av datorer, när olika system hanterade text på olika sätt. Standardisering, som kapitalisering, var ett sätt att säkerställa att data var kompatibla över olika system.

Ett alternativ till `strings.ToUpper` i Go är att manuellt iterera över varje bokstav i strängen och använda `unicode.ToUpper` på den. Men i de flesta fall är `strings.ToUpper` den bästa och mest effektiva lösningen.

När det gäller implementeringsdetaljer, använder `strings.ToUpper` intern bokstavsrepresentation (i form av "runes" för att hantera potentiella multi-byte karakterer), vilket är viktigt för att korrekt hantera icke-ASCII bokstäver.

## Se också

För mer information om strängar i Go, se officiella dokumentationen här: https://golang.org/pkg/strings/

Du kan också läsa mer om Rune typen och unicode hantering i Go här: https://blog.golang.org/strings