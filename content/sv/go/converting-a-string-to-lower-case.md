---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Artikeln: Konvertera en sträng till gemener i Go

## Vad och Varför?
Att konvertera en sträng till gemener innebär att förändra alla versaler till gemener. Programmerare gör detta för att standardisera ingångsdata, vilket möjliggör konsekvent och effektiv databearbetning.

## Hur gör man?
Programmet nedan demonstrerar hur du konverterar en sträng till gemener i Go.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "HEJ VÄRLD!"
	fmt.Println(strings.ToLower(s))  // Prints "hej värld!"
}
```

När du kör detta program printed den omvandlade strängen "hej värld!" till konsolen.

## Djupdykning 
Funktionen `ToLower` introducerades först i ASCII, där idén var att förskjuta bokstavskoderna för versaler till deras motsvarande gemener. Go använder samma koncept, men fungerar med Unicode, vilket är en modernare och mer inkluderande standard för teckenkod.

Ett alternativ till `ToLower` är att iterera över varje tecken i strängen och konvertera versaler till gemener manuellt. Men detta är normalt sett mer komplicerat och ineffektivt jämfört med att använda `ToLower`.

Den inre implementationen av `ToLower` använder en tabell för att kartlägga varje tecken till dess motsvarande gemener. Detta innebär att funktionen är väldigt snabb, eftersom den bara behöver se upp ett resultat i tabellen för varje tecken i strängen.

## Se även
Fler resurser relaterat till strängomvandling i Go kan hittas på:
1. [GoDoc för paketet "strings"](https://pkg.go.dev/strings) - Detaljerade dokumentationer för Go's strängfunktionalitet.  
2. [Go Blog - Strings in Go](https://blog.golang.org/strings) - En djupdykning into strängar i Go, inklusive deras interna representation och funktioner.