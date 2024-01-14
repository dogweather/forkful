---
title:    "Go: Omvandla en sträng till små bokstäver"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener kan vara användbart när man behöver söka och jämföra text på ett icke-skillnadskänsligt sätt.

## Hur man gör

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hej på dig!"
	fmt.Println(strings.ToLower(str))
}

```

Output:
```shell
hej på dig!
```

## Djupdykning

Konvertering av strängar till gemener är en vanlig uppgift inom programmering. För att uppnå detta kan man använda funktionen `ToLower` från paketet `strings`. Denna funktion omvandlar alla bokstäver i en sträng till gemener och returnerar sedan en ny sträng.

Det finns dock några viktiga saker att tänka på vid denna konvertering. Beroende på språk och skrivstil kan resultaten variera. Det är därför viktigt att förstå vilka tecken som kan påverkas av konverteringen och att hantera dessa korrekt.

Ett annat vanligt problem är skillnaden mellan Unicode och ASCII-tecken. Om din sträng innehåller Unicode-tecken är det viktigt att använda rätt funktion för att konvertera till gemener, till exempel `ToLowerSpecial` som hanterar alla Unicode-tecken korrekt.

Med förståelse för dessa saker kan du enkelt och korrekt konvertera strängar till gemener i ditt Go-program.

## Se även

- [Go Dokumentation om string package](https://golang.org/pkg/strings/)
- [Go Dokumentation om Unicode](https://blog.golang.org/strings)
- [Go Dokumentation om Rune](https://golang.org/pkg/unicode/utf8/)