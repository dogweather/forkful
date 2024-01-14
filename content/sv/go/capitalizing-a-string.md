---
title:                "Go: Huvudbokstavsändring av en sträng"
simple_title:         "Huvudbokstavsändring av en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Har du någonsin undrat varför det är viktigt att kunna göra en sträng med stora bokstäver i Go-programmering? I denna bloggpost kommer vi att utforska varför det kan vara användbart och hur du enkelt kan göra det.

## Så här gör du

För att göra en sträng med stora bokstäver i Go, kan du använda funktionen `strings.ToUpper()` och ange den sträng som du vill göra stora bokstäver som en parameter. Se exempel nedan:

```Go
package main

// Importera "strings" biblioteket för att få tillgång till ToUpper() funktionen
import (
	"fmt"
	"strings"
)

func main() {
	// Skapa en variabel med en sträng
	str := "hej världen"

	// Använd funktionen ToUpper() för att göra strängen till stora bokstäver
	upStr := strings.ToUpper(str)

	// Skriv ut den nya strängen
	fmt.Println(upStr)
}

```

Output:

```bash
HEJ VÄRLDEN
```

## Djupdykning

Det finns flera anledningar till varför det kan vara användbart att kunna göra en sträng med stora bokstäver i Go. En av de vanligaste anledningarna är när man behöver jämföra olika strängar utan att vara orolig för storleksbokstäver. Genom att göra alla strängar till stora bokstäver blir det lättare att jämföra dem eftersom skillnader i små eller stora bokstäver inte kommer att påverka resultatet.

Funktionen `strings.ToUpper()` är också användbar när man behöver konvertera input från användare, som kan skriva i olika storleksbokstäver, till en enhetlig form för bearbetning och lagring.

## Se även

- Läs mer om strings paketet i Go: https://golang.org/pkg/strings/
- Utforska fler funktioner för hantering av strängar i Go: https://gobyexample.com/string-functions
- Se en live kodexempel på Playground: https://play.golang.org/p/ixBC8jMb44I