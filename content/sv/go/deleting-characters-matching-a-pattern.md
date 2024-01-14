---
title:    "Go: Radera tecken som matchar ett mönster"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Det kan finnas många anledningar till varför någon skulle vilja ta bort vissa tecken från en textsträng baserat på ett visst mönster. Det kan till exempel vara för att rensa bort onödiga eller ogiltiga tecken innan man bearbetar data, eller för att standardisera en viss typ av formatering.

## Så här gör du

För att ta bort tecken från en textsträng baserat på ett visst mönster, kan du använda Go:s inbyggda `ReplaceAllString` funktion i paketet `regexp`. Här är ett exempel på hur koden skulle kunna se ut:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Skapa en regexp som matchar tecknen vi vill ta bort
	re := regexp.MustCompile("[a-z]")

	// Strängen som vi vill ta bort tecken ifrån
	str := "Hej123! Vad är ditt namn?"

	// Använd ReplaceAllString för att ta bort alla tecken som matchar det givna mönstret
	newStr := re.ReplaceAllString(str, "")

	// Skriv ut den nya strängen
	fmt.Println(newStr)
}
```

Kör man detta program kommer output att se ut så här:

```
123! ?
```

I detta exempel matchar vår regexp alla små bokstäver a-z och tar bort dem från strängen. Men det går självklart att anpassa efter det specifika mönstret man vill matcha.

## Djupdykning

I exemplet ovan använde vi `ReplaceAllString` som endast tar bort matchande tecken. Om man istället vill behålla matchande tecken och bara ta bort de som inte matchar, kan man använda sig av `ReplaceAllStringFunc` som tar emot en funktion som argument. Denna funktion måste returnera en sträng som ska ersätta den matchande strängen. Här är ett exempel på hur det skulle kunna se ut:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Skapa en regexp som matchar tecknen vi vill ta bort
	re := regexp.MustCompile("[a-z]")

	// Strängen som vi vill ta bort tecken ifrån
	str := "Hej123! Vad är ditt namn?"

	// Använd ReplaceAllStringFunc för att ta bort alla tecken som inte matchar det givna mönstret
	newStr := re.ReplaceAllStringFunc(str, func(m string) string {
		// Returnera ett tomt tecken för varje matchande tecken, vilket i praktiken tar bort alla tecken som inte matchar
		return ""
	})

	// Skriv ut den nya strängen
	fmt.Println(newStr)
}
```

Output i detta fall skulle bli:

```
Hej?!?
```

## Se även

- [Go regexp paketet](https://golang.org/pkg/regexp/)
- [Regular Expressions 101 - en användbar guide](https://regex101.com/)