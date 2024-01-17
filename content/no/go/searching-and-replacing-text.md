---
title:                "Søking og utskifting av tekst"
html_title:           "Go: Søking og utskifting av tekst"
simple_title:         "Søking og utskifting av tekst"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søking og erstattning av tekst er en vanlig oppgave for programmerere. Det innebærer å finne et bestemt stykke tekst i en fil eller streng, og erstatte det med en annen tekst. Dette kan være nyttig når man ønsker å gjøre globale endringer eller rette feil i koden.

## Hvordan:
I Go-språket, kan du bruke funksjonen `strings.Replace()` for å søke og erstatte tekst. Her er et eksempel på hvordan du bruker denne funksjonen:

```
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Hei, verden!"
	newText := strings.Replace(text, "Hei", "Hallo", 1)
	fmt.Println(newText)
}
```

Dette vil gi følgende utskrift:
```
Hallo, verden!
```

## Dypdykk:
Søking og erstatting av tekst har blitt brukt helt siden programmeringens begynnelse. I dag er det mange forskjellige metoder og verktøy tilgjengelig for å utføre denne oppgaven. I Go-språket er `strings.Replace()`-funksjonen den mest effektive og enkleste måten å søke og erstatte tekst på. Du kan også bruke regulære uttrykk ved hjelp av `regexp`-pakken i Go.

## Se også:
- [Go documentation](https://golang.org/pkg/strings/#Replace)
- [Understanding Regular Expressions](https://www.regular-expressions.info/)