---
title:    "Go: Å bruke regulære uttrykk"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regulære uttrykk, eller regex, er et viktig verktøy for å håndtere tekstbaserte data i Go. Det tillater deg å finne og manipulere tekstbaserte mønstre, og gjør koding av komplekse søk og erstatninger enklere og mer effektivt.

## Hvordan

For å bruke regulære uttrykk i Go kan du bruke pakken "regexp". Først må du importere pakken ved å legge til ```"regexp"``` i importdeklarasjonen din. Deretter kan du lage et nytt regeltreffobjekt ved å bruke ```regexp.Compile()``` funksjonen. La oss si at vi ønsker å finne alle ord som begynner med bokstaven "a" i en streng:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	text := "apple and banana are my favorite fruits"
	r := regexp.MustCompile(`a\w+`)
	fmt.Println(r.FindAllString(text, -1)) // output: [apple and]
}
```
Her har vi brukt regexet ```a\w+```, som betyr en bokstav "a" etterfulgt av en eller flere bokstaver eller tall. Du kan også bruke spesialtegn som plassholder for å finne mønstre. For eksempel kan ```\d``` brukes for å finne tall og ```\s``` for å finne mellomrom.

## Dypdykk

Regex kan være veldig kraftig, men det kan også være forvirrende og komplekst for nybegynnere. Det er viktig å forstå grunnleggende syntax og hvordan man bruker spesialtegn og plassholdere for å finne det riktige mønsteret. Det kan også være nyttig å bruke online verktøy som regex101.com for å teste og validere regex før du implementerer det i koden din.

## Se også

- [Go regex pakke dokumentasjon](https://golang.org/pkg/regexp/)
- [Regulære uttrykk Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/go)
- [regex101.com](https://regex101.com/)