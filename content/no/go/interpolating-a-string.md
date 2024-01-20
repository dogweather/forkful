---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Streng interpolering er en metode for å injisere verdiene fra variabler rett inn i en streng. Det hjelper programmerere med å generere mer dynamiske strenger, og unngå rotete + operator bruk for streng konkatenasjon.

## Hvordan gjør man det:

Her er en enkel kodebit i Go for å demonstrere streng interpolering.

```Go
package main

import (
	"fmt"
)

func main() {
	var name string = "Per"
	fmt.Printf("Hei, %s\n", name)  
}
```

Når du kjører denne koden, vil du få følgende utskrift:

```Go
Hei, Per
```

## Dypdykk

Strenginterpolering har lenge vært vanlig i mange programmeringsspråk. Perl, Ruby og nyligere JavaScript er eksempler på språk som støtter denne funksjonen. 

I Go, en alternativ måte å interpolere strenger er ved hjelp av funksjonen `Sprintf` fra fmt pakken:

```Go
text := fmt.Sprintf("Hei, %s", name)
fmt.Println(text)
```

Golang kjører strenginterpolering ved å erstatte plassholderne med verdien til variablene i minnet, noe som kan være kostbart i form av ytelse hvis det blir brukt uforsiktig.

## Se også:

For en mer detaljert gjennomgang av strenginterpolering, og hvordan den kan forbedre koden din, sjekk disse linkene:

- Go Dokumentasjon: [fmt](https://golang.org/pkg/fmt/)
- Go By Example: [String Formatting](https://gobyexample.com/string-formatting)