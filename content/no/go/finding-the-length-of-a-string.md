---
title:                "Å finne lengden av en streng"
html_title:           "Go: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

### Hva & Hvorfor?

Å finne lengden på en streng er en vanlig operasjon i programmering. Dette betyr ganske enkelt å bestemme hvor mange tegn en gitt tekststreng inneholder. Dette er nyttig for å manipulere og behandle data på riktig måte.

### Hvordan:

Go gir oss en innebygd funksjon for å finne lengden på en streng. Se eksempelet nedenfor for å se hvordan det gjøres:

```Go
package main

import "fmt"

func main() {
	tekst := "Dette er en tekststreng"
	fmt.Println(len(tekst)) // Output: 24
}
```

Her bruker vi ```len``` funksjonen og gir den vår tekststreng som parameter, og den vil returnere antall tegn i strengen. 

### Deep Dive:
Å finne lengden på en streng kan virke som en enkel operasjon, men det er viktig å forstå at i programmering er hver handling og operasjon nøye utformet og har en historisk sammenheng. Alternativer til å bruke ```len``` funksjonen inkluderer å bruke en løkke og telle antall tegn, eller bruke en innebygd funksjon i et annet språk som Python sin ```len()``` funksjon.

I Go er lengden på en streng lagret som et heltall, og den bruker en effektiv algoritme for å beregne lengden. Dette betyr at operasjonen er rask og pålitelig, selv for lange strenger.

### Se også:
- [Go offisiell dokumentasjon for strenger](https://golang.org/pkg/strings/)
- [Wikipedia - String length](https://en.wikipedia.org/wiki/String_length)