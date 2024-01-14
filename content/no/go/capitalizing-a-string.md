---
title:                "Go: Å kapitalisere en streng"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kapitalisere en streng (string) i et program kan være nyttig når man ønsker å formatere tekst på en bestemt måte, for eksempel når man skal skrive ut en tekst eller vise den på en skjerm.

## Hvordan

For å kapitalisere en streng i Go kan man bruke funksjonen `strings.ToUpper()` som tar inn en streng som argument og returnerer en ny versjon av strengen med kun store bokstaver. For eksempel:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	name := "lars"
	
	// Kapitaliserer "name" strengen og lagrer det i "capitalizedName"
	capitalizedName := strings.ToUpper(name)
	
	fmt.Println(capitalizedName) // Skriver ut "LARS"
}
```

## Dypdykk

I Go er strenger immutable, noe som betyr at de ikke kan endres direkte. Derfor vil ikke `strings.ToUpper()` endre den originale strengen, men heller returnere en ny versjon av den. Det finnes også andre funksjoner for å manipulere strenger i Go, som `strings.ToLower()` for å gjøre alle bokstaver små, og `strings.Title()` for å gjøre alle ord i en streng til stor forbokstav.

## Se Også

- [Go Dokumentasjon: strings](https://golang.org/pkg/strings/)
- [Go By Examples: Strings](https://gobyexample.com/strings)
- [Kapitalisere strenger i Go: Enkel løsning eller gode rutiner?](https://no.wikipedia.org/wiki/Wikipedia:Torget/Kapitalisere_strenger_i_Go:_Enkel_l%C3%B8sning_eller_gode_rutiner%3F)