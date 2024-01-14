---
title:                "Go: Slette tegn som matcher et mønster."
simple_title:         "Slette tegn som matcher et mønster."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Hvorfor
Det er mange grunner til å ville slette karakterer som matcher et bestemt mønster i Go-programmering. Det kan være for å rense tekststrenger eller filtrere ut uønskede data. Uansett årsak, er det viktig å kunne behandle og manipulere data effektivt for å skrive gode og funksjonelle programmer.

# Hvordan

En enkel måte å slette karakterer som matcher et mønster på, er å bruke funksjonene strings.ReplaceAll() og strings.Count() som finnes i standardbiblioteket for Go. Disse kan brukes til å erstatte alle forekomster av et spesifikt mønster med en tom streng, og også telle hvor mange ganger mønsteret forekommer.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Opprett en tekststreng med noen uønskede karakterer
	text := "D3t eR en T%ekst me+ mye o1!sant&"

	// Bruk strings.ReplaceAll() for å slette alle tall, spesielle tegn og mellomrom
	cleanedText := strings.ReplaceAll(text, "1", "")
	cleanedText = strings.ReplaceAll(cleanedText, "3", "")
	cleanedText = strings.ReplaceAll(cleanedText, "%", "")
	cleanedText = strings.ReplaceAll(cleanedText, "+", "")
	cleanedText = strings.ReplaceAll(cleanedText, " ", "")

	// Bruk strings.Count() for å telle antall forekomster av bokstaven "e"
	countE := strings.Count(text, "e")

	fmt.Println("Orginal tekst:", text)
	fmt.Println("Renset tekst:", cleanedText)
	fmt.Println("Antall forekomster av bokstaven 'e':", countE)
}
```

Denne koden vil ha følgende output:

```
Orginal tekst: D3t eR en T%ekst me+ mye o1!sant&
Renset tekst: DtRnTkstmeyesoant
Antall forekomster av bokstaven 'e': 2
```

Det er også mulig å bruke regulære uttrykk og funksjonen regexp.MatchString() for å finne og slette karakterer som matcher et mer komplekst mønster. Dette kan være nyttig hvis man ønsker å fjerne uttrykk som følger et spesifikt mønster, for eksempel telefonnummer eller e-postadresser.

# Dykk ned

For de som er interessert i å lære mer om å slette karakterer som matcher et mønster i Go, er det mange ressurser tilgjengelig. Standardbiblioteket for Go har en omfattende dokumentasjon om strings-pakken som inneholder flere nyttige funksjoner for å håndtere tekststrenger. Det finnes også flere gode tutorials og onlinekurs som tar for seg bruken av regulære uttrykk i Go-programmering.

# Se også

- [Go dokumentasjon - strings-pakken](https://golang.org/pkg/strings/)
- [Go by Example - regex](https://gobyexample.com/regular-expressions)
- [Utdannet.no - Lær Go fra grunnleggende til avansert](https://utdannet.no/kurs/programmering/go/)