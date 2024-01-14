---
title:                "Go: Kapitalisere en streng"
simple_title:         "Kapitalisere en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor Engasjere Seg i Store Bokstaver i en Streng?

Noen ganger, spesielt når vi jobber med tekstbehandling, vil vi kanskje ha en streng med tekst som består av små bokstaver, men vi ønsker å endre den til store bokstaver. Dette kan være av estetiske grunner, eller kanskje fordi vi skal vise noe som et skilt eller et overskrift. I slike tilfeller er det nyttig å kunne konvertere en streng til store bokstaver, og i denne bloggposten vil vi gå gjennom en enkel måte å gjøre det på ved hjelp av Go-programmeringsspråket.

## Slik Gjør Du Det:

For å kapitalisere en streng i Go, bruker vi funksjonen `strings.ToUpper()`. La oss se på et eksempel:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	litenStreng := "dette er en streng med små bokstaver"
	storStreng := strings.ToUpper(litenStreng)
	fmt.Println(storStreng) // utskrift: DETTE ER EN STRENG MED SMÅ BOKSTAVER
}
```

Som du ser, bruker vi først `import`-koden for å få `strings`-pakken til å jobbe med strenger, og deretter deklarerer vi en variabel `litenStreng` som inneholder en streng med små bokstaver. Deretter bruker vi `strings.ToUpper()`-funksjonen for å endre den til store bokstaver og lagrer det i en ny variabel `storStreng`. Til slutt skriver vi ut den nye strengen ved hjelp av `fmt.Println()`-funksjonen.

## Dykk dypere:

Selv om konvertering av en streng til store bokstaver kan virke som en enkel oppgave, er det faktisk mer å lære om hvordan Go håndterer strenger. For eksempel, hvis du trenger å håndtere ikke-vestlige tegn og symboler, må du bruke`unicode`-pakken for å sikre at konverteringen til store bokstaver gjøres på en korrekt måte.

## Se Også:

- `strings`-pakken i Go dokumentasjonen: https://golang.org/pkg/strings/
- En detaljert artikkel om strenger i Go: https://www.calhoun.io/working-with-strings-in-go/
- Om Unicode og strenger i Go: https://blog.golang.org/strings