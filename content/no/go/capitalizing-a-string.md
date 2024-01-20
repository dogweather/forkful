---
title:                "Stor bokstav i en streng"
html_title:           "Go: Stor bokstav i en streng"
simple_title:         "Stor bokstav i en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kapitalisering av en streng innebærer å endre første bokstav i en streng til en stor bokstav. Programmerere gjør dette for å forbedre lesbarheten og for å skille mellom forskjellige ord som funksjonsnavn og variabler.

## Hvordan gjøre det:
Her er en rask skisse over hvordan du kapitaliserer en streng i Go:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "hei, verden!"
	capStr := strings.Title(str)
	fmt.Println(capStr)
}
```
Når du kjører koden ovenfor, vil output være:

```Go
"Hei, Verden!"
```

## Et dypdykk 
#### Historisk kontekst:
Konseptet med strengkapitalisering har faktisk en ganske lang historie. Det stammer fra dager da datamaskiner var begrenset til tekstbaserte grensesnitt.

#### Alternativer:
Go's innebygde `strings` pakke tilbyr flere metoder for å manipulere strenger, ikke bare `Title`. Du kan for eksempel bruke `ToUpper`, som vil gjøre alle bokstavene i strengen store, ikke bare den første.

```Go
str := "hei, verden!"
upperStr := strings.ToUpper(str)
fmt.Println(upperStr)
```
Output vil være:

```Go
"HEI, VERDEN!"
```

#### Implementasjonsdetaljer:
`strings.Title` i Go bruker `unicode.ToTitle` funksjonen for å konvertere den første bokstaven i hvert ord til en tittelbokstav (som er identisk med en stor bokstav for de fleste språk)

## Se også:
For ytterligere lesing, ta en titt på følgende kilder:

1. Go's offisielle dokumentasjon om strenger: https://golang.org/pkg/strings/
2. Go's innebygde funksjon `ToUpper`: https://golang.org/pkg/strings/#ToUpper
3. Go's innebygde funksjon `Title`: https://golang.org/pkg/strings/#Title
4. Utforsk utfyllende innhold rundt strengmanipulasjon i Go på nettstedet GoByExample: https://gobyexample.com/string-functions.