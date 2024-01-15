---
title:                "Å bruke regulære uttrykk"
html_title:           "Go: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor
Om du ønsker å finne, filtrere eller manipulere tekst på en enkel og effektiv måte, er regular expressions (regex) svaret. Ved å bruke regex, kan du søke etter bestemte mønstre i en tekststreng og deretter utføre spesifikke handlinger basert på disse mønstrene.

## Slik gjør du det
For å bruke regular expressions i Go, må du først importere "regexp" pakken. Deretter kan du bruke funksjonen "MatchString" for å søke etter et gitt mønster i en streng. For eksempel:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// Definerer en tekststreng
	text := "Dette er en teststreng for å vise bruk av regex i Go"

	// Søker etter mønsteret "regex" i strengen
	match, _ := regexp.MatchString("regex", text)

	// Printer ut resultatet
	fmt.Println(match) // Output: false
}
```

Som du kan se, returnerer "MatchString" en boolsk verdi avhengig av om mønsteret eksisterer i strengen eller ikke. Du kan også bruke "FindAllString" for å finne alle forekomster av et mønster i en streng og "ReplaceAllString" for å erstatte forekomster av et mønster med en annen streng.

## Dypdykk
Regex uttrykk kan bli ganske komplekse, og det er viktig å forstå de forskjellige elementene som utgjør dem. For eksempel betyr "regex" i vårt første eksempel bare at det skal matches på den eksakte strengen "regex". Men du kan også bruke spesielle symboler og uttrykk for å matche et bredere spekter av tekst. Her er noen vanlige symboler:

- "." - Matcher ethvert tegn
- "^" - Matcher starten av en streng
- "$" - Matcher slutten av en streng
- "*" - Matcher null eller flere forekomster av det foregående elementet
- "+" - Matcher en eller flere forekomster av det foregående elementet
- "?" - Matcher null eller en forekomst av det foregående elementet
- "[]" - Matcher ethvert tegn som er inkludert i parentesene
- "[^ ]" - Matcher ethvert tegn som ikke er inkludert i parentesene
- "|" - Matcher enten uttrykket til venstre eller til høyre

For en mer omfattende og detaljert forklaring av regular expressions, anbefaler vi å ta en titt på følgende ressurser:

- [Go Documentation: regexp Package](https://golang.org/pkg/regexp/)
- [Regular Expressions 101](https://regex101.com/)
- [Regular Expressions Tutorial på W3Schools](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)

## Se også
- [Go Dokumentasjon: regexp Pakke](https://golang.org/pkg/regexp/)
- [Regular Expressions 101](https://regex101.com/)
- [Regular Expressions Tutorial på W3Schools](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)