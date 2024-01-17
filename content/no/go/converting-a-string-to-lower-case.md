---
title:                "Konvertere en streng til små bokstaver."
html_title:           "Go: Konvertere en streng til små bokstaver."
simple_title:         "Konvertere en streng til små bokstaver."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en streng til små bokstaver er en vanlig operasjon i programmering som gjør at alle bokstavene i en tekst blir byttet ut med tilsvarende små bokstaver. Dette gjøres for å kunne sammenligne og behandle tekst på en mer enhetlig måte, uavhengig av hvilken form den opprinnelig ble skrevet i.

## Hvordan:
I Go kan du enkelt konvertere en streng til små bokstaver ved å bruke funksjonen strings.ToLower(). Se eksemplet under for å se hvordan det kan gjøres:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    tekst := "Dette er EN tekst Som kan VÆRE SkRevET I Ulike FoRMER."
    konvertertTekst := strings.ToLower(tekst)
    fmt.Println(konvertertTekst)
}

// Output: dette er en tekst som kan være skrevet i ulike former.
```

## Dykk dypere:
Å konvertere en streng til små bokstaver er en vanlig operasjon som går langt tilbake i dataprogrammeringens historie. Første gang dette ble implementert var i programmeringsspråket COBOL på 1950-tallet. I tillegg til å bruke funksjonen strings.ToLower(), kan man også benytte seg av Unicode- og RegEx-biblioteker for å oppnå lignende resultater.

## Se også:
- [Go strings.ToLower() dokumentasjon](https://golang.org/pkg/strings/#ToLower)
- [COBOL historie og utvikling](https://www.cs.umd.edu/projects/PL/cobol/)
- [Unicode og tekstbehandling i programmering](https://www.unicode.org/standard/principles.html)