---
title:                "Interpolering av en streng"
html_title:           "Go: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

Hva og Hvorfor?

Interpolering av en streng i programmering betyr å sette inn variabelverdier i en tekststreng. Programmerere gjør dette for å gjøre teksten dynamisk og tilpasse den til ulike situasjoner.

Hvordan:

```Go
navn := "Marie"
alder := 25
fmt.Printf("Hei, mitt navn er %s og jeg er %d år gammel.", navn, alder)
```

Output:
Hei, mitt navn er Marie og jeg er 25 år gammel.

Dykk dypere:

Interpolering av strenger har eksistert i programmering siden tidlig på 1960-tallet og ble introdusert som en måte å erstatte variabler i en tekststreng. I Go språket er den vanligste måten å interpolere en streng å bruke “fmt.Sprintf” funksjonen. Alternativt kan man også bruke “Printf” funksjonen eller “strings.Replace” funksjonen. Når det kommer til implementering, vil disse funksjonene konvertere variabelverdiene til tekststrenger og sette dem inn i en ny streng før den blir returnert.

Se også:

1. Offisiell Go dokumentasjon: https://golang.org/pkg/fmt/
2. Go By Example: https://gobyexample.com/string-formatting