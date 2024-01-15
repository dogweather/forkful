---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Go: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
Målet med denne artikkelen er å vise deg hvordan du kan slette tegn som matcher et spesifikt mønster. Dette kan være nyttig hvis du for eksempel ønsker å fjerne tegn som ikke er tillatt i en tekst eller ønsker å fjerne uønskede tegn fra en streng.

## Slik gjør du det
Det første du må gjøre er å importere "regexp" pakken til Go-programmet ditt ved å inkludere følgende linje på toppen av koden din:

```Go
import (
  "fmt"
  "regexp"
)
```

Deretter kan du bruke funksjonen "ReplaceAllString" fra pakken for å slette tegn som matcher et mønster. Her er et eksempel på hvordan du kan bruke det:

```Go
pattern := "[^a-zA-Z0-9\\s]" //Dette vil matche alle tegn som ikke er bokstaver, tall eller mellomrom
str := "Hei, jeg har en aprilie! #" //Opprinnelig tekst med uønskede tegn
repl := "" //Erstatt med ingenting - altså sletter tegnene
result := regexp.ReplaceAllString(str, pattern) //returnerer den nye teksten uten de uønskede tegnene
fmt.Println(result) //Output: "Hei jeg har en aprilie"
```

Som du kan se fra eksemplet vil alle tegn som matcher mønsteret bli slettet fra teksten. Du kan også bruke dette til å erstatte tegn med noe annet hvis du ønsker det.

## Dykk dypere
La oss se nærmere på hvordan funksjonen "ReplaceAllString" fungerer. Den tar inn tre parametere: en tekststreng, et mønster og en erstatningstreng. Den leter så gjennom tekststrengen og finner alle forekomster av mønsteret. Hvis et tegn matcher mønsteret blir det erstattet med erstatningstrengen. Hvis erstatningstrengen er tom blir tegnet slettet.

Det finnes også andre nyttige funksjoner i "regexp" pakken for å håndtere og manipulere tekst. Utforsk mer og se hva du kan finne ut!

## Se også
- [Golang "regexp" pakken](https://golang.org/pkg/regexp/)
- [Dokumentasjon for "ReplaceAllString" funksjonen](https://golang.org/pkg/regexp/#Regexp.ReplaceAllString)
- [Eksempel på hvordan bruke "regexp" pakken i praksis](https://golang.org/pkg/regexp/#example_ReplaceAllString)