---
title:                "Å jobbe med csv"
html_title:           "Go: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
CSV står for Comma Separated Values og er et vanlig filformat som brukes til å lagre tabellignende data. Programmere bruker CSV for å enkelt lagre og håndtere store datamengder uten behov for en database.

## Hvordan:
Under er et eksempel på hvordan du kan lese og behandle en CSV-fil i Go:

```Go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    // Åpne CSV-fil
    f, err := os.Open("data.csv")
    if err != nil {
        fmt.Println("Kunne ikke åpne fil:", err)
        return
    }
    // Les inn CSV-data
    reader := csv.NewReader(f)
    records, err := reader.ReadAll()
    if err != nil {
        fmt.Println("Kunne ikke lese CSV-data:", err)
        return
    }
    // Skriv ut hver rad i CSV-filen
    for _, row := range records {
        for _, col := range row {
            fmt.Printf("%s\t", col)
        }
        fmt.Println()
    }
}
```
Denne koden vil lese en CSV-fil som heter "data.csv" og skrive ut hver rad som en tabell med forskjellige kolonner.

## Dypdykk:
CSV ble utviklet på 1970-tallet som et enkelt filformat for å importere og eksportere databasedata. Alternativene til CSV inkluderer formater som Excel og JSON. Go har standardbiblioteker for å håndtere CSV-filer, men det finnes også tredjepartsbiblioteker som gir mer funksjonalitet og fleksibilitet.

## Se også:
- [Go's dokumentasjon om CSV-pakken](https://golang.org/pkg/encoding/csv/)
- [En guide til å arbeide med CSV-filer i Go](https://blog.gopheracademy.com/advent-2014/parsing-xml-and-json-with-go/)
- [Et populært tredjepartsbibliotek for CSV-håndtering i Go](https://github.com/gocarina/gocsv)