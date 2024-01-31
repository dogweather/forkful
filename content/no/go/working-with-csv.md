---
title:                "Arbeid med CSV"
date:                  2024-01-19
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Arbeid med CSV (Comma-Separated Values) innebærer å lese og skrive data i tekstfilformat som deler data med komma. Programmerere bruker CSV for enkel utveksling av data mellom forskjellige systemer og programmer.

## How to:
### Lese en CSV-fil:
```Go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    // Åpne CSV-filen
    fil, err := os.Open("data.csv")
    if err != nil {
        panic(err)
    }
    defer fil.Close()

    // Les med csv.NewReader
    reader := csv.NewReader(fil)
    data, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    // Skriv ut CSV-data
    for _, linje := range data {
        fmt.Println(linje)
    }
}
```
### Skrive til en CSV-fil:
```Go
package main

import (
    "encoding/csv"
    "os"
)

func main() {
    // Lag en ny CSV-fil
    fil, err := os.Create("ny_data.csv")
    if err != nil {
        panic(err)
    }
    defer fil.Close()

    // Skriv til fil med csv.NewWriter
    writer := csv.NewWriter(fil)
    defer writer.Flush()

    data := [][]string{
        {"Navn", "Alder", "By"},
        {"Ola", "23", "Oslo"},
        {"Kari", "30", "Bergen"},
    }

    for _, linje := range data {
        err := writer.Write(linje)
        if err != nil {
            panic(err)
        }
    }
}
```

## Deep Dive
CSV-formatet har vært en gjenganger siden 1970-tallet, brukt for sin enkelhet i å lagre tabellære data. Alternativer inkluderer JSON og XML, men CSV er fortsatt foretrukket for sin enkelhet og gode støtte i regnearkprogrammer. I Go, pakken `encoding/csv` er anvendt for både lesing og skriving, som håndterer vanlige utfordringer som newlines i felt eller kommane som data.

## See Also
- Go dokumentasjon for `encoding/csv`: https://pkg.go.dev/encoding/csv
- Go by Example - CSV-formatet: https://gobyexample.com/reading-files
- Liste over CSV-verktøy: https://github.com/csvkit/csvkit
