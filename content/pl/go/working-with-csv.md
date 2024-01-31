---
title:                "Praca z plikami CSV"
date:                  2024-01-19
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
W Go praca z CSV to obsługa plików tekstowych z danymi podzielonymi przecinkami (Comma-Separated Values). Programiści wykorzystują je do wymiany danych, bo to prosty i uniwersalny format, łatwy do odczytu zarówno dla maszyn, jak i ludzi.

## Jak to zrobić:
```Go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
    "strings"
)

func main() {
    // Zapis do CSV
    str := strings.NewReader("imię,wiek\nAlicja,25\nBob,30")
    r := csv.NewReader(str)
    records, _ := r.ReadAll()

    file, _ := os.Create("przykład.csv")
    w := csv.NewWriter(file)
    _ = w.WriteAll(records)
    w.Flush()

    // Odczyt z CSV
    f, _ := os.Open("przykład.csv")
    defer f.Close()

    reader := csv.NewReader(f)
    readRecords, _ := reader.ReadAll()

    for _, record := range readRecords {
        fmt.Println(record)
    }
}
```
Wyjście:
```
[imię wiek]
[Alicja 25]
[Bob 30]
```

## Deep Dive:
CSV to format znany od lat 70., gdy dyski były drogie i ludzie potrzebowali oszczędnych formatów. Alternatywy to JSON, XML, ale CSV jest szybszy w obsłudze i łatwiejszy do pisania parserów. Implementacja w Go jest prosta: używamy pakietu `encoding/csv` i standardowej obsługi plików.

## Zobacz także:
- Dokumentacja Go dla pakietu `encoding/csv`: https://pkg.go.dev/encoding/csv
- Szczegółowe instrukcje odczytu/zapisu do plików w Go: https://gobyexample.com/reading-files i https://gobyexample.com/writing-files
- Porównanie formatów danych (CSV, JSON, XML): https://www.datacamp.com/community/tutorials/data-formats-csv-json-xml
