---
title:                "Werken met CSV"
date:                  2024-01-28T22:10:03.766315-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met CSV"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met CSV in programmering betekent het lezen van en schrijven naar bestanden met door komma's gescheiden waarden - een eenvoudig, platte-tekst opslagformaat voor tabelgegevens. Programmeurs gebruiken het omdat het breed ondersteund wordt, eenvoudig te creëren en te interpreteren is, en simpel te importeren is in databases en spreadsheetprogramma's.

## Hoe:
### Een CSV-bestand lezen:
```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("data.csv")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	reader := csv.NewReader(file)
	records, err := reader.ReadAll()
	if err != nil {
		panic(err)
	}

	for _, record := range records {
		fmt.Println(record)
	}
}
```
### Schrijven naar een CSV-bestand:
```Go
package main

import (
	"encoding/csv"
	"os"
)

func main() {
	records := [][]string{
		{"Naam", "Leeftijd", "Stad"},
		{"Alice", "25", "New York"},
		{"Bob", "30", "San Francisco"},
	}

	file, err := os.Create("output.csv")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	defer writer.Flush()

	for _, record := range records {
		if err := writer.Write(record); err != nil {
			panic(err)
		}
	}
}
```

## Diepgaande verkenning
Het CSV-formaat bestaat al sinds de vroege jaren 70, afkomstig van de IBM Fortran (level G) compiler. Hoewel JSON of XML-formaten meer functies en complexiteit kunnen bieden, houdt CSV stand vanwege de pure eenvoud. In Go, behandelt het `encoding/csv` pakket de CSV-parsing en -serialisatie. Dit pakket ondersteunt aanpassingen, zoals het instellen van verschillende veldscheidingstekens of het afhandelen van variabele aantallen velden per record. Hoewel het niet elke CSV-variant behandelt, werkt het uitstekend voor standaard CSV-formaten.

## Zie ook
Voor meer over het werken met CSV in Go, bekijk deze bronnen:
- De officiële Go-documentatie voor het [`csv` pakket](https://pkg.go.dev/encoding/csv).
- Bekijk [Go by Example](https://gobyexample.com/reading-files) voor aanvullende procedures voor het lezen en schrijven van bestanden.
