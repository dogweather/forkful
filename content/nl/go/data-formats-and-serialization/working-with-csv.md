---
title:                "Werken met CSV"
date:                  2024-02-03T18:11:57.759704-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het CSV-formaat (Comma-Separated Values) is alomtegenwoordig voor gegevensuitwisseling vanwege zijn eenvoud en gemak van integratie met de meeste programmeertalen, inclusief Go. Programmeurs werken vaak met CSV-bestanden voor gegevensmigratie, rapportagegeneratie of gegevensanalyse, waardoor een begrip van CSV-manipulatie essentieel is in een softwareontwikkelingsgereedschapskist.

## Hoe:

Werken met CSV-bestanden in Go is eenvoudig, dankzij de standaardbibliotheek, `encoding/csv`. Hieronder volgt een inleiding over het lezen en schrijven van CSV-bestanden.

### Een CSV-bestand lezen

Om een CSV-bestand te lezen, open je eerst het bestand met `os.Open`, en vervolgens maak je een nieuwe CSV-lezer aan met `csv.NewReader`.

```go
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

Deze codefragment zal alle records uit `data.csv` lezen en ze afdrukken. Elk record is een reeks velden.

### Naar een CSV-bestand schrijven

Voor het schrijven gebruik je `csv.NewWriter` en `writer.WriteAll` of `writer.Write` voor respectievelijk het schrijven van meerdere of een enkel CSV-record.

```go
package main

import (
    "encoding/csv"
    "os"
)

func main() {
    file, err := os.Create("output.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    records := [][]string{
        {"Name", "Age", "City"},
        {"John Doe", "30", "New York"},
        {"Jane Doe", "27", "Los Angeles"},
    }

    if err := writer.WriteAll(records); err != nil {
        panic(err)
    }
}
```

Dit zal een bestand met de naam `output.csv` creëren met de opgegeven records. Vergeet niet om de writer te flushen om ervoor te zorgen dat alle gebufferde gegevens naar het bestand worden geschreven.

## Diepgaand

Het Go `encoding/csv` pakket biedt robuuste ondersteuning voor het lezen en schrijven van CSV-bestanden, maar het is ontworpen met eenvoud in gedachten, wat betekent dat het niet automatisch omgaat met meer complexe scenario's zoals het autodetecteren van scheidingstekens, omgaan met aanhalingstekens of ingesloten regelonderbrekingen in velden zonder handmatige afhandeling.

Historisch gezien is het omgaan met CSV in programmeertalen vaak omslachtig geweest vanwege deze complexiteiten, maar de standaardbibliotheek van Go abstraheert veel van deze problemen, waardoor ontwikkelaars met relatief gemak met CSV-gegevens kunnen werken. Voor meer complexe CSV-manipulatie kunnen echter bibliotheken van derden zoals `gocsv` nodig zijn of de parsing handmatig behandeld worden.

Een opmerkelijk aspect van Go's `csv` pakket is de ondersteuning voor het specificeren van aangepaste komma's (scheidingstekens), waardoor het naadloos kan werken met varianten van CSV-bestanden, zoals door tabs gescheiden waarden (TSV). Echter, bij het omgaan met sterk onregelmatige of niet-standaard CSV-bestanden, vinden Go-programmeurs zich mogelijk genoodzaakt de bestaande csv-lezer- of schrijverimplementaties uit te breiden.

Hoewel de CSV-afhandelingsmogelijkheden van Go robuust zijn voor algemene doeleinden, kunnen programmeurs voor applicaties die intensieve gegevensmanipulatie vereisen, zoals datawetenschap of complexe gegevenstransformatietaken, kijken naar gespecialiseerde gegevensverwerkingspakketten of zelfs naar andere talen die beter geschikt zijn voor deze taken, zoals Python met zijn `pandas`-bibliotheek. Niettemin steekt de standaardbibliotheek van Go uit in efficiëntie en eenvoud voor rechttoe rechtaan CSV-lees-schrijfbewerkingen.
