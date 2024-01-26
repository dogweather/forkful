---
title:                "Lavorare con i file CSV"
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

Lavorare con CSV significa manipolare file di valori separati da virgole, utili per esportare e importare dati in modo semplice. I programmatori lo fanno perché CSV è un formato flessibile e ampiamente supportato per lo scambio dati fra diversi sistemi.

## Come Fare:

Per gestire CSV in Go, usiamo il pacchetto `encoding/csv`. Apriamo un file, leggiamo e scriviamo righe con facilità.

```Go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
    "strings"
)

func main() {
    // Lettura CSV
    csvContent := "nome,cognome,età\nMario,Rossi,30\nLuca,Bianchi,25"
    reader := csv.NewReader(strings.NewReader(csvContent))
    
    for {
        record, err := reader.Read()
        if err != nil {
            break
        }
        fmt.Println(record)
    }

    // Scrittura CSV
    records := [][]string{
        {"nome", "cognome", "età"},
        {"Giulia", "Verdi", "22"},
        {"Marco", "Neri", "35"},
    }
    
    csvFile, err := os.Create("persone.csv")
    if err != nil {
        panic(err)
    }
    writer := csv.NewWriter(csvFile)
    writer.WriteAll(records) // scrivo tutto
    if err := writer.Error(); err != nil {
        panic(err)
    }
}
```

Output lettura:
```
[nome cognome età]
[Mario Rossi 30]
[Luca Bianchi 25]
```

Il file `persone.csv` è stato creato con i dati inseriti.

## Approfondimento:

CSV risale agli anni '70. Alternative moderne includono JSON e XML, ma CSV rimane ideale per grandi volumi di dati semplici. In Go, `encoding/csv` gestisce citazioni, linee vuote e altri dettagli tipici del formato CSV.

## Vedere Anche:

- Documentazione di Go per CSV: https://pkg.go.dev/encoding/csv
- Go by Example con CSV: https://gobyexample.com/reading-files
- Tutorial approfondito su Go e CSV: https://www.thepolyglotdeveloper.com/2017/03/parse-csv-data-go-programming-language/
