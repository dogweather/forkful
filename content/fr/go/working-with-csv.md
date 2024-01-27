---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Lire et écrire des fichiers CSV, c'est manipuler des données tabulaires comme des feuilles Excel en texte brut. Les programmeurs font ça pour échanger des données facilement entre différents systèmes.

## Comment faire :
```Go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
    "strings"
)

func main() {
    // Écrire dans un CSV
    records := [][]string{
        {"Nom", "Ville", "Langage"},
        {"Alice", "Paris", "Go"},
        {"Bob", "Lyon", "Python"},
    }
    
    file, err := os.Create("exemple.csv")
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
    
    // Lire un fichier CSV
    file, err = os.Open("exemple.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    data, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    fmt.Println(data)
}
```
Output :
```
[[Nom Ville Langage] [Alice Paris Go] [Bob Lyon Python]]
```

## Exploration
Le CSV est né dans les premiers jours de l’informatique pour faciliter les échanges de données. Des alternatives comme JSON ou XML existent, mais le CSV reste populaire pour sa simplicité et sa compatibilité omniprésente. En Go, l'encodage et le décodage sont gérés via le package `csv`, qui utilise `io.Reader` et `io.Writer` pour la manipulation des données, permettant ainsi une grande flexibilité.

## Voir aussi :
- Documentation Go sur le package `csv` : https://pkg.go.dev/encoding/csv
- Tutoriel Go sur la manipulation de fichiers CSV : https://golang.org/doc/articles/csv_and_json
- Comparaison entre CSV, JSON, et XML : https://www.dataintegration.info/csv-vs-json-vs-xml
