---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:37.701172-07:00
description: "Comment faire : Travailler avec des fichiers CSV en Go est simple, gr\xE2\
  ce \xE0 sa biblioth\xE8que standard, `encoding/csv`. Ci-dessous, un guide pour lire\
  \ et\u2026"
lastmod: '2024-03-13T22:44:57.160397-06:00'
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV en Go est simple, gr\xE2ce \xE0 sa biblioth\xE8\
  que standard, `encoding/csv`."
title: Travailler avec CSV
weight: 37
---

## Comment faire :
Travailler avec des fichiers CSV en Go est simple, grâce à sa bibliothèque standard, `encoding/csv`. Ci-dessous, un guide pour lire et écrire des fichiers CSV.

### Lire un fichier CSV
Pour lire à partir d'un fichier CSV, vous commencez d'abord par ouvrir le fichier en utilisant `os.Open`, puis vous créez un nouveau lecteur CSV avec `csv.NewReader`.

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

Ce morceau de code lira tous les enregistrements de `data.csv` et les imprimera. Chaque enregistrement est un tableau de champs.

### Écrire dans un fichier CSV
Pour écrire, vous utilisez `csv.NewWriter` et `writer.WriteAll` ou `writer.Write` pour écrire respectivement plusieurs ou un seul enregistrement CSV.

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

Cela créera un fichier nommé `output.csv` avec les enregistrements fournis. Souvenez-vous toujours de vider le tampon du writer pour garantir que toutes les données en mémoire tampon sont écrites dans le fichier.

## Plongée profonde
Le package `encoding/csv` de Go offre un support robuste pour la lecture et l'écriture de fichiers CSV, mais il est conçu dans un esprit de simplicité, ce qui signifie qu'il ne gère pas des scénarios plus complexes comme la détection automatique des délimiteurs, la manipulation des guillemets, ou les sauts de ligne intégrés dans les champs sans traitement manuel.

Historiquement, la gestion des CSV dans les langages de programmation a souvent été laborieuse en raison de ces complexités, mais la bibliothèque standard de Go abstrait beaucoup de ces problèmes, permettant aux développeurs de travailler avec des données CSV avec une relative facilité. Cependant, pour une manipulation CSV plus complexe, des bibliothèques tierces comme `gocsv` ou le traitement manuel de l'analyse peuvent être nécessaires.

Un aspect remarquable du package `csv` de Go est sa prise en charge pour la spécification d'une virgule personnalisée (délimiteur), qui lui permet de fonctionner sans problème avec des variantes de fichiers CSV, comme les valeurs séparées par des tabulations (TSV). Cependant, lors du traitement de fichiers CSV hautement irréguliers ou non standards, les programmeurs Go pourraient se trouver dans le besoin d'étendre les implémentations existantes du lecteur ou de l'écrivain csv.

Alors que les capacités de gestion des CSV de Go sont robustes pour des fins générales, pour des applications nécessitant une manipulation intensive des données, comme la science des données ou des tâches de transformation de données complexes, les programmeurs pourraient étudier des packages de traitement de données dédiés, ou même d'autres langues mieux adaptées à ces tâches, comme Python avec sa bibliothèque `pandas`. Néanmoins, pour des opérations de lecture et d'écriture de CSV simples, la bibliothèque standard de Go se démarque par son efficacité et sa simplicité.
