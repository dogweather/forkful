---
title:                "Go: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

CSV, ou "Comma-Separated Values", est un format de fichier couramment utilisé pour stocker des données sous forme de tableau. Si vous travaillez avec des données structurées dans un contexte professionnel ou personnel, il est très probable que vous ayez déjà rencontré des fichiers CSV. C'est pourquoi il est essentiel de comprendre comment travailler avec ces fichiers en utilisant le langage de programmation Go.

## Comment faire

La première étape pour travailler avec des fichiers CSV en Go est d'importer le package "encoding/csv" dans votre code. Ensuite, vous pouvez utiliser les fonctions fournies par ce package pour lire et écrire des données à partir de fichiers CSV.

```
import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    // Ouvrir un fichier CSV en lecture
    file, err := os.Open("fichier.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    // Lire le contenu du fichier CSV
    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    // Afficher les données en tant que tableau
    fmt.Println(records)

    // Écrire des données dans un fichier CSV
    data := [][]string{
        {"ID", "Nom", "Âge"},
        {"1", "Marie", "25"},
        {"2", "Pierre", "30"},
    }

    // Ouvrir un fichier CSV en écriture
    file, err = os.Create("fichier.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    // Écrire les données dans le fichier CSV
    writer := csv.NewWriter(file)
    for _, record := range data {
        err = writer.Write(record)
        if err != nil {
            panic(err)
        }
    }
    writer.Flush()
}
```

Output:

```
[["ID" "Nom" "Âge"] ["1" "Marie" "25"] ["2" "Pierre" "30"]]
```

## Plongée en profondeur

Travailler avec des fichiers CSV ne se limite pas seulement à lire et écrire des données. Il est également possible de manipuler ces données en utilisant des fonctions telles que "Read", "ReadAll" et "Write". De plus, en utilisant le package "encoding/csv", vous pouvez spécifier différentes options pour lire et écrire des fichiers CSV, telles que le délimiteur de colonne et les comas dans les valeurs.

## Voir aussi

- [Documentation officielle de Go sur l'utilisation des fichiers CSV](https://golang.org/pkg/encoding/csv/)
- [Un tutoriel vidéo sur la manipulation de fichiers CSV en Go](https://www.youtube.com/watch?v=nuw48-u51dQ)
- [Un guide sur la création de rapports à partir de données CSV en utilisant Go](https://opensource.com/article/19/9/create-csv-reports-go)