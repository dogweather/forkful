---
title:                "Travailler avec les fichiers csv"
html_title:           "Go: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-csv.md"
---

{{< edit_this_page >}}

Le petit guide décontracté pour travailler avec des fichiers CSV en Go

## Pourquoi

Si vous travaillez avec des données tabulaires, telles que des feuilles de calcul ou des bases de données, vous allez probablement rencontrer des fichiers CSV. Ces fichiers sont très couramment utilisés pour stocker et échanger des données tabulaires, et travailler avec eux peut grandement simplifier le traitement de ces données.

## Comment faire

Pour travailler avec des fichiers CSV en Go, vous pouvez utiliser la librairie standard encoding/csv. Tout d'abord, importez cette librairie dans votre code :

```Go
import "encoding/csv"
```

Ensuite, vous devrez ouvrir votre fichier CSV avec la fonction `Open` et un `os.File` :

```Go
file, err := os.Open("mon_fichier.csv")
```

Puis, vous pouvez utiliser la fonction `NewReader` de la librairie encoding/csv pour créer un lecteur de CSV à partir du fichier ouvert :

```Go
reader := csv.NewReader(file)
```

Maintenant que vous avez un lecteur de CSV, vous pouvez parcourir les données ligne par ligne à l'aide de la boucle `for` :

```Go
for {
    // Lit une ligne
    record, err := reader.Read()
    // Vérifie s'il y a une fin de fichier
    if err == io.EOF {
        break
    }
    // Traitement de la ligne
    fmt.Println(record)
}
```

Si vous avez besoin d'accéder à des données spécifiques d'une ligne, vous pouvez utiliser l'index de `record` :

```Go
// Accède à la première colonne de la deuxième ligne
fmt.Println(record[0])
```

Et voilà ! Vous avez maintenant toutes les bases pour travailler avec des fichiers CSV en Go.

## Plongée en profondeur

La librairie encoding/csv offre également plusieurs options pour personnaliser votre traitement de fichiers CSV. Vous pouvez, par exemple, spécifier un caractère de délimitation différent de la virgule par défaut, ou ignorer la première ligne du fichier si elle contient un en-tête. Pour plus d'informations sur les options disponibles, vous pouvez consulter la documentation officielle de la librairie Go.

## Voir aussi

- Documentation de la librairie encoding/csv : https://golang.org/pkg/encoding/csv/
- Exemples de code pour travailler avec des fichiers CSV en Go : https://gobyexample.com/reading-files