---
title:                "Travailler avec des fichiers csv"
html_title:           "Go: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Travailler avec des fichiers CSV est courant pour les programmeurs car c'est un moyen simple et efficace de stocker et manipuler des données tabulaires. Les fichiers CSV (Comma Separated Values) sont des fichiers texte contenant des données organisées en colonnes séparées par des virgules, ce qui les rend faciles à lire et à éditer avec du code.

## Comment:
Le processus pour travailler avec des fichiers CSV en Go est assez simple. Tout d'abord, nous devons importer le package "encoding/csv". Ensuite, nous pouvons utiliser la fonction "ReadFile()" pour lire un fichier CSV et stocker les données dans une variable. Enfin, nous pouvons utiliser la fonction "ReadAll()" pour parcourir et manipuler les données dans la variable. Voici un exemple de code et sa sortie:

```
package main

import (
    "encoding/csv"
    "fmt"
    "log"
    "os"
)

func main() {
    // Lecture du fichier CSV
    f, err := os.Open("donnees.csv")
    if err != nil {
        log.Fatal(err)
    }

    // Stockage des données dans une variable
    donnees, err := csv.NewReader(f).ReadAll()
    if err != nil {
        log.Fatal(err)
    }

    // Parcours et manipulation des données
    for _, ligne := range donnees {
        fmt.Println(ligne[0], ligne[1])
    }
}
```

Output :

```
Alice Smith
Bob Johnson
Carla Davis
```

## Plongée en profondeur:
Les fichiers CSV ont été initialement développés pour être utilisés avec des tableurs, mais ils sont maintenant largement utilisés dans le domaine de la programmation en raison de leur simplicité et de leur compatibilité avec de nombreux langages de programmation. Une alternative à l'utilisation de fichiers CSV est d'utiliser une base de données, mais cela peut être plus complexe à mettre en place et à utiliser. Pour ceux qui souhaitent plus d'informations sur la manipulation de fichiers CSV en Go, le package officiel "encoding/csv" a une documentation complète et claire.

## Voir aussi:
Pour en savoir plus sur le travail avec des fichiers CSV en Go, voici quelques ressources utiles:

- [Documentation du paquet "encoding/csv"](https://golang.org/pkg/encoding/csv/)