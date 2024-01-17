---
title:                "Travailler avec des fichiers csv"
html_title:           "Kotlin: Travailler avec des fichiers csv"
simple_title:         "Travailler avec des fichiers csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Travailler avec des fichiers CSV (valeurs séparées par des virgules) est une façon courante pour les programmeurs de stocker et d'organiser des données tabulaires. Les CSV sont largement utilisés dans les applications et les bases de données en raison de leur format simple et facilement lisible.

## Comment faire:

Voici un exemple de code en Kotlin pour lire un fichier CSV et imprimer chaque ligne:

```Kotlin
import java.io.File

fun main() {
    val file = File("data.csv").bufferedReader()
    file.forEachLine { line ->
        println(line)
    }
    file.close()
}
```

Sortie:

```
Colonne 1, Colonne 2, Colonne 3
Valeur 1, Valeur 2, Valeur 3
Valeur 4, Valeur 5, Valeur 6
```

## Plongée en profondeur:

Les CSV ont été créés dans les années 1970 comme un moyen d'importer et d'exporter des données dans un format standardisé. Bien qu'ils soient toujours largement utilisés, il existe maintenant des alternatives telles que JSON et XML pour stocker des données tabulaires. Kotlin offre différentes bibliothèques externes pour travailler avec des fichiers CSV, telles que OpenCSV et Kotlin CSV.

## Voir aussi:

- [Documentation officielle de Kotlin pour le traitement des CSV](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-csv-reader/)
- [OpenCSV: Une bibliothèque pour lire et écrire des fichiers CSV en Java et Kotlin](http://opencsv.sourceforge.net/) 
- [Kotlin CSV: Une bibliothèque simple pour lire et écrire des fichiers CSV en Kotlin](https://github.com/doyaaaaaken/kotlincsv)