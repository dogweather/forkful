---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Travailler avec des CSV, c'est manipuler des données sous forme de textes structurés en lignes et colonnes, souvent exportées de bases de données ou de feuilles de calcul. Les programmeurs les utilisent pour leur simplicité et leur compatibilité universelle.

## Comment faire :

```kotlin
import java.io.File

fun readCsv(filePath: String): List<List<String>> {
    return File(filePath).useLines { lines ->
        lines.map { it.split(",") }.toList()
    }
}

fun writeCsv(filePath: String, data: List<List<String>>) {
    File(filePath).bufferedWriter().use { out ->
        data.forEach { line ->
            out.write(line.joinToString(","))
            out.newLine()
        }
    }
}

fun main() {
    val csvData = readCsv("data.csv")
    println(csvData)  // Affiche les données lues

    val newData = listOf(listOf("id", "name", "email"), listOf("1", "John Doe", "john@doe.com"))
    writeCsv("newdata.csv", newData)
    // Crée un nouveau fichier CSV avec les nouvelles données
}
```

## Plongée Profonde

Le format CSV, originellement conçu dans les années 1970, reste populaire malgré de nouveaux formats comme JSON et XML, grâce à sa simplicité. Concernant les alternatives, Kotlin permet l'utilisation de bibliothèques spécialisées comme `Apache Commons CSV` ou `kotlin-csv` pour un traitement plus avancé. En interne, la manipulation de CSV implique souvent l'utilisation de lectures et écritures de fichiers textes et le travail sur des listes ou des tableaux de chaînes de caractères.

## Voir Aussi

- Bibliothèque Apache Commons CSV : [commons.apache.org](https://commons.apache.org/proper/commons-csv/)
