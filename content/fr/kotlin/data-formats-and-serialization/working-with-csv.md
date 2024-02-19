---
aliases:
- /fr/kotlin/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:28.539271-07:00
description: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ implique de lire et d'\xE9crire des donn\xE9es dans des fichiers CSV, un format\
  \ commun pour\u2026"
lastmod: 2024-02-18 23:09:08.805728
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ implique de lire et d'\xE9crire des donn\xE9es dans des fichiers CSV, un format\
  \ commun pour\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec des fichiers CSV (Valeurs Séparées par des Virgules) implique de lire et d'écrire des données dans des fichiers CSV, un format commun pour le stockage des données tabulaires en texte brut. Les programmeurs manipulent les fichiers CSV pour faciliter l'échange de données entre différentes applications, bases de données, ou pour faciliter les tâches de traitement et d'analyse des données.

## Comment :

Kotlin, étant un langage de programmation à typage statique qui s'exécute sur la JVM, n'inclut pas de bibliothèque intégrée pour la manipulation des fichiers CSV. Cependant, vous pouvez utiliser les classes Java `BufferedReader` et `FileWriter` pour les opérations de base, ou profiter de bibliothèques tierces populaires comme `kotlinx.serialization` et `opencsv` pour des fonctionnalités plus avancées.

### Lire un fichier CSV en utilisant BufferedReader :

```kotlin
import java.io.BufferedReader
import java.io.FileReader

fun main() {
    val chemin = "data.csv"
    val br = BufferedReader(FileReader(chemin))
    br.useLines { lignes ->
        lignes.forEach { ligne ->
            val cols = ligne.split(',')
            println(cols)
        }
    }
}
```

_Exemple de sortie :_

```
[Name, Age, City]
[John Doe, 30, New York]
[Jane Smith, 25, London]
```

### Écrire dans un fichier CSV en utilisant FileWriter :

```kotlin
import java.io.FileWriter

fun main() {
    val données = listOf(
        listOf("Name", "Age", "City"),
        listOf("John Doe", "30", "New York"),
        listOf("Jane Smith", "25", "London")
    )

    FileWriter("output.csv").use { writer ->
        données.forEach { row ->
            writer.write(row.joinToString(",") + "\n")
        }
    }
}
```

Cela va créer ou remplacer `output.csv` avec les données fournies.

### Utilisation de kotlinx.serialization pour la sérialisation CSV :

D'abord, ajoutez la dépendance à votre `build.gradle.kts` :

```kotlin
implementation("org.jetbrains.kotlinx:kotlinx-serialization-csv:0.3.0")
```

_Remarque : Assurez-vous d'avoir la bonne version et configuration du dépôt._

Ensuite, définissez votre classe de données et utilisez le format `Csv` pour la sérialisation :

```kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.csv.Csv
import kotlinx.serialization.encodeToString

@Serializable
data class Personne(val nom: String, val âge: Int, la ville: String)

fun main() {
    val formatCsv = Csv { delimiter = ',' }
    val données = listOf(
        Personne("John Doe", 30, "New York"),
        Personne("Jane Smith", 25, "London")
    )

    val donnéesCsv = formatCsv.encodeToString(données)
    println(donnéesCsv)
}
```

_Exemple de sortie :_

```
John Doe,30,New York
Jane Smith,25,London
```

### Utilisation d'OpenCSV pour des opérations avancées :

Ajoutez OpenCSV aux dépendances de votre projet :

```kotlin
implementation("com.opencsv:opencsv:5.6")
```

Lecture et écriture avec OpenCSV :

```kotlin
import com.opencsv.CSVReader
import com.opencsv.CSVWriter
import java.io.FileReader
import java.io.FileWriter

fun main() {
    // Lecture CSV
    CSVReader(FileReader("data.csv")).use { lecteurCsv ->
        val entrées = lecteurCsv.readAll()
        entrées.forEach { println(it.toList()) }
    }

    // Écriture CSV
    CSVWriter(FileWriter("output.csv")).use { rédacteurCsv ->
        val entrées = listOf(
            arrayOf("Name", "Age", "City"),
            arrayOf("John Doe", "30", "New York"),
            arrayOf("Jane Smith", "25", "London")
        )
        rédacteurCsv.writeAll(entrées)
    }
}
```

Ces extraits de code démontrent la flexibilité que Kotlin offre lorsque vous travaillez avec des fichiers CSV, vous permettant de choisir la méthode qui correspond le mieux aux besoins de votre projet.
