---
title:                "Lecture d'un fichier texte"
html_title:           "Kotlin: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Lecture d'un fichier texte est une méthode courante utilisée par les programmeurs pour extraire des données à partir d'un fichier texte. Les fichiers texte sont des fichiers simples contenant du texte et aucun formatage ou code spécial. La lecture d'un fichier texte peut aider les programmeurs à gérer de grandes quantités de données de manière plus efficace.

## Comment faire:

Voici un exemple de code simple en Kotlin pour lire un fichier texte et imprimer chaque ligne:

```Kotlin
fun main() {
    val file = File("fichier.txt")
    file.forEachLine {
        println(it)
    }
}
```

Output:
```
Première ligne de texte
Deuxième ligne de texte
Troisième ligne de texte
...
```

## Plongée en profondeur:

La lecture de fichiers textes est disponible depuis les premiers langages de programmation et a été utilisée pour stocker et récupérer des données avant l'avènement des bases de données. D'autres options pour stocker des données incluent les bases de données relationnelles et les fichiers CSV (valeurs séparées par des virgules). La méthode de lecture de fichiers textes peut être utilisée pour manipuler différents types de données, y compris du texte, des nombres et d'autres caractères.

## À voir également:

Pour en savoir plus sur la lecture de fichiers textes en Kotlin, vous pouvez consulter la documentation officielle de Kotlin sur les opérations de fichiers: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/kotlin.-file-reader.html