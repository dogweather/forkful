---
title:                "Kotlin: Ouverture d'un fichier texte"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes intéressé par la programmation en Kotlin, il est important de savoir comment lire un fichier texte. Cela peut vous être utile pour différentes raisons, telles que le traitement des données ou la lecture de fichiers de configuration.

## Comment faire

Pour lire un fichier texte en utilisant Kotlin, vous pouvez utiliser la classe Java File et la fonction readLines() pour lire toutes les lignes du fichier en tant que liste de chaînes. Voici un exemple de code pour lire un fichier texte nommé "exemple.txt" :

```Kotlin
val file = File("exemple.txt")
val lines = file.readLines()
```
Le contenu du fichier sera stocké dans la variable "lines" sous forme de liste de chaînes. Vous pouvez ensuite traiter ces chaînes selon vos besoins.

Une autre méthode est d'utiliser la classe Kotlin BufferedReader, qui vous permet de lire le fichier ligne par ligne. Voici un exemple de code pour cette méthode :

```Kotlin
val reader = BufferedReader(FileReader("exemple.txt"))
var line: String? = reader.readLine()
while (line != null) {
    // Traitement de chaque ligne
    line = reader.readLine()
}
reader.close()
```

## Deep Dive

Il est important de noter que lors de la lecture d'un fichier texte, il est possible qu'une exception se produise. Vous devriez donc toujours entourer votre code de lecture de fichier avec un bloc try-catch pour gérer ces exceptions.

De plus, il existe d'autres méthodes pour lire un fichier texte en utilisant Kotlin, telles que la fonction forEachLine() et la fonction readText(). Vous pouvez expérimenter avec ces différentes méthodes pour trouver celle qui convient le mieux à votre cas d'utilisation.

## Voir Aussi

- [Documentation officielle de Kotlin sur la gestion des fichiers](https://kotlinlang.org/docs/reference/basic-input-output.html#file-read-write)
- [Guide de démarrrage rapide pour lire des fichiers en Kotlin](https://www.raywenderlich.com/3672936-kotlin-input-output-a-practical-guide-for-ios-developers)