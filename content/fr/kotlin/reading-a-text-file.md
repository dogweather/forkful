---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?

Lire un fichier texte est l'action d'interpréter et de traiter les informations contenues dans un fichier au format texte dans votre programme. Les développeurs le font pour ingérer des donnés, configurer des paramètres, ou analyse de contenu.

## Comment faire :

Lisant un fichier texte est simple avec Kotlin. Voici comment on fait.

```Kotlin
import java.io.File

fun main(args: Array<String>) {
    val fileName = "chemin_vers_votre_fichier.txt"
    val lines: List<String> = File(fileName).readLines()

    lines.forEach { line -> println(line) }
}
```
La sortie sera le contenu de votre fichier texte.

## Deep Dive

Historiquement, la lecture de fichiers texte est l'une des tâches les plus basiques qu'un système d'exploitation doit accomplir. Dans cet exemple, nous utilisons l'API `java.io.File` qui a été intégrée à Kotlin pour maintenir la compatibilité avec Java.

Il existe plusieurs façons de lire un fichier en Kotlin. L'appel à `readLines()` renvoie une liste d'énoncés. Si vous voulez lire le fichier en une seule chaîne, vous pouvez utiliser `readText()`. 

Ces méthodes sont assez simples pour les fichiers petits à moyens. Pour de très gros fichiers, la méthode `bufferedReader().use { }` est plus efficace car elle utilise un tampon pour réduire les accès au disque.

## Voir aussi :

2. [Tutorial: Lire un fichier en Kotlin](https://www.baeldung.com/kotlin/read-file)
3. [API Java.io.File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)