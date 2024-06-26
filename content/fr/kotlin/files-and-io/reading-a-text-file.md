---
date: 2024-01-20 17:54:38.362988-07:00
description: 'How to: (Comment faire :) En Kotlin, lire un fichier texte c''est simple.
  Voici un exemple .'
lastmod: '2024-04-05T21:53:59.246888-06:00'
model: gpt-4-1106-preview
summary: (Comment faire :) En Kotlin, lire un fichier texte c'est simple.
title: Lecture d'un fichier texte
weight: 22
---

## How to: (Comment faire :)
En Kotlin, lire un fichier texte c'est simple. Voici un exemple :

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("monFichier.txt")
    val lignes = Files.readAllLines(path)
    lignes.forEach { ligne -> 
        println(ligne) 
    }
}
```

Si `monFichier.txt` contient:

```
Salut, c'est un test.
Seconde ligne !
```

La sortie sera :

```
Salut, c'est un test.
Seconde ligne !
```

## Deep Dive (Plongée profonde)
Historiquement, on lisait les fichiers Byte par Byte. Kotlin, basé sur Java, propose une API moderne : `java.nio`. C'est plus lisible et sûr.

Alternatives ? `File.readLines()` pour les petits fichiers, ou `bufferedReader()` pour les gros fichiers avec `use` qui ferme le flux automatiquement.

Détails d'implémentation : `Files.readAllLines()` utilise le charset par défaut, attention aux encodages. Pour la performance, préférez `Files.newBufferedReader()`.

## See Also (Voir aussi)
- Kotlin Documentation: https://kotlinlang.org/docs/home.html
- File I/O in Java: https://docs.oracle.com/javase/tutorial/essential/io
- Java NIO: https://docs.oracle.com/javase/8/docs/api/java/nio/package-summary.html

Divez dans le code, expérimentez avec différents fichiers et méthodes. Bon coding !
