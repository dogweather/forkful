---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:06.766898-07:00
description: "Comment faire : Kotlin, fonctionnant sur la JVM, utilise l'API File\
  \ de Java pour les op\xE9rations sur les fichiers, rendant les v\xE9rifications\
  \ de l'existence\u2026"
lastmod: '2024-03-13T22:44:57.755864-06:00'
model: gpt-4-0125-preview
summary: "Kotlin, fonctionnant sur la JVM, utilise l'API File de Java pour les op\xE9\
  rations sur les fichiers, rendant les v\xE9rifications de l'existence des r\xE9\
  pertoires directes."
title: "V\xE9rifier si un r\xE9pertoire existe"
weight: 20
---

## Comment faire :
Kotlin, fonctionnant sur la JVM, utilise l'API File de Java pour les opérations sur les fichiers, rendant les vérifications de l'existence des répertoires directes. Voici un exemple simple :

```kotlin
import java.io.File

fun main() {
    val chemin = "/chemin/vers/repertoire"
    val repertoire = File(chemin)

    if (repertoire.exists() && repertoire.isDirectory) {
        println("Le répertoire existe : $chemin")
    } else {
        println("Le répertoire n'existe pas : $chemin")
    }
}
```
Exemple de sortie, en supposant que le répertoire existe :
```
Le répertoire existe : /chemin/vers/repertoire
```
Et s'il n'existe pas :
```
Le répertoire n'existe pas : /chemin/vers/repertoire
```

Dans un projet Kotlin, vous pourriez aussi souvent travailler avec des bibliothèques ou des cadres spécifiques à Kotlin, comme Ktor pour les applications web ou kotlinx.coroutines pour la programmation asynchrone. Cependant, pour vérifier si un répertoire existe, l'API `File` de Java standard comme montré est généralement suffisante et largement utilisée en raison de l'interopérabilité de Kotlin avec Java. Aucune bibliothèque tierce n'est requise pour cette tâche spécifique, la rendant accessible et simple pour les débutants en transition d'autres langages de programmation vers Kotlin.
