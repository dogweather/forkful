---
title:                "Création d'un fichier temporaire"
aliases:
- fr/kotlin/creating-a-temporary-file.md
date:                  2024-01-20T17:40:51.109082-07:00
model:                 gpt-4-1106-preview
simple_title:         "Création d'un fichier temporaire"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Créer un fichier temporaire permet de stocker des données qui ne sont nécessaires que brièvement. Les programmeurs le font pour gérer l'espace de stockage efficacement et garder des données volatiles sans encombrer la mémoire permanente.

## How to:
Kotlin rend la création de fichiers temporaires super facile. Voici comment :

```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    // Créer un fichier temporaire
    val tempFile = Files.createTempFile(Paths.get("/tmp"), "monApp_", ".tmp")

    // Écrire dans le fichier temporaire
    Files.write(tempFile, "Contenu de démo".toByteArray())

    // Lire le contenu du fichier temporaire
    val content = Files.readAllBytes(tempFile)
    println(String(content))

    // Supprimer le fichier temporaire
    Files.deleteIfExists(tempFile)
}

// Sample output
// Contenu de démo
```

## Deep Dive
Les fichiers temporaires existent depuis les premiers jours de la programmation. Ils minimisent l'usage de la mémoire en stockant temporairement les données non essentielles. 
En Kotlin, `java.nio.file.Files` est utilisé car il fournit une API robuste et moderne pour travailler avec des fichiers. Des alternatives incluent `java.io.File`, mais `java.nio` est plus récent et généralement préféré pour sa facilité d'utilisation et ses performances.
Un fichier temporaire est souvent situé dans un répertoire spécifique du système comme `/tmp` sur les système Unix. Il porte généralement un nom unique pour éviter les conflits et est supprimé automatiquement ou manuellement après usage.

## See Also
- [Java NIO File API](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Handling Files in Kotlin](https://kotlinlang.org/docs/idioms.html#working-with-files)
