---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:57:22.928520-07:00
simple_title:         "Vérifier si un répertoire existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Vérifier si un dossier existe permet d'éviter les erreurs lors de la manipulation de fichiers. Les programmeurs le font pour s'assurer que leur code gère les fichiers de manière fiable et préventive.

## Comment faire :
```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/chemin/vers/le/dossier")

    if (Files.exists(path)) {
        println("Le dossier existe.")
    } else {
        println("Le dossier n'existe pas.")
    }
}
```
Sortie échantillon :
```
Le dossier existe.
```
ou
```
Le dossier n'existe pas.
```

## Immersion profonde
Historiquement, vérifier l'existence d'un dossier était plus compliqué et moins sûr. Avec Java NIO (Non-blocking I/O), introduit dans Java 7 et disponible en Kotlin, ce processus est simplifié. Il y a des alternatives comme utiliser `File.exists()` de l'API `java.io.File`, mais `Files.exists()` de `java.nio.file.Files` est plus moderne et offre une meilleure performance avec un code plus lisible. Cependant, `Files.exists()` peut ne pas être fiable à 100% dans tous les cas de systèmes de fichiers et peut nécessiter une combinaison de vérifications pour des cas d'usage spécifiques.

## Voir aussi
- Documentation Kotlin sur la manipulation de fichiers : [Kotlin File Handling](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- API Java NIO Files : [Java NIO Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- Guide de migration de `java.io.File` à `java.nio.file` : [Migrating from File to Path](https://docs.oracle.com/javase/tutorial/essential/io/legacy.html)
