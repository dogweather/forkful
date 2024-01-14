---
title:                "Kotlin: Création d'un fichier temporaire"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en Kotlin ?

Lorsque vous développez une application en Kotlin, il peut être utile de créer des fichiers temporaires à des fins de test ou pour stocker temporairement des données. Cela peut vous permettre de gagner du temps et de simplifier votre code en évitant de manipuler des fichiers existants.

## Comment faire pour créer un fichier temporaire en Kotlin ?

Dans Kotlin, vous pouvez utiliser la classe `File` pour créer un fichier temporaire. Voici un exemple de code :

```Kotlin
val fichierTemp = File.createTempFile("prefixe", "suffixe")
println(fichierTemp.absolutePath)
```

Ce code va créer un fichier avec le préfixe "prefixe" et le suffixe "suffixe" dans le répertoire temporaire par défaut du système d'exploitation. Vous pouvez également spécifier un répertoire de destination en utilisant `File.createTempFile("prefixe", "suffixe", directory)`.

Vous pouvez également spécifier un nom de fichier complet en utilisant `File(directory, nomFichier)` pour créer le fichier. Une fois que vous avez terminé d'utiliser le fichier temporaire, vous pouvez le supprimer en appelant `file.delete()`.

## Plongée au coeur de la création de fichiers temporaires

Lorsque vous appelez `File.createTempFile()`, le nom du fichier temporaire est généré automatiquement et il est stocké dans le répertoire temporaire par défaut du système d'exploitation. Cependant, vous pouvez également spécifier un préfixe et un suffixe personnalisés pour le nom du fichier. Cela peut être utile si vous avez besoin de plusieurs fichiers temporaires dans votre application et que vous souhaitez les différencier par leur nom.

Il est également important de noter que les fichiers temporaires créés de cette manière peuvent ne pas être supprimés automatiquement par le système d'exploitation si votre application plante ou se ferme de manière inattendue. Il est donc recommandé de toujours inclure une logique pour supprimer ces fichiers dans votre code.

## Voir aussi

- [Documentation officielle de la classe File en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Tutoriel sur la création de fichiers en Kotlin](https://www.baeldung.com/kotlin-create-file)
- [Exemples de code pour utiliser la classe File en Kotlin](https://www.programiz.com/kotlin-programming/file-handling)