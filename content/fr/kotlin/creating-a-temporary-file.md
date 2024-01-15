---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi créer un fichier temporaire en programmation Kotlin ? Eh bien, il peut être utile de stocker temporairement des données ou des informations avant de les enregistrer définitivement ou de les utiliser dans une autre partie du code.

## Comment créer un fichier temporaire en Kotlin

La création d'un fichier temporaire en Kotlin est facile grâce à la fonction `createTempFile()` de la classe File. Voici un exemple de code qui crée un fichier temporaire nommé "exemple.txt" dans le répertoire système temporaire :

```Kotlin
val file = File.createTempFile("exemple", ".txt")
```

Ce fichier sera automatiquement supprimé lorsque le programme se terminera. Si vous souhaitez spécifier vous-même le répertoire où le fichier temporaire sera créé, vous pouvez utiliser la fonction `createTempFile()` qui prend en paramètre le nom du fichier et le chemin du répertoire :

```Kotlin
val file = File.createTempFile("exemple", ".txt", File("chemin/vers/repertoire/temporaire"))
```

Vous pouvez également définir le préfixe et le suffixe du nom du fichier temporaire en utilisant la fonction `createTempFile()` avec un troisième paramètre optionnel :

```Kotlin
val file = File.createTempFile("prefixe", "suffixe", File("chemin/vers/repertoire/temporaire"))
```

Une fois le fichier temporaire créé, vous pouvez l'utiliser comme n'importe quel autre fichier en Kotlin, en lui écrivant ou en le lisant.

## Plongée en profondeur

Il est important de noter que chaque fois que vous créez un fichier temporaire en Kotlin, un nom unique sera généré pour le fichier. Cela signifie que si vous créez plusieurs fichiers temporaires avec le même préfixe et suffixe, ils auront tous des noms différents. De plus, la suppression automatique du fichier temporaire ne se produira que si le programme se termine correctement, donc si vous rencontrez des erreurs ou des exceptions, le fichier temporaire peut ne pas être supprimé.

## Voir aussi

Pour plus d'informations sur la manipulation des fichiers en Kotlin, vous pouvez consulter les liens suivants :

- [Documentation officielle - Classe File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Créer et manipuler des fichiers en Kotlin](https://www.programiz.com/kotlin-programming/file-handling)
- [Tutoriel vidéo - Gestion des fichiers en Kotlin](https://www.youtube.com/watch?v=VNT3-hdpTZo)