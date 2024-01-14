---
title:                "Kotlin: Vérifier si un répertoire existe"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans la programmation en Kotlin, vous pouvez vous demander pourquoi il est important de vérifier si un répertoire existe. La réponse est simple : cela vous permet de vous assurer que votre code fonctionne correctement et d'éviter les erreurs inattendues.

## Comment faire

La vérification de l'existence d'un répertoire en Kotlin est assez simple. Dans votre code, vous devez d'abord importer la classe `java.io.File` qui vous permettra de contrôler les opérations de fichier et de répertoire. Ensuite, vous pouvez utiliser la méthode `exists()` pour vérifier si un répertoire existe dans le chemin spécifié.

```Kotlin
import java.io.File

fun main() {
    // Définir le chemin du répertoire à vérifier
    val chemin = "./mon-répertoire"

    // Créer un objet File pour représenter le chemin
    val repertoire = File(chemin)

    // Vérifier si le répertoire existe
    if (repertoire.exists()) {
        // Le répertoire existe !
        println("Le répertoire $chemin existe.")
    } else {
        // Le répertoire n'existe pas
        println("Le répertoire $chemin n'existe pas.")
    }
}
```

La sortie de ce code serait `Le répertoire ./mon-répertoire n'existe pas.` car dans cet exemple, le répertoire n'existe pas.

## Plongée en profondeur

Il est important de noter que la méthode `exists()` ne vérifie que l'existence d'un répertoire ou d'un fichier sur le chemin spécifié. Elle ne vérifie pas si vous avez les autorisations pour accéder à ce chemin. Vous devez donc également prendre en compte cela dans votre code.

En outre, si vous souhaitez créer un nouveau répertoire s'il n'existe pas, vous pouvez utiliser la méthode `mkdir()` à la place de `exists()`.

## Voir aussi

- [La documentation officielle sur la classe File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Un tutoriel sur la création et la gestion de fichiers et répertoires en Kotlin](https://www.journaldev.com/4722/java-create-file-directory)
- [Une explication détaillée sur la manipulation de fichiers en Kotlin](https://blog.mindorks.com/using-kotlin-for-manipulating-file-directory)

Maintenant que vous savez comment vérifier si un répertoire existe en Kotlin, vous pouvez l'appliquer dans vos projets pour un code plus robuste et fiable. N'oubliez pas de toujours vérifier si le répertoire existe avant d'effectuer des opérations dessus pour éviter des erreurs inattendues.