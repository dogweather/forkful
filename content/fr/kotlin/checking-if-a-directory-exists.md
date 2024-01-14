---
title:    "Kotlin: Vérifier si un répertoire existe"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Saviez-vous qu'il est possible de vérifier si un répertoire existe en utilisant du code Kotlin ? Bien que cela puisse sembler trivial, cette fonctionnalité peut en fait être très utile pour les programmeurs. Dans cet article, nous allons explorer pourquoi il est important de vérifier l'existence d'un répertoire et comment le faire en utilisant Kotlin.

## Comment faire

Tout d'abord, il est important de comprendre que pour vérifier si un répertoire existe, nous avons besoin d'accéder au système de fichiers de l'ordinateur. Kotlin offre une fonction très pratique pour cela, appelée `exists()`. Voici un exemple de code montrant comment utiliser cette fonction pour vérifier si un répertoire nommé "documents" existe :

```Kotlin
val directory = File("documents")
if (directory.exists()) {
    println("Le répertoire 'documents' existe !")
} else {
    println("Le répertoire 'documents' n'existe pas.")
}
```

Si le répertoire existe, le code ci-dessus affichera "Le répertoire 'documents' existe !". Sinon, il affichera "Le répertoire 'documents' n'existe pas.".

## Plongée en profondeur

Maintenant que nous savons comment vérifier si un répertoire existe, il est important de comprendre pourquoi cela peut être utile dans la programmation. Tout d'abord, cela peut aider à éviter les erreurs. Par exemple, si votre programme a besoin d'accéder à un répertoire pour y stocker des fichiers, il est important de vérifier son existence avant de tenter de le trouver ou de le créer. Sinon, votre programme risque de planter s'il ne trouve pas le répertoire ou s'il ne peut pas le créer.

De plus, il est également possible de vérifier l'existence d'un fichier en utilisant la même fonction `exists()`, en spécifiant le chemin complet du fichier. Cela peut être très utile si vous souhaitez vérifier avant d'écraser un fichier existant ou d'en créer un nouveau avec le même nom.

## Voir aussi

- [Documentation officielle pour la classe File de Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-file/)
- [Tutoriel sur la gestion des fichiers avec Kotlin](https://www.tutorialkart.com/kotlin/file-handling-create-read-write-kotlin/)
- [Exemples de code pour vérifier si un répertoire existe en utilisant Kotlin](https://www.tutorial2learn.com/kotlin-check-directory-if-it-is-empty-or-no-empty)

Nous espérons que cet article vous a aidé à comprendre l'importance de vérifier si un répertoire existe en programmation Kotlin. N'hésitez pas à explorer davantage cette fonctionnalité et à l'utiliser dans vos propres projets !