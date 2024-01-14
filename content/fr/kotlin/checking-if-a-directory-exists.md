---
title:                "Kotlin: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

S'il vous arrive de travailler avec des fichiers et des dossiers dans vos projets de programmation en Kotlin, il est important de savoir comment vérifier si un dossier existe avant de le manipuler. Cela peut vous éviter des erreurs et des problèmes de compatibilité avec d'autres systèmes. Dans cet article, nous allons plonger dans la façon de vérifier si un dossier existe en utilisant Kotlin.

## Comment faire

Il existe plusieurs façons de vérifier si un dossier existe en Kotlin. Voici un exemple simple en utilisant la méthode `exists()` de la classe `File` :

```Kotlin
val directory = File("/chemin/vers/votre/dossier")

if (directory.exists()) {
    println("Le dossier existe.")
} else {
    println("Le dossier n'existe pas.")
}
```

Output :
```
Le dossier existe.
```

Vous pouvez également utiliser la méthode `isDirectory()` pour vérifier si un fichier est un dossier ou non :

```Kotlin
val file = File("/chemin/vers/votre/fichier")

if (file.isDirectory()) {
    println("C'est un dossier.")
} else {
    println("Ce n'est pas un dossier.")
}
```

Output :
```
Ce n'est pas un dossier.
```

Une autre approche consiste à utiliser la méthode `listFiles()` pour obtenir la liste des fichiers et dossiers à l'intérieur d'un dossier donné. Si cette méthode retourne une liste vide, cela signifie que le dossier est vide ou n'existe pas.

```Kotlin
val directory = File("/chemin/vers/votre/dossier")
val files = directory.listFiles()

if (files.isNullOrEmpty()) {
    println("Le dossier est vide ou n'existe pas.")
} else {
    // Effectuer d'autres opérations sur les fichiers/dossiers présents
}
```

Output :
```
Le dossier est vide ou n'existe pas.
```

## Plongée plus profonde

Il est important de noter que la méthode `exists()` peut renvoyer `true` même si le chemin donné mène à un fichier et non à un dossier. Cela peut être déroutant pour certains et il est donc recommandé d'utiliser également la méthode `isDirectory()` pour une vérification plus précise.

De plus, il est important de prendre en compte le système d'exploitation sur lequel vous travaillez. Par exemple, sur Windows, les noms de fichiers sont insensibles à la casse tandis que sur Linux, ils sont sensibles à la casse. Cela peut entraîner des problèmes de compatibilité lors de la vérification de l'existence d'un dossier. Il est donc important d'en tenir compte lors de la mise en œuvre de cette fonctionnalité dans vos projets.

## Voir aussi

- [Documentation officielle de Kotlin sur la classe File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Tutoriel sur la manipulation de fichiers et de dossiers en Kotlin](https://www.baeldung.com/kotlin/input-output)
- [Référence de la classe File en Java](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)