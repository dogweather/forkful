---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Kotlin: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Vérifier si un répertoire existe est une opération courante dans la programmation Kotlin. Cela permet aux programmeurs de s'assurer qu'ils peuvent accéder aux fichiers ou dossiers nécessaires pour exécuter leur code. Par exemple, si vous avez besoin de lire un fichier dans un certain répertoire, vous devez d'abord vérifier si ce répertoire existe avant d'essayer d'y accéder.

## Comment faire:

Voici un exemple simple de code montrant comment vérifier si un répertoire existe en utilisant la fonction `exists()` de la classe `File`:

```kotlin
val directory = File("chemin/vers/mon/répertoire")
if(directory.exists()){
    println("Le répertoire existe!")
}else{
    println("Le répertoire n'existe pas.")
}
```

Lorsque vous exécutez ce code, vous verrez le message "Le répertoire existe!" si le répertoire spécifié existe, ou "Le répertoire n'existe pas." s'il n'existe pas.

Il est également possible de vérifier si un répertoire existe en utilisant la fonction `isDirectory()` de la classe `File`. Cette fonction vérifie si le chemin spécifié correspond à un répertoire existant plutôt qu'à un fichier spécifique.

```kotlin
val directory = File("chemin/vers/mon/répertoire")
if(directory.isDirectory()){
    println("C'est un répertoire!")
}else{
    println("Ce n'est pas un répertoire.")
}
```

Il est important de noter que ces fonctions ne créent pas de nouveaux répertoires s'ils n'existent pas déjà. Si vous essayez d'accéder à un répertoire qui n'existe pas, vous obtiendrez une erreur.

## Plongée en profondeur:

La vérification de l'existence d'un répertoire remonte aux premiers jours de la programmation informatique, lorsque les développeurs devaient s'assurer que les fichiers nécessaires étaient disponibles avant de les utiliser. Cependant, avec le développement d'outils de gestion de fichiers plus avancés, la nécessité de cette vérification a diminué. De nos jours, la plupart des systèmes d'exploitation gèrent ces vérifications automatiquement, mais il est toujours important pour les programmeurs de les effectuer pour s'assurer que leur code fonctionne correctement.

Il existe plusieurs alternatives pour vérifier si un répertoire existe en Kotlin. Par exemple, vous pouvez utiliser la fonction `listFiles()` de la classe `File` pour obtenir une liste de tous les fichiers et répertoires dans un chemin spécifié, puis vérifier si le répertoire que vous recherchez est présent dans cette liste.

En termes d'implémentation, la fonction `exists()` utilise un appel système pour vérifier si le répertoire existe, tandis que `isDirectory()` utilise une combinaison d'appels système et de vérifications de type pour déterminer si le chemin spécifié correspond à un répertoire valide.

## Voir aussi:

- La documentation officielle de Kotlin sur la vérification de l'existence de répertoires : https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/exists.html
- Un tutoriel vidéo sur la manipulation de fichiers et de répertoires en Kotlin : https://www.youtube.com/watch?v=wlvJctrBX_s