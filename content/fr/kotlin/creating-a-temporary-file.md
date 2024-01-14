---
title:                "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire est une tâche courante en programmation. Il est utile lorsque vous avez besoin d'un espace de stockage temporaire pour stocker des données ou lorsque vous devez effectuer des opérations sur un fichier sans risquer d'écraser les données existantes. Dans cet article, nous allons explorer comment créer un fichier temporaire en utilisant Kotlin.

## Comment faire

La création d'un fichier temporaire en Kotlin est très simple. Tout d'abord, nous devons importer la classe "java.io.File" qui nous permettra de travailler avec des fichiers.

```
Kotlin

import java.io.File

```

Ensuite, nous pouvons utiliser la fonction "createTempFile()" pour créer notre fichier temporaire. Cette fonction prend deux arguments, le nom du fichier et l'extension. Voyons un exemple concret:

```
Kotlin

val monFichierTemp = File.createTempFile("example", ".txt")

```

Ici, nous avons créé un fichier temporaire nommé "example" avec l'extension ".txt". Si vous voulez spécifier le répertoire où le fichier sera créé, vous pouvez utiliser une deuxième version de la fonction "createTempFile()" qui prend un troisième argument pour le chemin du répertoire.

```
Kotlin

val monFichierTemp = File.createTempFile("example", ".txt", "chemin/vers/le/répertoire")

```

Maintenant que nous avons créé notre fichier temporaire, nous pouvons effectuer des opérations dessus, par exemple écrire des données dans le fichier et les lire.

```
Kotlin

// Écrire dans le fichier
monFichierTemp.writeText("Ceci est un exemple de données dans notre fichier temporaire")

// Lire à partir du fichier
val contenu = monFichierTemp.readText()
println(contenu) // Résultat : Ceci est un exemple de données dans notre fichier temporaire

```

Enfin, n'oubliez pas de supprimer le fichier temporaire une fois que vous avez terminé de l'utiliser. Cela peut être fait en utilisant la fonction "delete()".

```
Kotlin

monFichierTemp.delete() // Supprime le fichier temporaire

```

## Profondeur de plongée

En plongeant un peu plus profondément, nous pouvons également spécifier un préfixe et un suffixe personnalisés pour notre fichier temporaire en utilisant une autre version de la fonction "createTempFile()".

```
Kotlin

val monFichierTemp = File.createTempFile("prefixe", "suffixe", "chemin/vers/le/répertoire")

```

Cela peut être utile si vous souhaitez avoir un meilleur contrôle sur le nom du fichier temporaire créé.

## Voir aussi

Maintenant que vous savez comment créer un fichier temporaire en utilisant Kotlin, voici quelques liens pour approfondir vos connaissances:

- [Documentation officielle sur la classe File de Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Tutoriel pour apprendre Kotlin pour les débutants](https://www.raywenderlich.com/1436737-kotlin-tutorial-for-beginners-getting-started)
- [GitHub pour Kotlin](https://github.com/JetBrains/kotlin)

Merci d'avoir lu cet article et n'hésitez pas à utiliser des fichiers temporaires dans vos prochains projets Kotlin !