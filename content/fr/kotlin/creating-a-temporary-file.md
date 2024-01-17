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

("Quoi et Pourquoi?")
Création d'un Fichier Temporaire en Kotlin: Un Aperçu pour les Programmateurs

La création d'un fichier temporaire est une tâche courante pour les programmateurs. Il s'agit d'un fichier qui est créé lors de l'exécution d'un programme et qui est utilisé temporairement pour stocker des données. Les programmateurs utilisent souvent des fichiers temporaires pour stocker des informations de travail et les supprimer une fois qu'elles ne sont plus nécessaires.

("Comment faire?")
Voici un exemple simple de création d'un fichier temporaire en Kotlin:

```Kotlin
import java.io.File
fun main() {
  val fichierTemp = File.createTempFile("tempFichier", ".txt")
  println("Le fichier temporaire a été créé à l'emplacement suivant: ${fichierTemp.absolutePath}")
}
```

La sortie de ce code sera quelque chose comme ceci:

Le fichier temporaire a été créé à l'emplacement suivant: /users/nom_utilisateur/AppData/Local/Temp/tempFichier6849882715328485.txt

Vous pouvez également spécifier le répertoire dans lequel vous souhaitez que le fichier temporaire soit créé en utilisant la méthode `createTempFile()`:

```Kotlin
// Crée un fichier temporaire dans le répertoire spécifié
val fichierTemp = File.createTempFile("tempFichier", ".txt", File("chemin_vers_le_repertoire"))
```

Si vous souhaitez écrire des données dans le fichier temporaire, vous pouvez utiliser la classe `FileWriter`. Voici un exemple:

```Kotlin
import java.io.File
import java.io.FileWriter

fun main() {
  val fichierTemp = File.createTempFile("tempFichier", ".txt")
  val writer = FileWriter(fichierTemp)
  writer.write("Contenu du fichier temporaire")
  writer.close()
}
```

("Plongée en profondeur")
La création de fichiers temporaires est courante depuis longtemps dans le monde de la programmation. Historiquement, les fichiers temporaires étaient créés en utilisant les opérations de système de fichiers bas niveau dans les langages de programmation. Cependant, avec l'avènement de langages de programmation comme Kotlin, il est maintenant plus facile de créer et de manipuler des fichiers temporaires en utilisant des méthodes et des classes dédiées.

Une alternative à la création de fichiers temporaires est l'utilisation de variables temporaires en mémoire, appelées "variables tampons". Cependant, les fichiers temporaires peuvent être plus efficaces pour stocker des données volumineuses.

L'implémentation de la création de fichiers temporaires en Kotlin est basée sur la bibliothèque standard de Java. Cela garantit des performances élevées et une bonne intégration avec d'autres bibliothèques Java.

("Voir aussi")
Pour plus d'informations sur la création de fichiers temporaires en Kotlin, vous pouvez consulter la documentation officielle de Kotlin (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html).

Vous pouvez également trouver utile de consulter la documentation de Java sur la classe `File` (https://docs.oracle.com/javase/8/docs/api/java/io/File.html).