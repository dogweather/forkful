---
title:                "Écrire un fichier texte"
html_title:           "Kotlin: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi 

Ecrire un fichier texte peut sembler être une tâche banale, mais c'est en fait une compétence essentielle pour tout développeur Kotlin. Cela vous permet de stocker et de manipuler des données de manière simple et efficace, ce qui peut être très utile pour de nombreux projets.

## Comment faire 

Pour écrire un fichier texte en Kotlin, suivez ces étapes simples: 

1. Tout d'abord, importez les classes nécessaires en utilisant l'instruction `import java.io.File` 
2. Créez une instance de la classe `File`, en spécifiant le chemin et le nom du fichier que vous souhaitez créer. Par exemple, `val file = File("monfichier.txt")` 
3. Utilisez la méthode `writeText()` pour écrire du texte dans votre fichier. Par exemple, `file.writeText("Bonjour le monde!")` 
4. Si vous souhaitez ajouter du texte à un fichier existant, utilisez la méthode `appendText()` au lieu de `writeText()`. 
5. N'oubliez pas de gérer les exceptions lors de l'utilisation de ces méthodes en utilisant des blocs `try-catch`.

Voici un exemple complet de code pour écrire du texte dans un fichier et le récupérer ensuite en tant que chaîne de caractères:

```Kotlin
import java.io.File

fun main() {
  // Crée un fichier "monfichier.txt"
  val file = File("monfichier.txt")
  
  // Ecrire du texte dans le fichier
  file.writeText("Bonjour le monde!")
  
  // Récupère le contenu du fichier en tant que chaîne de caractères
  val content = file.readText()
  
  println(content) // Affiche "Bonjour le monde!"
}
```

## Plongée en profondeur 

Maintenant que vous savez comment écrire et récupérer du texte dans un fichier en utilisant Kotlin, voici quelques informations supplémentaires pour améliorer vos connaissances sur le sujet:

- Vous pouvez également utiliser la méthode `printWriter()` pour écrire du texte dans un fichier avec plus de flexibilité, en spécifiant l'encodage, le format de nouvelle ligne, etc.
- Outre les méthodes `writeText()` et `appendText()`, vous pouvez également utiliser `writeBytes()` pour écrire des données binaires dans un fichier.
- Kotlin offre également une syntaxe de blocs `use` qui s'assure que le fichier est correctement fermé même en cas d'exception. Par exemple:

```Kotlin
File("monfichier.txt").printWriter().use { out ->
  out.println("Bonjour le monde!")
}
```

## Voir aussi 

- [Documentation officielle Kotlin pour les entrées-sorties](https://kotlinlang.org/docs/tutorials/kotlin-for-py/io.html)
- [Tutoriel sur la manipulation de fichiers en Kotlin](https://www.tutorialkart.com/kotlin/kotlin-write-to-file/)
- [Documentation officielle Java pour la classe `File`](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)