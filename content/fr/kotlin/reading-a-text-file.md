---
title:    "Kotlin: Lecture d'un fichier texte"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Pourquoi
Lire des fichiers texte est une tâche courante dans la programmation et est utile pour stocker et récupérer des données. En apprenant à lire des fichiers texte en Kotlin, vous pourrez améliorer vos compétences en programmation et gagner du temps lors de la manipulation de données.

## Comment
Pour lire un fichier texte en Kotlin, vous pouvez utiliser la fonction `readText()` avec comme paramètre le nom du fichier à lire. Par exemple, si vous avez un fichier texte nommé "monfichier.txt", le code suivant vous permettra de lire le fichier et d'afficher son contenu :

```Kotlin
val contenu = readText("monfichier.txt")
println(contenu)
```

Si vous voulez lire uniquement certaines lignes du fichier, vous pouvez utiliser la fonction `readLines()` en spécifiant le numéro des lignes à lire. Par exemple, pour afficher seulement la première et la troisième ligne d'un fichier, vous pouvez utiliser le code suivant :

```Kotlin
val lignes = readLines("monfichier.txt")

println(lignes[0]) //affiche la première ligne
println(lignes[2]) //affiche la troisième ligne
```

## Plongée Profonde
En plus des fonctions `readText()` et `readLines()`, il existe d'autres façons de lire un fichier texte en Kotlin, comme en utilisant la fonction `forEachLine()` qui vous permet de lire le fichier ligne par ligne et d'effectuer une action sur chaque ligne.

De plus, il est important de noter que lors de la lecture d'un fichier texte, vous devez faire attention aux exceptions qui peuvent se produire. Par exemple, le fichier peut être introuvable ou il peut y avoir un problème de permission d'accès au fichier.

## Voir Aussi
- [Documentation officielle Kotlin sur les entrées/sorties de fichiers](https://kotlinlang.org/docs/tutorials/kotlin-for-py/input-output.html)
- [Tutoriel Kotlin : lecture et écriture de fichiers](https://www.tutorialkart.com/kotlin/kotlin-read-write-files-using-filewriter-bufferedwriter/)
- [Exemples de lecture de fichiers en Kotlin](https://www.programiz.com/kotlin-programming/examples/read-file)