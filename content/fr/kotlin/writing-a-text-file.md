---
title:    "Kotlin: Ecrire un fichier texte"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi écrire un fichier texte en Kotlin ?

Écrire un fichier texte est une tâche courante dans la programmation. Que ce soit pour stocker des données, générer des rapports ou créer des fichiers de configuration, les fichiers texte sont souvent utilisés pour communiquer avec l'ordinateur. Alors pourquoi ne pas apprendre comment le faire en Kotlin ?

## Comment faire pour écrire un fichier texte en Kotlin ?

Écrire un fichier texte en Kotlin est assez simple. Tout d'abord, il est nécessaire d'ouvrir un fichier en utilisant la classe `File` et en passant le chemin du fichier en paramètre. Ensuite, il suffit d'utiliser la fonction `writeText()` pour écrire le contenu du fichier. Voici un exemple de code :

```Kotlin
// Ouvre le fichier "example.txt" en écriture
val file = File("example.txt")

// Écrit le contenu du fichier
file.writeText("Bonjour le monde !")
```

Lorsque vous exécutez ce code, vous devriez voir un nouveau fichier texte nommé "example.txt" contenant le texte "Bonjour le monde !". Il est également possible de spécifier un autre encodage pour le fichier en utilisant l'argument facultatif `charset` de la fonction `writeText()`. Par défaut, l'encodage utilisé est UTF-8.

## Plongez plus profondément dans l'écriture de fichiers texte en Kotlin

En plus de la fonction `writeText()`, il existe d'autres façons d'écrire dans un fichier texte en Kotlin. Par exemple, vous pouvez utiliser la classe `BufferedWriter` pour écrire de grandes quantités de données dans un fichier. Cette classe offre des méthodes telles que `write()` et `newLine()` pour écrire le contenu de manière plus flexible.

Il est également important de noter que lors de l'écriture d'un fichier, il est recommandé d'utiliser la structure `try-catch-finally` pour gérer les exceptions éventuelles. Voici un exemple de code utilisant la classe `BufferedWriter` et la structure `try-catch-finally` :

```Kotlin
val file = File("example.txt")

try {
    val writer = BufferedWriter(FileWriter(file))

    // Écrit le contenu du fichier
    writer.write("Bonjour le monde !")
    writer.newLine()
} catch (e: IOException) {
    // Traite l'exception ici
} finally {
    // Ferme le fichier
    writer.close()
}
```

## Voir aussi

Si vous souhaitez en savoir plus sur l'écriture de fichiers texte en Kotlin, voici quelques ressources utiles :

- [Documentation officielle de la classe File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Documentation officielle de la classe BufferedWriter](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-writer/index.html)
- [Tutoriel vidéo: Comment écrire un fichier en Kotlin](https://www.youtube.com/watch?v=sgy9OS6-Nn8)