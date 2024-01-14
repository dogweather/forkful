---
title:                "Kotlin: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur débutant ou même expérimenté, vous vous êtes peut-être demandé comment lire le contenu d'un fichier texte en utilisant Kotlin. Cela peut sembler être une tâche simple, mais il y a plusieurs façons de le faire en utilisant différentes méthodes et bibliothèques. Dans cet article, nous allons examiner différentes façons de lire un fichier texte en utilisant Kotlin, en espérant que cela vous aidera dans vos projets futurs.

## Comment faire

Pour lire un fichier texte en utilisant Kotlin, nous allons utiliser la classe [java.io.File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html). Voici un exemple de code pour lire et afficher le contenu d'un fichier texte :

```Kotlin
val file = File("mon_fichier.txt")
file.forEachLine { line ->
    println(line)
}
```

En utilisant la méthode `forEachLine`, nous itérons sur chaque ligne du fichier et affichons son contenu. Si vous souhaitez stocker le contenu du fichier dans une variable, vous pouvez utiliser la méthode `readText()` :

```Kotlin
val file = File("mon_fichier.txt")
val content = file.readText()
println(content)
```

En outre, nous pouvons utiliser la bibliothèque [Apache Commons IO](https://commons.apache.org/proper/commons-io/) pour lire un fichier texte en utilisant Kotlin. Pour ce faire, nous devons d'abord ajouter la dépendance dans notre fichier `build.gradle` :

```
implementation 'commons-io:commons-io:2.6'
```

Ensuite, voici un exemple de code utilisant cette bibliothèque :

```Kotlin
val file = File("mon_fichier.txt")
val content = FileUtils.readFileToString(file, StandardCharsets.UTF_8)
println(content)
```

## Plongez plus en profondeur

Si vous souhaitez avoir plus de contrôle sur la lecture d'un fichier texte, vous pouvez utiliser les classes `FileReader` et `BufferedReader`. Cette approche est plus utile pour traiter de gros fichiers ou pour effectuer des opérations plus avancées telles que la recherche de mots spécifiques.

```Kotlin
val file = FileReader("mon_fichier.txt")
val reader = BufferedReader(file)
var line: String? = reader.readLine()
while (line != null) {
    println(line)
    line = reader.readLine()
}
```

En utilisant `BufferedReader`, nous lisons le fichier ligne par ligne jusqu'à ce que nous atteignons la fin du fichier. Vous pouvez également utiliser la méthode `readLines()` pour stocker chaque ligne du fichier dans une liste.

## Voir aussi

- [Documentation Kotlin sur la classe File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Apache Commons IO](https://commons.apache.org/proper/commons-io/)
- [Documentation sur la classe FileReader en Java](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)
- [Documentation sur la classe BufferedReader en Java](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)