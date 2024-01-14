---
title:                "Kotlin: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi écrire un fichier texte?

L'écriture de fichiers texte est un élément fondamental de la programmation en Kotlin. Cela permet aux développeurs de créer et de stocker des données qui peuvent être utilisées par d'autres parties du code ou même par d'autres programmes.

## Comment faire?

L'écriture d'un fichier texte en Kotlin est assez simple. Tout d'abord, vous devez ouvrir un flux de fichier en utilisant la classe `FileOutputStream` et en spécifiant le chemin et le nom du fichier que vous souhaitez créer :

```Kotlin
val fileOutputStream = FileOutputStream("chemin/vers/votre/fichier.txt")
```

Ensuite, vous pouvez écrire du contenu dans le fichier en utilisant la méthode `write()` du flux de fichier :

```Kotlin
fileOutputStream.write("Ceci est un exemple de texte qui sera écrit dans le fichier.".toByteArray())
```

Enfin, n'oubliez pas de fermer le flux de fichier une fois que vous avez terminé d'écrire dans le fichier :

```Kotlin
fileOutputStream.close()
```

## Plongée en profondeur

Il est important de noter que lors de l'écriture d'un fichier texte en Kotlin, vous devez spécifier le type de données en utilisant la méthode `toByteArray()` pour convertir une chaîne en un tableau d'octets. De plus, vous pouvez également spécifier un encodeur pour votre fichier en utilisant le deuxième paramètre de la méthode `write()`. Par exemple, si vous voulez utiliser l'encodeur UTF-8, vous pouvez utiliser `fileOutputStream.write("Votre texte".toByteArray(), Charset.forName("UTF-8"))`.

Les fichiers texte sont également utiles pour stocker des données structurées, telles que du JSON ou du CSV. Vous pouvez utiliser des bibliothèques telles que Moshi ou OpenCSV pour faciliter l'écriture de ces types de données dans des fichiers texte.

## Voir aussi

- [Documentation officielle de Kotlin sur l'écriture de fichiers](https://kotlinlang.org/docs/tutorials/kotlin-for-py/create-files.html)
- [Exemple de code pour écrire un fichier texte en Kotlin](https://www.geeksforgeeks.org/kotlin-file-outputstream-writebytes-method-with-example/)
- [Tutoriel sur l'utilisation de Moshi pour écrire des fichiers JSON en Kotlin](https://medium.com/@t1kw0ndo/reading-and-writing-json-with-kotlin-using-moshi-and-gson-963c54bc15fc)