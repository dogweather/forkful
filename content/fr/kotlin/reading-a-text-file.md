---
title:    "Kotlin: Lecture d'un fichier texte"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi lire un fichier texte

Lire un fichier texte est une tâche commune dans la programmation. Cela peut être utile pour stocker des données, lire des configurations ou lire des fichiers de log. Dans cet article, nous allons apprendre comment lire un fichier texte en utilisant Kotlin.

## Comment le faire

Pour lire un fichier texte en Kotlin, nous allons utiliser la bibliothèque standard `java.io.File`. Tout d'abord, nous devons créer une instance du fichier que nous voulons lire :

```
val file = File("mon_fichier.txt")
```

Ensuite, nous pouvons utiliser la fonction `readText()` pour lire le contenu du fichier en tant que chaîne de caractères :

```
val contenu = file.readText()
```

Nous pouvons également spécifier un encodage en utilisant un paramètre facultatif :

```
val contenu = file.readText(Charsets.UTF_8)
```

Et voilà ! Nous avons maintenant le contenu de notre fichier texte stocké dans la variable `contenu`.

## Plongée en profondeur

La fonction `readText()` que nous avons utilisée fait en fait appel à la fonction `readBytes()` de la classe `File` pour lire le contenu en tant qu'octets. Ensuite, cette fonction utilise le décodeur par défaut du système pour convertir ces octets en une chaîne de caractères. Cela peut être spécifié avec le paramètre facultatif que nous avons vu précédemment.

Il est également possible de lire le fichier ligne par ligne en utilisant la fonction `forEachLine()` :

```
file.forEachLine { line ->
    // faire quelque chose avec chaque ligne
}
```

Il est important de noter que la classe `File` peut lever des exceptions lors de la lecture d'un fichier. Il est donc recommandé d'utiliser une structure de try-catch lors de la lecture d'un fichier.

## Voir aussi

- Documentation officielle de la bibliothèque standard Kotlin : https://kotlinlang.org/docs/reference/stdlib.html#file-handling
- Tutoriel vidéo sur la lecture de fichiers en Kotlin : https://www.youtube.com/watch?v=Fb7AWWIzYIo