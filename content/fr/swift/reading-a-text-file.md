---
title:                "Swift: Lecture d'un fichier texte"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes intéressé par la programmation Swift, vous avez probablement entendu parler de la manipulation de fichiers texte. Mais pourquoi est-il important de savoir comment lire un fichier texte? Eh bien, la réponse est simple : la lecture de fichiers texte est une compétence fondamentale pour tout programmeur, car elle vous permet d'interagir avec des données stockées dans un format facilement lisible.

## Comment faire

Pour lire un fichier texte en Swift, nous allons utiliser la méthode `String(contentsOf:)`. Prenons un exemple concret en créant un fichier texte nommé "hello.txt" avec le contenu suivant :

```
Bonjour le monde!
```

Maintenant, dans notre code Swift, nous pouvons utiliser cette méthode pour lire le contenu du fichier et l'afficher dans la console :

```
if let contenu = try? String(contentsOf: URL(fileURLWithPath: "hello.txt")) {
    print(contenu)
}
```

Dans cet exemple, nous utilisons le mot-clé `if let` pour vérifier s'il existe une valeur pour `contenu` avant de l'utiliser. La méthode `String(contentsOf:)` peut lancer une erreur si elle ne trouve pas le fichier spécifié, donc nous devons être prudents.

Maintenant, si nous exécutons ce code, nous devrions voir "Bonjour le monde!" imprimé dans la console.

## Deep Dive

En utilisant la méthode `String(contentsOf:)`, Swift va automatiquement essayer de déterminer l'encodage du fichier pour le lire correctement. Cependant, il est également possible de spécifier explicitement l'encodage souhaité en utilisant un paramètre optionnel dans la méthode.

De plus, il est important de noter que cette méthode peut être utilisée pour lire d'autres types de fichiers en plus de fichiers texte, tels que des fichiers HTML ou JSON.

## Voir aussi

Pour en apprendre davantage sur la lecture de fichiers texte en Swift, voici quelques ressources utiles en français :

- [Documentation officielle de Swift sur la manipulation de fichiers](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html)
- [Tutoriel vidéo sur la manipulation de fichiers avec Swift](https://www.youtube.com/watch?v=guBhCFat26w)
- [Article de blog sur la lecture de CSV avec Swift](https://www.ios-blog.com/tutorials/swift/how-to-read-csv-file-with-swift)