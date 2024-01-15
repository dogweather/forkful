---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Swift: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Swift, vous savez probablement que la gestion des fichiers et des dossiers est une partie importante de nombreuses applications. Il est donc essentiel de savoir si un dossier existe avant de tenter de l'utiliser, afin d'éviter toute erreur ou problème potentiel.

## Comment faire

Pour vérifier si un dossier existe en Swift, vous pouvez utiliser la méthode `fileExists(atPath:)` de la classe `FileManager`, qui renvoie un booléen indiquant si le chemin d'accès spécifié correspond à un fichier ou un dossier existant.

```Swift
let fileManager = FileManager.default
let path = "/Users/Utilisateur/Documents/Projet/Photos"
if fileManager.fileExists(atPath: path) {
    print("Le dossier existe.")
} else {
    print("Le dossier n'existe pas.")
}
```

Dans cet exemple, nous utilisons `default`, qui renvoie l'instance du gestionnaire de fichiers pour le répertoire de l'application. Vous pouvez également créer une instance personnalisée pour un autre répertoire si nécessaire.

## Plongée en profondeur

Il est important de noter que la méthode `fileExists(atPath:)` vérifie à la fois les fichiers et les dossiers. Elle ne fait pas la distinction entre les deux, donc si le chemin d'accès spécifié correspond à un fichier plutôt qu'à un dossier, elle renverra également `true`.

De plus, il est important de considérer les variations de majuscules et de minuscules dans les noms de dossiers lors de la vérification de leur existence. Par exemple, sur un système de fichiers macOS, le dossier "Photos" sera considéré comme différent du dossier "photos".

## Voir aussi

- [Documentation officielle sur la classe FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutoriel YouTube sur la gestion des fichiers en Swift](https://www.youtube.com/watch?v=1g5YB5taPcY)
- [Article sur les bonnes pratiques en matière de gestion de fichiers en Swift](https://www.hackingwithswift.com/articles/108/the-right-way-to-read-files-in-swift)