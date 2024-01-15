---
title:                "Création d'un fichier temporaire"
html_title:           "Swift: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer un fichier temporaire en programmation peut être utile dans de nombreuses situations. Par exemple, cela peut être nécessaire pour stocker des données temporaires pendant le fonctionnement d'une application ou pour faciliter la gestion de fichiers temporaires lors de l'écriture de scripts.

## Comment faire

Pour créer un fichier temporaire en Swift, vous pouvez utiliser la classe `FileManager` et sa méthode `createTempFile`. Voici un exemple de code qui crée un fichier temporaire avec un contenu prédéfini :

```Swift
if let fileManager = FileManager.default {
    do {
        let tempFile = try fileManager.createTempFile(prefix: "temp", suffix: ".txt")
        let contents = "Ceci est un fichier temporaire."
        try contents.write(to: tempFile)
        print(tempFile.path)
    } catch {
        print(error)
    }
}
```

Ce code utilise le préfixe "temp" et le suffixe ".txt" pour générer le nom du fichier temporaire, mais vous pouvez les modifier selon vos besoins. Le fichier sera créé dans le répertoire temporaire par défaut du système d'exploitation.

Lorsque vous exécutez ce code, vous devriez voir le chemin du fichier temporaire s'afficher dans la console. Ce fichier sera supprimé automatiquement lorsque l'application se termine ou lorsque vous appelez la méthode `removeTemporaryItem` de la classe `FileManager`.

## Plongée en profondeur

En utilisant la méthode `createTempFile` avec les paramètres par défaut, le fichier temporaire sera créé avec des autorisations limitées pour garantir sa suppression automatique. Vous pouvez également spécifier des arguments facultatifs pour personnaliser les attributs du fichier temporaire, tels que les autorisations, le propriétaire, le groupe et la date de modification.

De plus, vous pouvez également utiliser la méthode `createTempFile` pour créer un fichier temporaire dans un répertoire spécifique. Cela peut être utile si vous souhaitez que le fichier temporaire soit stocké dans un emplacement précis.

## Voir aussi

Pour en savoir plus sur la classe `FileManager` et comment gérer les fichiers temporaires en Swift, voici quelques liens supplémentaires :

- [Documentation officielle de FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Guide de référence à la gestion de fichiers en Swift](https://www.raywenderlich.com/4978-swift-tutorial-how-to-use-arrays-in-swift)
- [Article sur la création de fichiers temporaires en Swift](https://www.andyibanez.com/posts/creating-temporary-files-with-url-in-swift/)

Maintenant que vous connaissez les bases de la création de fichiers temporaires en Swift, vous pouvez les utiliser dans vos projets pour simplifier la gestion de fichiers temporaires et améliorer les performances de votre application.