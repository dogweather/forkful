---
title:                "Swift: Création d'un fichier temporaire"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Pourquoi
Le création de fichiers temporaires est utile lors de la manipulation de données temporaires ou lors de la mise en œuvre de fonctions qui nécessitent un fichier temporaire comme paramètre.

# Comment faire
Pour créer un fichier temporaire en Swift, nous utilisons la classe `FileManager` et la méthode `temporaryDirectory` pour obtenir un répertoire temporaire. Ensuite, nous pouvons utiliser la méthode `createFile(atPath:contents:attributes:)` pour créer notre fichier avec le contenu et les attributs souhaités.

```Swift
let fileManager = FileManager.default
let tempDir = fileManager.temporaryDirectory
let tempFileURL = tempDir.appendingPathComponent("tempFile.txt")

// Créer un contenu pour notre fichier
let fileContent = "Ceci est un fichier temporaire."

// Créer le fichier avec le contenu et les attributs souhaités
fileManager.createFile(atPath: tempFileURL.path, contents: fileContent.data(using: .utf8), attributes: nil)

// Vérifier la création du fichier
if fileManager.fileExists(atPath: tempFileURL.path) {
    print("Le fichier temporaire a été créé avec succès !")
} else {
    print("Une erreur s'est produite lors de la création du fichier temporaire.")
}
```

Lorsque nous n'avons plus besoin de notre fichier temporaire, nous pouvons l'effacer en utilisant la méthode `removeItem(at:)` de la classe `FileManager`.

```Swift
// Effacer le fichier temporaire
do {
    try fileManager.removeItem(at: tempFileURL)
    print("Le fichier temporaire a été supprimé avec succès.")
} catch {
    print("Une erreur s'est produite lors de la suppression du fichier temporaire : \(error).")
}
```

# Plongée en profondeur
Les fichiers temporaires sont généralement utilisés dans des cas spécifiques tels que la création de sauvegardes temporaires, la création de fichiers pour les téléchargements ou les mises à jour, ou pour stocker des données récupérées pendant le traitement. Il est important de noter que ces fichiers temporaires sont souvent supprimés automatiquement par le système, donc il est important de ne pas compter sur leur existence à long terme.

# Voir aussi
- [Documentation sur la classe `FileManager`](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutoriel sur la création de fichiers en Swift](https://www.raywenderlich.com/702101-the-swift-cheat-sheet-for-creating-files-in-ios)