---
title:                "Swift: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Créer des fichiers temporaires peut être très utile lors de la programmation en Swift. Les fichiers temporaires sont utilisés pour stocker temporairement des données qui ne sont pas nécessaires pendant une longue période. Cela peut aider à optimiser les performances de votre programme et à libérer de l'espace lorsqu'il n'est plus nécessaire.

## Comment faire 

Pour créer un fichier temporaire en Swift, vous pouvez utiliser la méthode `NSTemporaryDirectory()` et `URLByAppendingPathComponent()` pour ajouter un nom unique à votre fichier. Voici un exemple de code pour créer un fichier temporaire nommé "myTempFile.txt" :

```Swift
let tempDirectory = NSTemporaryDirectory()
let tempFilePath = tempDirectory.URLByAppendingPathComponent("myTempFile.txt")

if !NSFileManager.defaultManager().createFileAtPath(tempFilePath.path!, contents: nil, attributes: nil) {
    fatalError("Impossible de créer le fichier temporaire.")
} else {
    print("Fichier temporaire créé avec succès à l'emplacement : \(tempFilePath)")
}

```

La méthode `NSTemporaryDirectory()` renvoie l'emplacement du répertoire temporaire de votre système d'exploitation. Ensuite, la méthode `URLByAppendingPathComponent()` ajoute le nom du fichier au répertoire temporaire. Enfin, la méthode `createFileAtPath()` crée le fichier à cet emplacement et renvoie un booléen pour confirmer si la création a été réussie ou non. 

## Deep Dive

Il est important de noter que la création d'un fichier temporaire n'est pas suffisante pour optimiser les performances de votre programme. Vous devez également penser à supprimer ces fichiers une fois qu'ils ne sont plus nécessaires. 

Pour supprimer un fichier temporaire, vous pouvez utiliser la méthode `removeItemAtPath()` de la classe `NSFileManager`. Voici un exemple de code pour supprimer le fichier temporaire créé précédemment : 

```Swift
do {
    try NSFileManager.defaultManager().removeItemAtPath(tempFilePath.path!)
    print("Fichier temporaire supprimé avec succès.")
} catch let error as NSError {
    print("Erreur lors de la suppression du fichier temporaire : \(error)")
}
```

Il est également important de noter que les fichiers temporaires sont automatiquement supprimés par le système d'exploitation lors du redémarrage de l'ordinateur, donc vous n'avez pas à vous soucier de les supprimer manuellement dans ce cas.

## Voir aussi

- [Apple Developer Documentation - Creating and Deleting Files](https://developer.apple.com/documentation/foundation/file_management/creating_and_deleting_files)
- [NSHipster - Temporary Files in Swift](https://nshipster.com/temporary-files-in-swift/)
- [Hacking with Swift - How to create a temporary directory on disk using URL](https://www.hackingwithswift.com/example-code/system/how-to-create-a-temporary-directory-on-disk-using-url)