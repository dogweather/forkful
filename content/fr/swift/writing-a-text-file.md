---
title:                "Swift: Écrire un fichier texte"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Swift, vous connaissez probablement déjà l'importance de savoir comment écrire un fichier texte en programmation. Que ce soit pour stocker des données, générer des rapports ou simplement pour des besoins de débogage, il est essentiel de savoir comment écrire dans un fichier texte. Dans cet article, nous allons explorer les différentes façons de le faire en Swift et comment cela peut être bénéfique dans vos projets.

## Comment faire

Il existe plusieurs façons d'écrire dans un fichier texte en programmation Swift, nous allons en couvrir trois principales dans cet article.

**1. Utiliser la méthode de base**

La première méthode consiste à utiliser la méthode de base de la classe `FileManager`. Cette méthode est utilisée pour créer un fichier et écrire dedans. Voici un exemple de code pour écrire "Bonjour tout le monde" dans un fichier texte nommé `message.txt` :

```Swift
let message = "Bonjour tout le monde"

if let docDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first {
    let fileURL = docDir.appendingPathComponent("message.txt")
    do {
        try message.write(to: fileURL, atomically: true, encoding: .utf8)
    } catch {
        print("Erreur lors de l'écriture dans le fichier : \(error)")
    }
}
```

Vous pouvez ensuite vérifier que le fichier a bien été créé et que le message a bien été écrit en utilisant `print` pour lire son contenu.

**2. Utiliser `NSFileHandle`**

Une autre façon d'écrire dans un fichier texte en Swift est d'utiliser la classe `NSFileHandle`. Avec cette méthode, vous devez d'abord ouvrir le fichier, puis écrire dedans. Voici un exemple de code pour écrire "Bonjour tout le monde" dans un fichier texte nommé `message.txt` :

```Swift
let message = "Bonjour tout le monde"

if let docDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first {
    let fileURL = docDir.appendingPathComponent("message.txt")
    do {
        let fileHandle = try FileHandle(forWritingTo: fileURL)
        fileHandle.seekToEndOfFile()
        fileHandle.write(message.data(using: .utf8)!)
        fileHandle.closeFile()
        print("Ecriture dans le fichier réussie !")
    } catch {
        print("Erreur lors de l'écriture dans le fichier : \(error)")
    }
}
```

**3. Utiliser `write(to:atomically:encoding:)`**

Enfin, la troisième façon d'écrire dans un fichier texte en Swift est d'utiliser la méthode `write(to:atomically:encoding:)` disponible pour les chaînes de caractères. Cette méthode est similaire à la méthode de base, mais elle simplifie le code en combinant les étapes de création du fichier et d'écriture. Voici un exemple de code pour écrire "Bonjour tout le monde" dans un fichier texte nommé `message.txt` :

```Swift
let message = "Bonjour tout le monde"

if let docDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first {
    let fileURL = docDir.appendingPathComponent("message.txt")
    do {
        try message.write(to: fileURL, atomically: true, encoding: .utf8)
    } catch {
        print("Erreur lors de l'écriture dans le fichier : \(error)")
    }
}
```

Il est important de noter que dans chacune de ces méthodes, le contenu du fichier précédent sera remplacé si le fichier existe déjà. Pour ajouter du contenu à un fichier existant, vous devrez utiliser la méthode `append(path:)` de la classe `NSFileHandle`.

## Deep Dive

Maintenant que nous avons vu comment écrire dans un fichier texte en Swift, il est important de mentionner que cette technique peut également être utile pour d'autres tâches telles que la lecture d'un fichier texte ou la suppression d'un fichier. Il est également important de noter que les méthodes de base que nous avons explorées sont également utilisées pour écrire dans d'autres types de fichiers, tels que les fichiers CSV ou JSON.

## Voir aussi

- [Utilisation de FileManager pour écrire des données dans des fichiers texte](https://developer.apple.com/documentation/uikit/view_controllers/adding_a_second_scene)
- [Documentation sur la classe `FileManager`](https://developer.apple.com/documentation/foundation/filemanager)
- [Documentation sur