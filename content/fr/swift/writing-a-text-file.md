---
title:                "Écriture d'un fichier texte"
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire un fichier texte permet de sauvegarder des données sous forme lisible. Les devs utilisent cette technique pour la persistance de données, les logs, ou l'échange d'informations.

## How to:
Écrire dans un fichier en Swift est direct. Voici comment :

```Swift
import Foundation

let stringToWrite = "Bonjour, ceci est un texte enregistré!"
let filePath = "/Users/yourUsername/Documents/swift_textfile.txt"

do {
    try stringToWrite.write(to: URL(fileURLWithPath: filePath), atomically: true, encoding: .utf8)
    print("Fichier écrit avec succès")
} catch {
    print("Une erreur s'est produite: \(error)")
}
```

Et voilà, `swift_textfile.txt` contient maintenant notre message.

## Deep Dive
Avant Swift, Objective-C était la norme pour l'écriture de fichiers sur iOS. Swift simplifie le processus avec `String.write()`. Les alternatives incluent `FileManager` pour plus de contrôle, ou `OutputStream` pour les gros fichiers. Il faut gérer les erreurs car le disque peut être plein ou le chemin inaccessible.

## See Also
Pour explorer davantage, consultez:
- [Apple's Swift Documentation on Data and Files](https://developer.apple.com/documentation/foundation/file_system)
- [NSHipster's article on File Management](https://nshipster.com/filemanager/)
- [Ray Wenderlich's Swift tutorial for working with files](https://www.raywenderlich.com/666-filemanager-class-tutorial-for-macos-getting-started-with-the-file-system)
