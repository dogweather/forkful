---
date: 2024-01-20 17:41:33.600849-07:00
description: "Cr\xE9er un fichier temporaire, c'est juste \xE9crire des donn\xE9es\
  \ qui n'ont pas besoin de rester longtemps. Les programmeurs le font pour stocker\
  \ des trucs de\u2026"
lastmod: '2024-03-13T22:44:58.248373-06:00'
model: gpt-4-1106-preview
summary: "Cr\xE9er un fichier temporaire, c'est juste \xE9crire des donn\xE9es qui\
  \ n'ont pas besoin de rester longtemps."
title: "Cr\xE9ation d'un fichier temporaire"
weight: 21
---

## How to:
Swift rend la création de fichiers temporaires assez easy. Voilà comment ça se passe :

```Swift
import Foundation

let tempDirectoryURL = FileManager.default.temporaryDirectory
let tempFileURL = tempDirectoryURL.appendingPathComponent("monFichierTemp.txt")
let tempContent = "Salut, je suis un texte temporaire!"

do {
    try tempContent.write(to: tempFileURL, atomically: true, encoding: .utf8)
    print("Fichier temporaire créé à l'adresse \(tempFileURL)")
} catch {
    print("Erreur de création du fichier temporaire: \(error.localizedDescription)")
}

// Lire le fichier temporaire
do {
    let contents = try String(contentsOf: tempFileURL, encoding: .utf8)
    print("Contenu du fichier: \(contents)")
} catch {
    print("Erreur de lecture du fichier temporaire: \(error.localizedDescription)")
}
```
Sortie d'exemple :
```
Fichier temporaire créé à l'adresse file:///.../monFichierTemp.txt
Contenu du fichier: Salut, je suis un texte temporaire!
```

## Deep Dive
Avant, on utilisait plus des dossiers temporaires en C, mais avec Swift, c’est plus fluide. En plus, y'a `NSTemporaryDirectory()` qui marchait avant, mais maintenant `FileManager.default.temporaryDirectory` est le top parce qu'il te donne direct un `URL`. C'est plus sécuritaire vis-à-vis des conflits de fichiers.

Alternativement, tu peux utiliser `mkstemp` de POSIX si tu as besoin de compatibilité avec des systèmes UNIX en bas niveau. Mais, franchement, c’est plus complexe. 

En implementation, Swift s'assure que ton fichier est bien dans le dossier temp et qu’il va disparaître quand t'en as plus besoin. À toi de jouer pour gérer la suppression manuelle si c'est nécessaire.

## See Also
Pour enfoncer le clou, t'as quelques liens qui vont t'aider à digérer tout ça :

- Documentation Apple sur FileManager: https://developer.apple.com/documentation/foundation/filemanager
- Guide pratique sur les URL en Swift: https://swift.org/documentation/api-design-guidelines/#promote-clear-usage
- Stack Overflow pour les questions communes sur les fichiers temporaires: https://stackoverflow.com/questions/tagged/swift+temporary-files
