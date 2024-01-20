---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

La création d'un fichier temporaire consiste à créer un fichier pour usage à court terme, habituellement pour stocker des données de manière transitoire pendant le déroulement d'un processus. Les programmeurs le font pour éviter d'encombrer la mémoire principale ou pour conserver des données à travers plusieurs exécutions de programme.

## Comment faire :

Le morceau de code suivant illustre comment créer un fichier temporaire en Swift.

```Swift
import Foundation

let tempDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
let targetURL = tempDirectoryURL.appendingPathComponent(UUID().uuidString)

do {
    try "Hello, World!".write(to: targetURL, atomically: true, encoding: .utf8)
    print("Fichier temporaire créé à l'endroit suivant: \(targetURL)")
} catch {
    print("Erreur lors de la création du fichier temporaire: \(error)")
}
```

**Résultat :**

Fichier temporaire créé à l'endroit suivant: file:///var/folders/random/3B6F9362-7FA1-479F-B76D-183931345332

## Plongée en profondeur :

Historiquement, la création de fichiers temporaires était une partie intégrante de la programmation en C standard où le programmeur devait gérer manuellement le processus. Swift permet une gestion plus facile grâce à la bibliothèque Foundation.

Un autre moyen de gérer les données temporaires consiste à utiliser une base de données en mémoire, telle que `SQLite`. Cette méthode a l'avantage d'être rapide et de permettre des interactions plus complexes avec les données.

En termes de mise en œuvre dans Swift, lorsque vous écrivez des données dans un fichier à l'aide de la méthode `write(to:atomically:encoding:)`, Swift utilise les techniques de la bibliothèque `Foundation` pour écrire les fichiers, offrant une gestion robuste des erreurs et des fonctionnalités supplémentaires, comme le support de l'encodage des caractères.

## Voir également :

- Documentation officielle de Swift : https://swift.org/documentation/
- Creer, lire, et écrire des fichiers avec Swift : https://www.hackingwithswift.com/read/0/16/reading-and-writing-basics
- Guide NSTemporaryDirectory : https://developer.apple.com/documentation/foundation/1409211-nstemporarydirectory