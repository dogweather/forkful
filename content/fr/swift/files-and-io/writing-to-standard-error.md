---
title:                "Écrire sur l'erreur standard"
aliases:
- fr/swift/writing-to-standard-error.md
date:                  2024-02-03T19:35:01.384095-07:00
model:                 gpt-4-0125-preview
simple_title:         "Écrire sur l'erreur standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire dans l'erreur standard (stderr) consiste à diriger les messages d'erreur de votre programme ou les sorties de diagnostic vers un flux séparé, distinct de la sortie standard (stdout). Cela est essentiel pour le débogage et l'enregistrement des erreurs sans encombrer la sortie standard, facilitant ainsi la compréhension de l'état et des problèmes du programme tant pour les développeurs que pour les utilisateurs.

## Comment faire :

En Swift, l'écriture dans l'erreur standard peut se faire à l'aide de la classe `FileHandle` pour un accès direct à stderr. Voici un exemple simple :

```swift
import Foundation

// Définir un message
let errorMessage = "Une erreur s'est produite.\n"

// Convertir le message en données
if let data = errorMessage.data(using: .utf8) {
    // Écrire le message d'erreur dans stderr
    FileHandle.standardError.write(data)
}
```

Sortie vers stderr (habituellement vue dans une console ou un terminal) :
```
Une erreur s'est produite.
```

Pour une journalisation plus complexe ou lors du travail avec des bibliothèques externes, on pourrait envisager d'utiliser une bibliothèque tierce comme **SwiftLog**. Bien que **SwiftLog** n'écrive pas directement dans stderr dès le départ, vous pouvez implémenter un backend de journalisation personnalisé pour y parvenir. Voici un exemple simplifié de la définition d'un gestionnaire de journalisation personnalisé qui écrit dans stderr :

D'abord, ajoutez **SwiftLog** à vos dépendances de projet dans `Package.swift` :
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    nom: "VotreNomDePaquet",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            nom: "VotreNomDeCible",
            dependencies: [
                .product(nom: "Logging", paquet: "swift-log"),
            ]),
    ]
)
```

Ensuite, implémentez un gestionnaire de journalisation personnalisé qui écrit dans stderr :

```swift
import Logging
import Foundation

struct StderrLogHandler: LogHandler {
    let label: String
    
    var logLevel: Logger.Level = .info
    
    func log(level: Logger.Level, message: Logger.Message, metadata: Logger.Metadata?, source: String, fichier: String, fonction: String, ligne: UInt) {
        let output = "\(message)\n"
        if let data = output.data(using: .utf8) {
            FileHandle.standardError.write(data)
        }
    }
    
    subscript(metadataKey metadataKey: String) -> Logger.Metadata.Value? {
        get { return nil }
        set(newValue) { }
    }
    
    var metadata: Logger.Metadata {
        get { return [:] }
        set(newMetadata) { }
    }
}

// Utilisation
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.exemple.votreappli")

logger.error("Ceci est un message d'erreur")
```

Sortie vers stderr :
```
Ceci est un message d'erreur
```

Ce gestionnaire personnalisé vous permet de diriger vos messages d'erreur SwiftLog directement vers l'erreur standard, s'intégrant de manière transparente avec d'autres messages de journalisation que votre application pourrait générer.
