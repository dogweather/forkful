---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:01.384095-07:00
description: "Comment faire : En Swift, l'\xE9criture dans l'erreur standard peut\
  \ se faire \xE0 l'aide de la classe `FileHandle` pour un acc\xE8s direct \xE0 stderr.\
  \ Voici un\u2026"
lastmod: '2024-03-13T22:44:58.244154-06:00'
model: gpt-4-0125-preview
summary: "En Swift, l'\xE9criture dans l'erreur standard peut se faire \xE0 l'aide\
  \ de la classe `FileHandle` pour un acc\xE8s direct \xE0 stderr."
title: "\xC9crire sur l'erreur standard"
weight: 25
---

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
