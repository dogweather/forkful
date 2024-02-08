---
title:                "Schreiben auf Standardfehler"
aliases:
- de/swift/writing-to-standard-error.md
date:                  2024-02-03T19:34:35.693703-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schreiben auf Standardfehler"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben in den Standardfehler (stderr) geht darum, Fehlermeldungen oder Diagnoseausgaben Ihres Programms auf einen separaten Stream zu leiten, getrennt von der Standardausgabe (stdout). Dies ist entscheidend für das Debugging und das Protokollieren von Fehlern, ohne die Standardausgabe zu überladen, und erleichtert sowohl Entwicklern als auch Benutzern das Verständnis des Zustands und der Probleme des Programms.

## Wie geht das:

In Swift kann man durch die Verwendung der Klasse `FileHandle` direkt auf stderr zugreifen, um dorthin zu schreiben. Hier ist ein einfaches Beispiel:

```swift
import Foundation

// Definiere eine Nachricht
let errorMessage = "Ein Fehler ist aufgetreten.\n"

// Konvertiere die Nachricht in Daten
if let data = errorMessage.data(using: .utf8) {
    // Schreibe die Fehlermeldung in stderr
    FileHandle.standardError.write(data)
}
```

Ausgabe auf stderr (typischerweise in einer Konsole oder einem Terminal zu sehen):
```
Ein Fehler ist aufgetreten.
```

Für komplexeres Logging oder bei der Arbeit mit externen Bibliotheken könnte man erwägen, eine Drittanbieterbibliothek wie **SwiftLog** zu verwenden. Obwohl **SwiftLog** nicht direkt out of the box auf stderr schreibt, können Sie eine benutzerdefinierte Log-Backend implementieren, um dies zu erreichen. Hier ist ein vereinfachtes Beispiel für die Definition eines benutzerdefinierten Log-Handlers, der auf stderr schreibt:

Fügen Sie zunächst **SwiftLog** zu Ihren Projekt-Abhängigkeiten in `Package.swift` hinzu:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "IhrPaketName",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "IhrZielName",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

Implementieren Sie dann einen benutzerdefinierten Log-Handler, der auf stderr schreibt:

```swift
import Logging
import Foundation

struct StderrLogHandler: LogHandler {
    let label: String
    
    var logLevel: Logger.Level = .info
    
    func log(level: Logger.Level, message: Logger.Message, metadata: Logger.Metadata?, source: String, file: String, function: String, line: UInt) {
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

// Verwendung
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.yourapp")

logger.error("Dies ist eine Fehlermeldung")
```

Ausgabe auf stderr:
```
Dies ist eine Fehlermeldung
```

Dieser benutzerdefinierte Handler ermöglicht es Ihnen, Ihre SwiftLog-Fehlermeldungen direkt auf den Standardfehler zu leiten und nahtlos in andere Log-Nachrichten zu integrieren, die Ihre Anwendung generieren könnte.
