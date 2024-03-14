---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:43.952308-07:00
description: "Scrivere sull'errore standard (stderr) consiste nel dirigere i messaggi\
  \ di errore o l'output di diagnostica del tuo programma su un flusso separato,\u2026"
lastmod: '2024-03-13T22:44:43.785924-06:00'
model: gpt-4-0125-preview
summary: "Scrivere sull'errore standard (stderr) consiste nel dirigere i messaggi\
  \ di errore o l'output di diagnostica del tuo programma su un flusso separato,\u2026"
title: Scrivere sull'errore standard
---

{{< edit_this_page >}}

## Cosa e Perché?

Scrivere sull'errore standard (stderr) consiste nel dirigere i messaggi di errore o l'output di diagnostica del tuo programma su un flusso separato, distinto dall'output standard (stdout). Questo è fondamentale per il debug e la registrazione degli errori senza ingombrare l'output standard, facilitando la comprensione dello stato e dei problemi del programma sia da parte degli sviluppatori che degli utenti.

## Come fare:

In Swift, scrivere sull'errore standard può essere fatto utilizzando la classe `FileHandle` per un accesso diretto a stderr. Ecco un semplice esempio:

```swift
import Foundation

// Definire un messaggio
let errorMessage = "Si è verificato un errore.\n"

// Convertire il messaggio in dati
if let data = errorMessage.data(using: .utf8) {
    // Scrivere il messaggio di errore su stderr
    FileHandle.standardError.write(data)
}
```

Output su stderr (tipicamente visualizzato in una console o un terminale):
```
Si è verificato un errore.
```

Per una registrazione più complessa o quando si lavora con librerie esterne, si potrebbe considerare l'uso di una libreria di terze parti come **SwiftLog**. Anche se **SwiftLog** non scrive direttamente su stderr di default, è possibile implementare un backend di registrazione personalizzato per ottenere ciò. Ecco un esempio semplificato di definizione di un gestore di log personalizzato che scrive su stderr:

Prima, aggiungi **SwiftLog** alle dipendenze del tuo progetto in `Package.swift`:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "IlTuoNomePacchetto",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "IlTuoNomeTarget",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

Poi, implementa un gestore di log personalizzato che scrive su stderr:

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

// Utilizzo
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.esempio.latuaapp")

logger.error("Questo è un messaggio di errore")
```

Output su stderr:
```
Questo è un messaggio di errore
```

Questo gestore personalizzato ti permette di indirizzare i tuoi messaggi di errore SwiftLog direttamente all'errore standard, integrandosi senza soluzione di continuità con altri messaggi di log che la tua applicazione potrebbe generare.
