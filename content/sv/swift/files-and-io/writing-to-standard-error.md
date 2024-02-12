---
title:                "Skriva till standardfel"
aliases:
- /sv/swift/writing-to-standard-error.md
date:                  2024-02-03T19:34:50.467067-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva till standardfel"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standardfel (stderr) handlar om att rikta ditt programs felmeddelanden eller diagnostikutdata till en separat ström, skild från standardutdata (stdout). Detta är avgörande för felsökning och loggning av fel utan att clutter standardutdata, vilket underlättar både utvecklarnas och användarnas förståelse för programmets tillstånd och problem.

## Hur man gör:

I Swift kan skrivning till standardfel göras med hjälp av klassen `FileHandle` för direkt åtkomst till stderr. Här är ett enkelt exempel:

```swift
import Foundation

// Definiera ett meddelande
let errorMessage = "Ett fel inträffade.\n"

// Konvertera meddelandet till data
if let data = errorMessage.data(using: .utf8) {
    // Skriv felmeddelandet till stderr
    FileHandle.standardError.write(data)
}
```

Utdata till stderr (visas vanligtvis i en konsol eller terminal):
```
Ett fel inträffade.
```

För mer komplex loggning eller när man arbetar med externa bibliotek kan man överväga att använda ett tredjepartsbibliotek som **SwiftLog**. Även om **SwiftLog** inte skriver till stderr direkt ur lådan, kan du implementera en anpassad loggbackend för att uppnå detta. Här är ett förenklat exempel på hur man definierar en anpassad logghanterare som skriver till stderr:

Först, lägg till **SwiftLog** i dina projektoberoenden i `Package.swift`:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "DittPaketnamn",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "DittMålnamn",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

Sedan, implementera en anpassad logghanterare som skriver till stderr:

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

// Användning
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.dittapp")

logger.error("Detta är ett felmeddelande")
```

Utdata till stderr:
```
Detta är ett felmeddelande
```

Denna anpassade hanterare låter dig dirigera dina SwiftLog felmeddelanden direkt till standardfel, och integreras sömlöst med andra loggmeddelanden som din applikation kan generera.
