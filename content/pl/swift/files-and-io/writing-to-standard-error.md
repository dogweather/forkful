---
aliases:
- /pl/swift/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:57.285874-07:00
description: "Pisanie do standardowego b\u0142\u0119du (stderr) polega na kierowaniu\
  \ komunikat\xF3w o b\u0142\u0119dach lub wyj\u015Bcia diagnostycznego programu do\
  \ osobnego strumienia, odr\u0119bnego\u2026"
lastmod: 2024-02-18 23:08:49.968819
model: gpt-4-0125-preview
summary: "Pisanie do standardowego b\u0142\u0119du (stderr) polega na kierowaniu komunikat\xF3\
  w o b\u0142\u0119dach lub wyj\u015Bcia diagnostycznego programu do osobnego strumienia,\
  \ odr\u0119bnego\u2026"
title: "Pisanie do standardowego b\u0142\u0119du"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie do standardowego błędu (stderr) polega na kierowaniu komunikatów o błędach lub wyjścia diagnostycznego programu do osobnego strumienia, odrębnego od standardowego wyjścia (stdout). Jest to kluczowe dla debugowania i logowania błędów bez zaśmiecania standardowego wyjścia, co ułatwia zarówno programistom, jak i użytkownikom zrozumienie stanu i problemów programu.

## Jak to zrobić:

W Swift pisanie do standardowego błędu można wykonać za pomocą klasy `FileHandle` do bezpośredniego dostępu do stderr. Oto prosty przykład:

```swift
import Foundation

// Zdefiniuj komunikat
let errorMessage = "Wystąpił błąd.\n"

// Konwertuj komunikat na dane
if let data = errorMessage.data(using: .utf8) {
    // Zapisz komunikat o błędzie do stderr
    FileHandle.standardError.write(data)
}
```

Wyjście do stderr (zwykle wyświetlane w konsoli lub terminalu):
```
Wystąpił błąd.
```

Dla bardziej złożonego logowania lub przy pracy z zewnętrznymi bibliotekami, można rozważyć użycie biblioteki firm trzecich jak **SwiftLog**. Chociaż **SwiftLog** nie zapisuje bezpośrednio do stderr "od razu po wyjęciu z pudełka", możesz zaimplementować własne zaplecze logujące, aby to osiągnąć. Oto uproszczony przykład definiowania własnego programu obsługi logowania, który zapisuje do stderr:

Najpierw dodaj **SwiftLog** do zależności projektu w `Package.swift`:
```swift
// swift-tools-version:5.3

import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/apple/swift-log.git", from: "1.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: [
                .product(name: "Logging", package: "swift-log"),
            ]),
    ]
)
```

Następnie zaimplementuj własny program obsługi logowania, który zapisuje do stderr:

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

// Sposób użycia
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.yourapp")

logger.error("To jest komunikat błędu")
```

Wyjście do stderr:
```
To jest komunikat błędu
```

Ten własny program obsługi pozwala kierować twoje komunikaty o błędach SwiftLog bezpośrednio do standardowego błędu, integrując się bezproblemowo z innymi komunikatami logów, które twoja aplikacja może generować.
