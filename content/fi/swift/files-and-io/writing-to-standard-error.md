---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:53.375248-07:00
description: "Kuinka: Swiftiss\xE4 standardivirheeseen kirjoittaminen voidaan suorittaa\
  \ k\xE4ytt\xE4m\xE4ll\xE4 `FileHandle`-luokkaa suoraa stderr-p\xE4\xE4sy\xE4 varten.\
  \ T\xE4ss\xE4 on\u2026"
lastmod: '2024-03-13T22:44:56.922903-06:00'
model: gpt-4-0125-preview
summary: "Swiftiss\xE4 standardivirheeseen kirjoittaminen voidaan suorittaa k\xE4\
  ytt\xE4m\xE4ll\xE4 `FileHandle`-luokkaa suoraa stderr-p\xE4\xE4sy\xE4 varten."
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Kuinka:
Swiftissä standardivirheeseen kirjoittaminen voidaan suorittaa käyttämällä `FileHandle`-luokkaa suoraa stderr-pääsyä varten. Tässä on yksinkertainen esimerkki:

```swift
import Foundation

// Määritä viesti
let errorMessage = "Tapahtui virhe.\n"

// Muunna viesti dataksi
if let data = errorMessage.data(using: .utf8) {
    // Kirjoita virheviesti stderriin
    FileHandle.standardError.write(data)
}
```

Tuloste stderriin (tyypillisesti nähtävissä konsolissa tai terminaalissa):
```
Tapahtui virhe.
```

Monimutkaisempaa lokitusta varten tai työskenneltäessä ulkoisten kirjastojen kanssa, voisi harkita kolmannen osapuolen kirjaston, kuten **SwiftLog**, käyttöä. Vaikka **SwiftLog** ei suoraan kirjoita stderriin laatikosta ulos, voit toteuttaa mukautetun lokitus-taustajärjestelmän tämän saavuttamiseksi. Tässä on yksinkertaistettu esimerkki mukautetun lokin käsittelijän määrittelemisestä, joka kirjoittaa stderriin:

Lisää ensin **SwiftLog** projektisi riippuvuuksiin `Package.swift`-tiedostossa:
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

Toteuta sitten mukautettu lokin käsittelijä, joka kirjoittaa stderriin:

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

// Käyttö
LoggingSystem.bootstrap(StderrLogHandler.init)
let logger = Logger(label: "com.example.yourapp")

logger.error("Tämä on virheviesti")
```

Tuloste stderriin:
```
Tämä on virheviesti
```

Tämä mukautettu käsittelijä mahdollistaa sinun reitittää SwiftLog-virheviestisi suoraan standardivirheeseen, integroituen saumattomasti muiden lokiviestien kanssa, joita sovelluksesi saattaisi tuottaa.
