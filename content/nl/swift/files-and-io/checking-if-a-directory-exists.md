---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:22.795740-07:00
description: 'Hoe te: Swift''s `FileManager` heeft de tools hiervoor. Gebruik zijn
  `fileExists(atPath:)` methode.'
lastmod: '2024-03-13T22:44:51.172123-06:00'
model: gpt-4-0125-preview
summary: Swift's `FileManager` heeft de tools hiervoor.
title: Controleren of een directory bestaat
weight: 20
---

## Hoe te:
Swift's `FileManager` heeft de tools hiervoor. Gebruik zijn `fileExists(atPath:)` methode:

```Swift
import Foundation

let fileManager = FileManager.default
let path = "/pad/naar/directory"

if fileManager.fileExists(atPath: path) {
    print("Ja, het is er!")
} else {
    print("Nee, bestaat niet.")
}
```

Voorbeelduitvoer als de directory bestaat:

```
Ja, het is er!
```

Of als het niet bestaat:

```
Nee, bestaat niet.
```

## Diepere Duik
Vóór `FileManager`, die kwam met het Foundation framework, waren UNIX-commando's in scripts gebruikelijk voor het controleren van paden. Maar `FileManager` is makkelijker en veiliger. Alternatieven in Swift omvatten het werken met de `URL` klasse en zijn `checkResourceIsReachable()` methode, hoewel het meer geschikt is voor het controleren van bestandsbeschikbaarheid en fouten kan genereren. Intern gebruikt `FileManager` de `stat` systeemaanroep om de aanwezigheid van een pad te verifiëren zonder rekening te houden met of het een bestand of een directory is, dus wanneer je moet differentiëren, moet je verder de eigenschappen van het pad inspecteren.

## Zie Ook
- Swift Documentatie: [`FileManager`](https://developer.apple.com/documentation/foundation/filemanager)
- Swift Boek: [Werken met Directories](https://docs.swift.org/swift-book/)
- Apple Developer Forums: [Bestandssysteem Toegang](https://developer.apple.com/forums/tags/file-system/)
