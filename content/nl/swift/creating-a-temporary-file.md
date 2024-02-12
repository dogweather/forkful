---
title:                "Een tijdelijk bestand aanmaken"
aliases:
- nl/swift/creating-a-temporary-file.md
date:                  2024-01-28T21:58:52.706474-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tijdelijk bestand aanmaken"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tijdelijk bestand maken is het creëren van een kortstondig bestand om gegevens op te slaan die niet op lange termijn nodig zijn. Programmeurs doen dit om data te verwerken die alleen relevant is tijdens de uitvoering van het programma of om te voorkomen dat de opslag van de gebruiker verstopt raakt met onnodige bestanden.

## Hoe doe je dat:
Swift maakt het vrij eenvoudig om tijdelijke bestanden te creëren, met behulp van de `FileManager` klasse. Hier is hoe je snel een tijdelijk bestand maakt en er wat tekst in schrijft:

```Swift
import Foundation

// Creëer een URL voor de tijdelijke map
let tempDirectoryURL = FileManager.default.temporaryDirectory

// Creëer een unieke bestandsnaam
let fileName = UUID().uuidString

// Construeer de volledige bestands-URL
let fileURL = tempDirectoryURL.appendingPathComponent(fileName)

// Voorbeeldtekst om te schrijven
let sampleText = "Hello, temporary world!"

do {
    // Schrijf de tekst naar het tijdelijke bestand
    try sampleText.write(to: fileURL, atomically: true, encoding: .utf8)
    print("Bestand gecreëerd: \(fileURL)")
} catch {
    print("Mislukt om naar bestand te schrijven: \(error)")
}

// Voorbeelduitvoer:
// Bestand gecreëerd: file:///pad/naar/temp/map/E0B4952E-5BEE-47E7-B5BB-DA5E6AF1EDC9
```

Om het bestand te lezen, draai je het script gewoon om—hier is hoe:

```Swift
do {
    // Lees de tekst van het tijdelijke bestand
    let savedText = try String(contentsOf: fileURL, encoding: .utf8)
    print("Bestandsinhoud: \(savedText)")
} catch {
    print("Mislukt om bestand te lezen: \(error)")
}

// Voorbeelduitvoer:
// Bestandsinhoud: Hello, temporary world!
```

Ruim na jezelf op door het temp bestand te verwijderen:

```Swift
do {
    // Verwijder het tijdelijke bestand
    try FileManager.default.removeItem(at: fileURL)
    print("Tijdelijk bestand verwijderd.")
} catch {
    print("Mislukt om bestand te verwijderen: \(error)")
}

// Voorbeelduitvoer:
// Tijdelijk bestand verwijderd.
```

## Diepere Duik
Voor `FileManager`, beheerden mensen bestanden op meer omslachtige manieren. Herinner je C’s `tmpfile()`? In vergelijking is Swift's `FileManager` een verademing: eenvoudig en modern.

Alternatieven? Zeker. Je zou in-memory representaties zoals `Data` of `String` kunnen gebruiken, perfect voor echt tijdelijke data met beperkte grootte. Een andere route is het gebruik van een aangepaste temp-bestandsmanager voor meer controle, maar dat is meestal overbodig.

De kern van de zaak: `FileManager` gebruikt de systeem's temp directory, die af en toe wordt opgeschoond, maar niet na elke programma-uitvoer. Houd dat in gedachten als het gaat om veiligheid of gevoelige gegevens - ruim handmatig op indien nodig.

## Zie Ook
Bekijk deze bronnen voor meer informatie over het omgaan met bestanden in Swift:
- [Apple's FileManager Documentatie](https://developer.apple.com/documentation/foundation/filemanager)
- [NSHipster artikel over bestandsbeheer](https://nshipster.com/temporary-files/)
- [Ray Wenderlich’s handleiding voor werken met het bestandssysteem in Swift](https://www.raywenderlich.com/666-filemanager-class-tutorial-for-macos-getting-started)
