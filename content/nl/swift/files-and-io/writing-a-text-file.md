---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:45.292662-07:00
description: "Een tekstbestand schrijven houdt in dat gegevens worden opgeslagen als\
  \ leesbare tekst, met behulp van een karaktercodering zoals UTF-8. Programmeurs\
  \ doen\u2026"
lastmod: '2024-03-13T22:44:51.176306-06:00'
model: gpt-4-0125-preview
summary: "Een tekstbestand schrijven houdt in dat gegevens worden opgeslagen als leesbare\
  \ tekst, met behulp van een karaktercodering zoals UTF-8. Programmeurs doen\u2026"
title: Een tekstbestand schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekstbestand schrijven houdt in dat gegevens worden opgeslagen als leesbare tekst, met behulp van een karaktercodering zoals UTF-8. Programmeurs doen dit voor logboekregistratie, gegevensbehoud of configuratie.

## Hoe:
Tekst naar een bestand schrijven in Swift is eenvoudig met de `String` klasse en `FileManager`. Hier is een snel voorbeeld:

```Swift
import Foundation

let stringToWrite = "Hallo, Swift!"
let fileURL = FileManager.default.urls(voor: .documentDirectory, in: .userDomainMask).first?.appendingPathComponent("voorbeeld.txt")

do {
    probeer stringToWrite.write(naar: fileURL!, atomisch: true, codering: .utf8)
    print("Bestand succesvol geschreven")
} catch {
    print("Fout bij het schrijven naar bestand: \(error)")
}
```

Voorbeelduitvoer:
```
Bestand succesvol geschreven
```

## Diepgaande Duik
Tekstbestanden schrijven is zo oud als computers zelf, vaak gebruikt voor kleine gegevensopslag voordat databases gemeengoed werden. Belangrijke alternatieven zijn databases en gebruikersinstellingen, die gestructureerd zijn en efficiënter voor grotere gegevenssets. Bij het schrijven van bestanden in Swift zorgt de `write(to:atomically:encoding:)` methode voor atomische schrijfoperaties, die tijdens een schrijfoperatie gegevenscorruptie voorkomen.

## Zie Ook
- Swift String Documentatie: https://developer.apple.com/documentation/swift/string
- FileManager Gids: https://developer.apple.com/documentation/foundation/filemanager
- Werken met JSON in Swift: https://developer.apple.com/swift/blog/?id=37
- Bestandsbeheer in Swift Tutorial: https://www.raywenderlich.com/1881-file-handling-in-swift-tutorial
