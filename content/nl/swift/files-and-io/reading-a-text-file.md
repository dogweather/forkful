---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:04.813260-07:00
description: 'Hoe: Om tekst uit een bestand in Swift te lezen, gebruik je de gemaksfuncties
  van de `String` klasse. Hier is een klein voorbeeld.'
lastmod: '2024-03-13T22:44:51.175312-06:00'
model: gpt-4-0125-preview
summary: Om tekst uit een bestand in Swift te lezen, gebruik je de gemaksfuncties
  van de `String` klasse.
title: Een tekstbestand lezen
weight: 22
---

## Hoe:
Om tekst uit een bestand in Swift te lezen, gebruik je de gemaksfuncties van de `String` klasse. Hier is een klein voorbeeld:

```Swift
import Foundation

if let filePath = Bundle.main.path(forResource: "voorbeeld", ofType: "txt") {
    do {
        let content = try String(contentsOfFile: filePath, encoding: .utf8)
        print(content)
    } catch {
        print("Oeps! Er is iets fout gegaan: \(error)")
    }
}
```
Als "voorbeeld.txt" de inhoud "Hallo, wereld!" bevat, is de uitvoer:
```
Hallo, wereld!
```

## Diepe Duik
Tekstbestanden lezen is zo oud als de weg naar Rome in de programmeerwereld. In het begin ging het allemaal over punchkaarten en tape. Nu, met hogere programmeertalen zoals Swift, is het eenvoudig. Het stukje code hierboven gebruikt `String(contentsOfFile:)`, maar er zijn alternatieven:

- `FileManager`: Goed voor complexere bestandsoperaties.
- `InputStream`: Gebruik dit wanneer je te maken hebt met grote bestanden - minder geheugenintensief.
- `URLSession`: Haal bestanden op van een externe server.

De `String(contentsOfFile:)` aanpak kan geheugenintensief zijn als het gebruikt wordt met mega-grote bestanden. Om problemen te voorkomen, overweeg methoden gebaseerd op streaming of chunked reading.

## Zie Ook
Duik in de officiële documentatie van Swift:
- [String](https://developer.apple.com/documentation/swift/string)
- [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Werken met URL Session](https://developer.apple.com/documentation/foundation/url_loading_system/fetching_website_data_into_memory)

Voor een dieper begrip, bekijk deze bronnen:
- [Apple's Gids voor Bestandssysteem Programmering](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html)
