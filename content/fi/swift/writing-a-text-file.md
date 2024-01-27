---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tiedoston kirjoittaminen koostuu datan tallentamisesta tekstin muodossa levylle. Koodarit käyttävät tätä toimintoa datan pysyvään tallennukseen ja myöhempään käsittelyyn.

## How to:
Swiftissä tekstitiedostoon kirjoittaminen on suoraviivaista.

```Swift
import Foundation

let fileManager = FileManager.default
let path = fileManager.currentDirectoryPath.appending("/example.txt")
let content = "Moi Swift-maailma!"

do {
    try content.write(toFile: path, atomically: true, encoding: .utf8)
    print("Tiedosto kirjoitettu.")
} catch {
    print("Virhe tiedoston kirjoittamisessa: \(error)")
}
```

Kun tämä koodi ajetaan, se luo `example.txt` tiedoston ja kirjoittaa siihen "Moi Swift-maailma!".

## Deep Dive
Tiedostoja on kirjoitettu ohjelmoinnissa aikojen alusta. Swift tarjoaa `Foundation` -kirjaston kautta helppokäyttöiset työkalut tiedostonhallintaan. Vaihtoehtoisesti voit käyttää alhaisemman tason C-kirjastoja, kuten `fwrite` POSIX API:sta, mutta Swiftin omat kirjoitusmetodit ovat yleensä riittäviä ja turvallisia. Atomisesti kirjoittaminen varmistaa, että tiedosto tallennetaan kunnolla virhetilanteissakin.

## See Also
- Swiftin virallinen dokumentaatio tiedostojenkäsittelystä: [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- Swiftin virallisen foorumin keskusteluja tiedostojen käsittelystä: [Swift Forums](https://forums.swift.org/c/related-projects/foundation)
