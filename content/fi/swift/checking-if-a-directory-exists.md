---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:58:40.317461-07:00
html_title:           "Gleam: Onko hakemisto olemassa? Tarkistaminen"
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Tarkistetaan, onko tiedostopolku olemassa kun on tarvetta varmistaa ettei esimerkiksi kirjoiteta päällekkäin tärkeitä dataa. Tämä on yleinen turvaoperaatio tiedostojen kanssa työskentelyssä.

## How to: (Kuinka tehdä:)
```Swift
import Foundation

let fileManager = FileManager.default
let directoryPath = "/path/to/directory"

if fileManager.fileExists(atPath: directoryPath) {
    print("Kansio on olemassa.")
} else {
    print("Kansio ei ole olemassa.")
}
```
Esimerkin tulostus riippuu polun olemassaolosta.

## Deep Dive (Syväsukellus)
Historiallisesti tiedostonhallintatoiminnot ovat olleet keskeinen osa ohjelmointia. Swift käyttää FileManager-luokkaa, joka perustuu Cocoa Frameworkin tarjoamiin palveluihin. Vaihtoehtoja ovat esimerkiksi komentorivikomennot shellissä tai alhaisemman tason system calls, mutta `FileManager` on suositeltava Swiftin käyttöympäristössä.

## See Also (Katso Myös)
- [FileManager Class Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift Standard Library](https://developer.apple.com/documentation/swift/swift_standard_library)