---
title:                "Tarkistetaan löytyykö kansio"
html_title:           "Swift: Tarkistetaan löytyykö kansio"
simple_title:         "Tarkistetaan löytyykö kansio"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On tärkeää tietää, miten tarkistaa, onko hakemistoa olemassa Swift-ohjelmointikielellä, jotta voidaan varmistaa ohjelman toimivuus ja välttää mahdolliset virheet.

## Miten

Hakemistojen olemassaolon tarkistaminen Swiftillä on helppoa ja nopeaa. Käytännössä tämä voidaan tehdä käyttämällä FileManager-luokkaa, joka tarjoaa pääsyn tiedostojärjestelmään.

```Swift
let fileManager = FileManager.default

// Tarkistetaan, onko hakemistoa olemassa annetulla polulla
let directoryPath = "/Users/kayttajanimi/Documents"
var isDirectory: ObjCBool = false
let exists = fileManager.fileExists(atPath: directoryPath, isDirectory: &isDirectory)

// Tulostetaan tulos konsoliin
print("Hakemisto olemassa: \(exists)")
```

Tässä esimerkissä käytetään `fileExists(atPath:isDirectory:)` -metodia, joka palauttaa totuusarvon hakemiston olemassaolosta polussa ja asettaa `isDirectory` -muuttujan arvoksi `true`, jos kyseessä on hakemisto.

## Syvällinen sukellus

Swiftin `FileManager`-luokka tarjoaa myös muita hyödyllisiä metodeja tiedostojen ja hakemistojen hallintaan, kuten `createDirectory(atPath:withIntermediateDirectories:attributes:)` ja `removeItem(atPath:)`. Lisäksi `fileExists(atPath:)` -metodi hyödyntää `NSURL`-luokkaa polun käsittelyyn.

## Katso myös

- [Swiftin FileManager-luokan dokumentaatio](https://developer.apple.com/documentation/foundation/filemanager)
- [Swiftin FileManager-tutoriaali](https://www.hackingwithswift.com/example-code/system/how-to-create-a-directory-on-disk-using-filemanager)
- [Swiftin NSURL-luokan dokumentaatio](https://developer.apple.com/documentation/foundation/nsurl)