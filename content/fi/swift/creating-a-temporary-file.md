---
title:                "Swift: **Luodaan väliaikainen tiedosto"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi luoda tilapäistiedosto?

Tilapäistiedostojen luominen on tärkeä osa ohjelmointia monessa tilanteessa. Voit käyttää niitä tallentamaan väliaikaisia tietoja tai luomaan välillisiä tiedostoja, jotka tarvitset suorittaessasi tiettyjä toimintoja ohjelmassasi. Tilapäistiedostot mahdollistavat myös tiedon tallentamisen väliaikaisesti ja sen poistamisen ohjelman suorituksen jälkeen, mikä voi olla hyödyllistä tietoturvasyistä.

# Kuinka luoda tilapäistiedosto Swiftillä?

Swift tarjoaa kätevän tavan luoda tilapäistiedostoja käyttäen FileManager-luokkaa. Se tarjoaa valmiita metodeja luoda, avata, kirjoittaa ja poistaa tiedostoja. Seuraavassa esimerkissä näytetään, miten voit luoda ja kirjoittaa tekstiä tilapäistiedostoon.

```Swift
let fileManager = FileManager.default
let temporaryFolder = NSTemporaryDirectory()
let temporaryFile = temporaryFolder + "temp_file.txt"

if fileManager.createFile(atPath: temporaryFile, contents: nil, attributes: nil) {
    print("Tilapäistiedosto luotu!")
    
    // Kirjoitetaan tekstiä tiedostoon
    let textData = "Tämä on tilapäistiedoston teksti."
    if let fileHandle = FileHandle(forWritingAtPath: temporaryFile) {
        fileHandle.seekToEndOfFile()
        fileHandle.write(textData.data(using: .utf8)!)
        fileHandle.closeFile()
    }
    print("Teksti kirjoitettu tilapäistiedostoon!")
} else {
    print("Tilapäistiedoston luominen epäonnistui.")
}
```

Tämän koodiesimerkin tulostus näyttää seuraavalta:

```
Tilapäistiedosto luotu!
Teksti kirjoitettu tilapäistiedostoon!
```

# Syvällisempi tarkastelu tilapäistiedostojen luomisesta

Swiftin FileManager-luokka tarjoaa lisäksi monia muita hyödyllisiä metodeja, joita voit käyttää tilapäistiedostojen luomiseen ja hallintaan. Voit esimerkiksi tarkistaa tiedoston olemassaolon, luoda puuhakemistoja ja noutaa tiedoston tiedot. On myös tärkeää varmistaa, että poistat tilapäistiedoston ohjelman suorituksen jälkeen, jotta et jätä jälkeesi turhia ja ehkä arkaluonteisia tietoja.

Lisätietoja FileManager-luokan tarjoamista metodeista ja niiden käytöstä voit lukea Swiftin dokumentaatiosta.

# Katso myös

* [FileManager-luokan dokumentaatio](https://developer.apple.com/documentation/foundation/filemanager)
* [Swiftin dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
* [Tietoturvasuositukset tiedostojen käsittelyyn Swiftillä](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)