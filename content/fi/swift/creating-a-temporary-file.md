---
title:    "Swift: Väliaikaisen tiedoston luominen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmoijat joutuvat joskus käsittelemään tiedostoja ja joskus on tarpeen luoda väliaikaisia tiedostoja. Tässä blogikirjoituksessa käydään läpi, miksi haluat ehkä luoda väliaikaisen tiedoston ja kuinka tehdä se Swiftillä.

## Kuinka luoda väliaikainen tiedosto Swiftillä

Väliaikaisen tiedoston luominen Swiftillä on helppoa ja vaivatonta. Voit käyttää siihen Foundation-kirjaston FileManager-luokkaa. Alla on esimerkki koodista, jossa luodaan väliaikainen tiedosto nimeltään "temp.txt":

```Swift

// Tuo Foundation-kirjasto
import Foundation

// Luo FileManager-instanssi
let fileManager = FileManager.default

// Luo tiedostopolku
let temporaryPath = fileManager.temporaryDirectory.appendingPathComponent("temp.txt")

do {
// Luo tiedosto annetusta tiedostopolusta ja tyhjällä dataobjektilla
try fileManager.createFile(atPath: temporaryPath.path, contents: nil)

// Tulosta onnistuminen
print("Väliaikainen tiedosto luotu: \(temporaryPath)")
} catch {
// Tulosta virhe
print(error)
}

```

## Syventyvä tarkastelu väliaikaisen tiedoston luomisesta

Väliaikaisen tiedoston luominen on kätevä tapa käsitellä tiedostoja tilapäisesti ja varmistaa, että niitä ei jää turhaan roikkumaan levylle. Tässä tarkemmin, kuinka FileManager-luokka toimii ja mitä muita vaihtoehtoja väliaikaisen tiedoston luomiseen on olemassa.

FileManager-luokalla on useita metodeja tiedostojen luomiseen, poistamiseen ja käsittelyyn. Koodissa käytetty temporaryDirectory-metodi palauttaa hakemiston, joka on varattu väliaikaisten tiedostojen käyttöön. Tämän hakemiston sisältämät tiedostot poistetaan automaattisesti käytön loputtua.

Toinen vaihtoehto väliaikaisen tiedoston luomiseen olisi käyttää tee-koetta ja poistamista tiedoston käytön jälkeen. Tämä ei kuitenkaan ole yhtä kätevä ja turvallinen kuin FileManager-luokan käyttö.

## Katso myös

- [FileManager-luokan dokumentaatio](https://developer.apple.com/documentation/foundation/filemanager)
- [Tiedostojen käsittely Swiftillä](https://www.hackingwithswift.com/quick-start/swiftui/how-to-read-and-write-basic-files-in-swiftui)