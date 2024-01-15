---
title:                "Tilapäistiedoston luominen"
html_title:           "Swift: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Vuoden 2021 Swiftin versiossa on monia uusia ominaisuuksia, mm. mahdollisuus luoda väliaikaisia tiedostoja. Miksi siis vaivautua luomaan väliaikaista tiedostoa?

Väliaikaiset tiedostot tarjoavat kätevän tavan tallentaa väliaikainen tietoohjelman suorituksen aikana. Ne ovat erityisen hyödyllisiä, kun haluat tallentaa väliaikaisesti käyttäjän syöttämän tiedon tai luoda testidataa ohjelman kehityksen aikana.

## Kuinka

Luodaksesi väliaikaisen tiedoston Swiftillä, tarvitset vain muutaman koodirivin:

```Swift
// Luodaan väliaikainen tiedosto
let temporaryFile = FileManager.default.temporaryDirectory.appendingPathComponent("temp.txt")

// Kirjoitetaan tiedostoon tietoja
try "Tervetuloa väliaikaiseen tiedostoon!".write(to: temporaryFile, atomically: true, encoding: .utf8)

// Luetaan tiedostosta tietoja
let fileContents = try String(contentsOf: temporaryFile, encoding: .utf8)

// Tulostetaan tiedostosta luetut tiedot
print(fileContents)

// Lopuksi poistetaan tiedosto
try FileManager.default.removeItem(at: temporaryFile)
```

Kuten näet, käytämme `FileManager`-luokkaa luodaksemme `temporaryDirectory`-nimisen hakemiston, johon lisäämme halutun tiedostonimen. Tämän jälkeen voimme käyttää `write`-funktiota kirjoittaaksemme haluttuja tietoja tiedostoon ja `String`-luokan `contentsOf`-funktiota tiedostosta lukemiseen. Lopuksi käytämme `FileManager`-luokkaa poistaaksemme tiedoston.

## Syvällisempää tietoa

Swiftin `TemporaryDirectory` tarjoaa turvallisen ja luotettavan tavan luoda väliaikaisia tiedostoja. Tiedostot poistetaan automaattisesti järjestelmän ollessa käynnissä, joten sinun ei tarvitse huolehtia niiden poistamisesta manuaalisesti.

Voit myös käyttää `temporaryDirectory`-hakemistoa tallentaaksesi väliaikaisia kuvia, videoita ja muita tiedostoja. Tämä on erityisen hyödyllistä sovellusten kehityksessä, kun haluat tallentaa testidatan ja varmistaa, että sovellus toimii oikein erilaisilla tiedostotyypeillä.

Lisäksi voit käyttää `FileManager`-luokan muita funktioita hallitaksesi väliaikaisia tiedostoja, kuten käyttäjän käytön seurantaa ja tiedostojen salaus- ja purkuominaisuuksia.

## Katso myös

- [Apple Developer Documentation for FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift Standard Library Documentation for String](https://developer.apple.com/documentation/swift/string)
- [Creating and Managing Temporary Files in Swift](https://www.avanderlee.com/swift/temporary-files/)