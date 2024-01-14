---
title:    "Swift: Kansion olemassaolon tarkistaminen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa onko kansio olemassa?

Monissa ohjelmointiprojekteissa on tärkeää varmistaa, että tiedostojärjestelmässä käytettävät kansiot ovat olemassa ennen kuin niihin yritetään tallentaa tai niistä yritetään lukea. Tämä varmistaa ohjelman sujuvan toiminnan ja välttää mahdollisia virheitä. Tässä blogikirjoituksessa kerromme miten tarkistaa, onko kansio olemassa käyttäen Swift-ohjelmointikieltä.

## Miten tarkistaa onko kansio olemassa?

Tarkistaaksesi onko kansio olemassa, käytä `fileManager`-luokan `fileExists(atPath:)`-metodia. Tämä metodi ottaa parametrinaan polun tarkistamaan kansiotiedostoon. Jos kansio on olemassa, palauttaa metodi totuusarvon `true` ja jos kansioa ei ole olemassa, palauttaa metodi totuusarvon `false`. 

```Swift
import Foundation

let fileManager = FileManager.default // Luodaan fileManager-objekti

let directory = "/Users/testikansio" // Määritellään kansion polku

// Tarkistetaan onko kansio olemassa
if fileManager.fileExists(atPath: directory) {
    print("Kansio on olemassa.")
} else {
    print("Kansiota ei ole olemassa.")
}

// Tulostaa:
// Kansiota ei ole olemassa.
```

## Syväsukellus

Tässä esimerkissä käytämme `directory`-muuttujaa, jossa määritellään kansiomme polku. Tämän jälkeen `fileManager`-objektia käytetään tarkistamaan onko kyseinen kansio olemassa. Tulostamme sitten viestin sen mukaan, oliko kansio olemassa vai ei.

On myös mahdollista tarkistaa, onko kansio olemassa käyttäen `fileManager`-luokan `enumerator(atPath:)`-metodia, joka palauttaa kaikki kansion sisältämät tiedostot ja alikansiot.

```Swift
import Foundation

let fileManager = FileManager.default // Luodaan fileManager-objekti

let directory = "/Users/testikansio" // Määritellään kansion polku

// Tarkistetaan onko kansio olemassa
if let enumerator = fileManager.enumerator(atPath: directory) {
    print("Kansio on olemassa ja se sisältää seuraavat tiedostot ja kansiot:")
    while let file = enumerator.nextObject() as? String {
        print(file)
    }
} else {
    print("Kansiota ei ole olemassa.")
}

// Tulostaa:
// Kansio on olemassa ja se sisältää seuraavat tiedostot ja kansiot:
// tiedosto1.txt
// tiedosto2.pdf
// alikansio
```

## Katso myös

- [File Manager Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [How to Check if a Directory Exists in Swift](https://stackoverflow.com/questions/45106514/how-to-check-if-a-directory-exists-in-swift)