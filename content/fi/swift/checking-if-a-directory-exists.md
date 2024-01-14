---
title:                "Swift: Tarkista onko hakemisto olemassa"
simple_title:         "Tarkista onko hakemisto olemassa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia eri syitä miksi haluat tarkistaa, onko hakemisto olemassa. Saatat esimerkiksi tarvita sitä, jotta voit tallentaa tiedostoja tai luoda uusia kansioita. Tai haluat ehkä varmistaa, että olet oikeassa paikassa ennen suorittamista tiettyjä toimia. Riippumatta siitä, miksi tarvitset tätä tarkistusta, se on tärkeää välttää virheitä ja parantaa ohjelmasi suorituskykyä.

## Kuinka

Tarkistaaksesi, onko hakemisto olemassa, käytä `fileExists(atPath:)` -metodia `FileManager`-luokasta. Tämä metodi ottaa parametrina polun tarkistettavaan hakemistoon ja palauttaa `true`, jos hakemisto löytyy ja `false` jos sitä ei ole. Alla on esimerkki koodista, jossa tarkistetaan, onko hakemisto nimeltä "Kuvat" olemassa ja tulostetaan vastaava viesti.

```Swift
let fileManager = FileManager.default
let path = "/Kuvat"

if fileManager.fileExists(atPath: path) {
    print("Hakemisto löytyi!")
} else {
    print("Hakemistoa ei löytynyt.")
}
```

Esimerkkituloste:

> Hakemistoa ei löytynyt.

## Syvällisempi katsaus

`fileExists(atPath:)` -metodi käyttää `FileManager`-luokan `attributesOfItem(atPath:)` -metodia tarkistaessaan, onko hakemisto olemassa. Tämä metodi on hyödyllinen myös saadaksesi tarkempia tietoja hakemistosta, kuten sen koon tai luontiajan. Voit käyttää tätä tietoa ohjelmasi parantamiseen ja tarvittaessa suorittaa lisätoimia hakemiston perusteella.

## Katso myös

- [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [attributesOfItem(atPath:)](https://developer.apple.com/documentation/foundation/filemanager/1415180-attributesofitem)
- [Directory and File Paths in Swift](https://www.appcoda.com/swift-string-path/)