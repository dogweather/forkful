---
title:                "Tiedoston lukeminen"
html_title:           "Swift: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet kiinnostunut oppimaan miten lukea tekstitiedostoja Swift-ohjelmointikielellä, tämä artikkeli on juuri sinulle. Tekstitiedostojen lukeminen on tärkeä taito, joka helpottaa tietojen käsittelyä ja ohjelman suorittamista.

## Miten tehdä se

```Swift
// Luodaan uusi tiedostopuskuri käyttäen tiedoston polkua ja nimeä 
let tiedostopuskuri = FileHandle(forReadingAtPath: "polku/tiedosto.txt")

// Tarkistetaan että tiedostopuskuri on olemassa ja voimme lukea tiedoston
if tiedostopuskuri == nil {
    print("Tiedostoa ei löydy!")
} else {
    // Luetaan tiedostosta kaikki rivit ja tallennetaan ne muuttujaan
    let sisalto = String(data: (tiedostopuskuri?.readDataToEndOfFile())!, encoding: .utf8)
    // Tulostetaan tiedoston sisältö
    print(sisalto!)
}

// Muista sulkea tiedostopuskuri käytön jälkeen
tiedostopuskuri?.closeFile()
```

Esimerkissä luomme tiedostopuskurin, tarkistamme sen tilan ja lopulta luemme tiedoston sisällön ja tulostamme sen. Muista aina sulkea tiedostopuskuri, kun olet lopettanut sen käytön.

## Syvempi sukellus

Tekstitiedostojen lukeminen Swiftissä tapahtuu käyttämällä Foundation Frameworkin tarjoamaa FileHandle-luokkaa. Tämä luokka tarjoaa erilaisia metodeja tiedoston käsittelyyn, kuten lukemiseen, kirjoittamiseen ja selaamiseen. On tärkeää tarkistaa, että tiedostopuskuri on olemassa ennen sen käyttöä, jotta voimme välttää mahdolliset virheilmoitukset.

## Katso myös

- [Swiftin virallinen dokumentaatio](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html)
- [Tekstipuskurin lukeminen ja kirjoittaminen Swiftissä](https://www.swiftlondon.com/blog/2016/03/28/reading-writing-file-swift/)
- [Swiftin perusteet: Tiedostojen käsittely](https://www.raywenderlich.com/875-tiedostojen-kasittely-swiftilla)