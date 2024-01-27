---
title:                "Koodin järjestäminen funktioihin"
date:                  2024-01-26T01:11:55.630004-07:00
model:                 gpt-4-1106-preview
simple_title:         "Koodin järjestäminen funktioihin"
programming_language: "Swift"
category:             "Swift"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Koodin järjestäminen funktioihin on tehtävien pilkkominen uudelleenkäytettäviksi osiksi. Tämä tekee koodista siistimmän, vähemmän virhealttiin ja helpommin vianetsittävän tai uudelleenmuotoiltavan.

## Kuinka:
Kuvittele tehtävä: laske taulukon keskiarvo. Ilman funktioita, liittäisit kaiken pääohjelmaan. Funktioiden avulla tekisit näin:

```swift
func laskeKeskiarvo(luvut: [Double]) -> Double {
    let summa = luvut.reduce(0, +)
    return luvut.isEmpty ? 0 : summa / Double(luvut.count)
}

// Käyttö
let pisteet = [92.5, 88.75, 99.0, 70.5]
let keskiPisteet = laskeKeskiarvo(luvut: pisteet)
print("Keskipisteet ovat \(keskiPisteet)")
```

Esimerkkituloste olisi:
```
Keskipisteet ovat 87.6875
```

## Syväsukellus
Historiallisesti kun ohjelmointi monimutkaistui, funktiot muodostuivat kivijalaksi monimutkaisuuden hallinnassa. Vaihtoehtoja ovat sisäänleivottu koodaus ja koodin kopioiminen ja liittäminen (spagetti-koodi) – joita nyt pidetään suurelta osin huonoina käytäntöinä. Swiftissä funktiot ovat ensiluokkaisia kansalaisia; niitä voidaan sijoittaa muuttujiin, välittää argumentteina ja palauttaa muista funktioista, mikä tekee koodista modulaarisempaa ja joustavampaa.

Toteutuksen osalta, suunnittele funktiosi tekemään yksi asia hyvin. Tavoittele funktioita, joilla on selkeä tarkoitus ja nimi, joka heijastaa sitä. Tarkkaile parametrien määrää – liian monia, ja todennäköisesti teet liikaa. Virheenkäsittely? Harkitse virheitä heittäviä funktioita ja käsittele ongelmia arvokkaasti. Muista: Swiftissä on kaikki kyse luettavuudesta ja ylläpidon helppoudesta.

## Katso myös
- [Swift-ohjelmointikielen opas - Funktiot](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Ray Wenderlichin Swift-tyyliopas](https://github.com/raywenderlich/swift-style-guide)
- [Martin Fowlerin Refaktorointi: Olemassa olevan koodin suunnittelun parantaminen](https://martinfowler.com/books/refactoring.html)