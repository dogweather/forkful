---
date: 2024-01-26 01:11:55.630004-07:00
description: "Kuinka: Kuvittele teht\xE4v\xE4: laske taulukon keskiarvo. Ilman funktioita,\
  \ liitt\xE4isit kaiken p\xE4\xE4ohjelmaan. Funktioiden avulla tekisit n\xE4in."
lastmod: '2024-04-05T22:38:57.524690-06:00'
model: gpt-4-1106-preview
summary: "Kuvittele teht\xE4v\xE4: laske taulukon keskiarvo. Ilman funktioita, liitt\xE4\
  isit kaiken p\xE4\xE4ohjelmaan. Funktioiden avulla tekisit n\xE4in."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

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
