---
title:                "Swift: Alirangon erottaminen"
simple_title:         "Alirangon erottaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen on tärkeä osa Swift-ohjelmointia, sillä se mahdollistaa merkkijonojen tarkemman käsittelyn. Näin voit esimerkiksi etsiä tiettyjä sanoja tai lauseita tekstistäsi helpommin.

## Miten

Substringien erottaminen Swiftissä on yksinkertaista. Voit käyttää `substring()`-funktiota, joka ottaa parametreikseen halutun merkkijonon ja sen enimmäispituuden. Tämän jälkeen voit käyttää alku- ja loppuindeksejä määritelläksesi, missä kohtaa merkkijonosta haluat ottaa substringin. Katso esimerkki alla:

```Swift
// Alkuperäinen merkkijono
let teksti = "Tämä on esimerkki merkkijonosta"

// Splitataan teksti välilyöntien kohdalta
let sanat = teksti.components(separatedBy: " ")

// Tulostetaan kaikki sanat yhdessä rivissä
print(sanat.joined(separator: " "))

// Tulostaa: Tämä on esimerkki merkkijonosta

// Otetaan substring "esimerkki" käyttäen alku- ja loppuindeksejä
let esimerkki = teksti.substring(from: 8, to: 16)

// Tulostetaan substring "esimerkki"
print(esimerkki)

// Tulostaa: esimerkki
```

## Syväsukellus

Substringien erottaminen on hyödyllistä myös esimerkiksi silloin, kun haluat muuttaa osan merkkijonosta toiseksi. Voit esimerkiksi korvata tietyt sanat tai merkit toisilla haluamillasi sanoilla tai merkeillä. Voit myös käyttää erilaisia metodeja, kuten `prefix()` ja `suffix()` erottaaksesi merkkijonon alku- ja loppuosat.

Merkkijonojen käsittely ja substringien erottaminen ovat tärkeitä taitoja, joita jokaisen Swift-kehittäjän tulisi hallita. Ne avaavat uusia mahdollisuuksia koodin käsittelyyn ja tekevät siitä tehokkaampaa.

## Katso myös

- [Swiftin virallinen dokumentaatio substringien erottamiselle](https://developer.apple.com/documentation/swift/string/1539162-substring)
- [Tietoa merkkijonojen käsittelystä Swiftissä](https://www.swift-programming-4-you.com/manipulating-strings.html)
- [Esimerkkejä substringien erottamisesta käyttäen eri metodeja](https://www.tutorialspoint.com/extracting-substrings-in-swift)