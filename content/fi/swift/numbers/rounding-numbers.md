---
date: 2024-01-26 03:47:08.243908-07:00
description: "Numeroiden py\xF6rist\xE4minen tarkoittaa numeerisen arvon likim\xE4\
  \xE4r\xE4ist\xE4mist\xE4 tiettyyn tarkkuuteen, tyypillisesti ei-toivottujen desimaalien\
  \ poistamiseksi.\u2026"
lastmod: '2024-03-13T22:44:56.902172-06:00'
model: gpt-4-0125-preview
summary: "Numeroiden py\xF6rist\xE4minen tarkoittaa numeerisen arvon likim\xE4\xE4\
  r\xE4ist\xE4mist\xE4 tiettyyn tarkkuuteen, tyypillisesti ei-toivottujen desimaalien\
  \ poistamiseksi.\u2026"
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

## Mikä ja miksi?

Numeroiden pyöristäminen tarkoittaa numeerisen arvon likimääräistämistä tiettyyn tarkkuuteen, tyypillisesti ei-toivottujen desimaalien poistamiseksi. Ohjelmoijat pyöristävät muistin hallinnan, luettavuuden parantamisen ja toimialakohtaisten vaatimusten, kuten valuuttarajoitusten, täyttämiseksi.

## Kuinka:

Swift tarjoaa useita tapoja numeroiden pyöristämiseen. Tässä maistiainen:

```Swift
let alkuperainen = 3.14159

// Tavallinen pyöristäminen
let tavallisestiPyoristetty = round(alkuperainen) // 3.0

// Pyöristäminen tiettyyn desimaalipaikkaan
let desimaaliPyoristetty = Double(round(alkuperainen * 1000) / 1000) // 3.142

// Pyöristäminen alas
let pyoristettyAlas = floor(alkuperainen) // 3.0

// Pyöristäminen ylös
let pyoristettyYlos = ceil(alkuperainen) // 4.0

print("Standardi: \(tavallisestiPyoristetty), Desimaali: \(desimaaliPyoristetty), Alas: \(pyoristettyAlas), Ylös: \(pyoristettyYlos)")
```

Tuloste: `Standardi: 3.0, Desimaali: 3.142, Alas: 3.0, Ylös: 4.0`

## Syväsukellus

Historiallisesti pyöristäminen on matemaattinen käsite, joka on ollut olemassa ennen tietokoneita, olennaista kaupassa ja tieteessä. Swiftin `Foundation`-kehys tarjoaa kattavia pyöristystoiminnallisuuksia:

- `round(_: )` on vanha kunnon puoliväliin pyöristäminen.
- `floor(_: )` ja `ceil(_: )` hoitavat suunnattua pyöristämistä.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` antaa tarkemman hallinnan pyöristyssääntöjä enumin avulla.

Ole tietoinen `Decimal`-tyypistä tarkkoihin rahoituslaskelmiin, mikä välttää liukulukuvirheet. Tutustu myös `NSDecimalNumber`-luokkaan Objective-C-yhteensopivuuden vuoksi.

## Katso myös

- IEEE-standardi liukulukuaritmetiikalle (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
