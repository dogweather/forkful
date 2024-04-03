---
date: 2024-01-26 03:47:08.243908-07:00
description: "Kuinka: Swift tarjoaa useita tapoja numeroiden py\xF6rist\xE4miseen.\
  \ T\xE4ss\xE4 maistiainen."
lastmod: '2024-03-13T22:44:56.902172-06:00'
model: gpt-4-0125-preview
summary: "Swift tarjoaa useita tapoja numeroiden py\xF6rist\xE4miseen."
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

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
