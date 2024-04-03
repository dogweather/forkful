---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:46.943051-07:00
description: "Getallen afronden betekent het benaderen van een numerieke waarde tot\
  \ een specifieke precisie, typisch om ongewenste decimalen te verwijderen.\u2026"
lastmod: '2024-03-13T22:44:51.151320-06:00'
model: gpt-4-0125-preview
summary: Getallen afronden betekent het benaderen van een numerieke waarde tot een
  specifieke precisie, typisch om ongewenste decimalen te verwijderen.
title: Afronden van getallen
weight: 13
---

## Wat & Waarom?

Getallen afronden betekent het benaderen van een numerieke waarde tot een specifieke precisie, typisch om ongewenste decimalen te verwijderen. Programmeurs ronden af om geheugengebruik te beheren, de leesbaarheid te verbeteren en om aan domeinspecifieke vereisten te voldoen zoals valutabeperkingen.

## Hoe te:

Swift biedt verschillende manieren om getallen af te ronden. Hier is een voorproefje:

```Swift
let origineel = 3.14159

// Standaard afronden
let standaardAfgerond = round(origineel) // 3.0

// Afronden op specifieke decimalen
let decimaalAfgerond = Double(round(origineel * 1000) / 1000) // 3.142

// Naar beneden afronden
let naarBenedenAfgerond = floor(origineel) // 3.0

// Naar boven afronden
let naarBovenAfgerond = ceil(origineel) // 4.0

print("Standaard: \(standaardAfgerond), Decimaal: \(decimaalAfgerond), Naar beneden: \(naarBenedenAfgerond), Naar boven: \(naarBovenAfgerond)")
```

Uitvoer: `Standaard: 3.0, Decimaal: 3.142, Naar beneden: 3.0, Naar boven: 4.0`

## Verdieping

Historisch gezien is afronden een wiskundig concept dat al bestond vóór computers, essentieel in handel en wetenschap. Swift's `Foundation` framework biedt uitgebreide afrondingsfunctionaliteiten:

- `round(_: )` is de goede oude afronding naar boven/onder.
- `floor(_: )` en `ceil(_: )` handelen richtingsafronding af.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` biedt fijnere controle met een enum van afrondingsregels.

Wees bewust van het `Decimal` type voor precieze financiële berekeningen, dat drijvende-kommagetallenfouten vermijdt. Verken ook `NSDecimalNumber` voor compatibiliteit met Objective-C.

## Zie ook

- IEEE-standaard voor drijvende-komma rekenkunde (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
