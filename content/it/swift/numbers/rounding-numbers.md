---
date: 2024-01-26 03:46:52.871494-07:00
description: 'Come fare: Swift offre diversi modi per arrotondare i numeri. Ecco un
  assaggio.'
lastmod: '2024-03-13T22:44:43.765417-06:00'
model: gpt-4-0125-preview
summary: Swift offre diversi modi per arrotondare i numeri.
title: Arrotondamento dei numeri
weight: 13
---

## Come fare:
Swift offre diversi modi per arrotondare i numeri. Ecco un assaggio:

```Swift
let original = 3.14159

// Arrotondamento standard
let standardRounded = round(original) // 3.0

// Arrotondamento a un punto decimale specifico
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// Arrotondamento verso il basso
let roundedDown = floor(original) // 3.0

// Arrotondamento verso l'alto
let roundedUp = ceil(original) // 4.0

print("Standard: \(standardRounded), Decimale: \(decimalRounded), Verso il basso: \(roundedDown), Verso l'alto: \(roundedUp)")
```

Output: `Standard: 3.0, Decimale: 3.142, Verso il basso: 3.0, Verso l'alto: 4.0`

## Approfondimento
Storicamente, l'arrotondamento è un concetto matematico che precede i computer, essenziale nel commercio e nella scienza. Il framework `Foundation` di Swift offre una funzionalità di arrotondamento completa:

- `round(_: )` è il buon vecchio arrotondamento a metà verso l'alto.
- `floor(_: )` e `ceil(_: )` gestiscono l'arrotondamento direzionale.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` offre un controllo più fine con l'enum delle regole di arrotondamento.

Sii consapevole del tipo `Decimal` per calcoli finanziari precisi, il quale evita errori di virgola mobile. Esplora anche `NSDecimalNumber` per la compatibilità con Objective-C.

## Vedi Anche
- Standard IEEE per l'aritmetica in virgola mobile (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
