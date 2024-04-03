---
date: 2024-01-26 03:46:34.288823-07:00
description: "Das Runden von Zahlen bedeutet, einen numerischen Wert auf eine spezifische\
  \ Genauigkeit zu approximieren, typischerweise, um unerw\xFCnschte Dezimalstellen\u2026"
lastmod: '2024-03-13T22:44:54.220539-06:00'
model: gpt-4-0125-preview
summary: "Das Runden von Zahlen bedeutet, einen numerischen Wert auf eine spezifische\
  \ Genauigkeit zu approximieren, typischerweise, um unerw\xFCnschte Dezimalstellen\
  \ zu entfernen."
title: Zahlen runden
weight: 13
---

## Wie zu:
Swift bietet mehrere Möglichkeiten, Zahlen zu runden. Hier ein Vorgeschmack:

```Swift
let original = 3.14159

// Standardmäßiges Runden
let standardRounded = round(original) // 3.0

// Runden auf eine spezifische Dezimalstelle
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// Abrunden
let roundedDown = floor(original) // 3.0

// Aufrunden
let roundedUp = ceil(original) // 4.0

print("Standard: \(standardRounded), Dezimal: \(decimalRounded), Unten: \(roundedDown), Oben: \(roundedUp)")
```

Ausgabe: `Standard: 3.0, Dezimal: 3.142, Unten: 3.0, Oben: 4.0`

## Vertiefung
Historisch gesehen ist das Runden ein mathematisches Konzept, das es schon vor Computern gab, unerlässlich im Handel und in der Wissenschaft. Swifts `Foundation` Framework bietet umfassende Rundungsfunktionalität:

- `round(_: )` ist das gute alte Aufrunden zur nächsten halben Zahl.
- `floor(_: )` und `ceil(_: )` behandeln gerichtetes Runden.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` bietet feinere Kontrolle mit einem Enum für Rundungsregeln.

Seien Sie sich des `Decimal` Typs für präzise finanzielle Berechnungen bewusst, der Gleitkommafehler vermeidet. Erkunden Sie auch `NSDecimalNumber` für die Kompatibilität mit Objective-C.

## Siehe auch
- IEEE-Standard für Gleitkommaarithmetik (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
