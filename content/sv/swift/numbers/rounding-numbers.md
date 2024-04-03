---
date: 2024-01-26 03:46:38.372831-07:00
description: "Hur man g\xF6r: Swift erbjuder flera s\xE4tt att avrunda tal. H\xE4\
  r \xE4r ett smakprov."
lastmod: '2024-03-13T22:44:38.246142-06:00'
model: gpt-4-0125-preview
summary: "Swift erbjuder flera s\xE4tt att avrunda tal."
title: Avrundning av tal
weight: 13
---

## Hur man gör:
Swift erbjuder flera sätt att avrunda tal. Här är ett smakprov:

```Swift
let original = 3.14159

// Standardavrundning
let standardRounded = round(original) // 3.0

// Avrundning till specifik decimalplats
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// Avrundning nedåt
let roundedDown = floor(original) // 3.0

// Avrundning uppåt
let roundedUp = ceil(original) // 4.0

print("Standard: \(standardRounded), Decimal: \(decimalRounded), Ned: \(roundedDown), Upp: \(roundedUp)")
```

Utdata: `Standard: 3.0, Decimal: 3.142, Ned: 3.0, Upp: 4.0`

## Djupdykning
Historiskt sett är avrundning ett matematiskt koncept som föregår datorer, avgörande i handel och vetenskap. Swifts `Foundation` ramverk erbjuder omfattande avrundningsfunktionalitet:

- `round(_: )` är bra gammal halv-upp avrundning.
- `floor(_: )` och `ceil(_: )` hanterar riktad avrundning.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` ger finare kontroll med avrundningsregler enum.

Var medveten om `Decimal` typen för precisa ekonomiska beräkningar, som undviker fel med flyttal. Utforska också `NSDecimalNumber` för kompatibilitet med Objective-C.

## Se även
- IEEE-standarden för flyttalsaritmetik (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
