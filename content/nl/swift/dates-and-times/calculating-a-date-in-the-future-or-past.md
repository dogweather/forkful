---
aliases:
- /nl/swift/calculating-a-date-in-the-future-or-past/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:15.099636-07:00
description: "Ooit moeten zoeken naar een datum in het verleden of toekomst? Programmatisch\
  \ berekenen we vaak datums voor deadlines, herinneringen of evenementen. Weten\u2026"
lastmod: 2024-02-18 23:09:02.243931
model: gpt-4-0125-preview
summary: "Ooit moeten zoeken naar een datum in het verleden of toekomst? Programmatisch\
  \ berekenen we vaak datums voor deadlines, herinneringen of evenementen. Weten\u2026"
title: Een datum in de toekomst of het verleden berekenen
---

{{< edit_this_page >}}

## Wat & Waarom?
Ooit moeten zoeken naar een datum in het verleden of toekomst? Programmatisch berekenen we vaak datums voor deadlines, herinneringen of evenementen. Weten hoe neemt het giswerk weg en laat je app tijdgevoelige taken accuraat afhandelen.

## Hoe te:
Swift maakt datumrekenen eenvoudig met `Calendar` en `DateComponents`. Hier is de essentie:

```Swift
import Foundation

// De datum van vandaag
let today = Date()

// Krijg de huidige kalender van de gebruiker
let currentCalendar = Calendar.current

// Voeg 2 weken toe aan vandaag
if let twoWeeksLater = currentCalendar.date(byAdding: .weekOfYear, value: 2, to: today) {
    print("Over twee weken: \(twoWeeksLater)")
}

// Trek 30 dagen af van vandaag
if let thirtyDaysBefore = currentCalendar.date(byAdding: .day, value: -30, to: today) {
    print("Dertig dagen geleden: \(thirtyDaysBefore)")
}
```

Output zou kunnen zijn:
```
Over twee weken: 2023-04-14 10:26:47 +0000
Dertig dagen geleden: 2023-03-15 10:26:47 +0000
```
Onthoud, de daadwerkelijke output zal variëren aangezien `Date()` je de huidige datum en tijd geeft.

## Diepgaand
Voor Swift regeerde Objective-C met zijn lastigere syntax. Swift's `Date`, `Calendar`, en `DateComponents` vereenvoudigen datumoperaties. Deze objecten respecteren tijdzones, gaan om met veranderingen voor zomertijd, en houden rekening met de kalenderinstellingen van de gebruiker – factoren die lastig te beheren waren in Objective-C.

Alternatieven omvatten bibliotheken van derden zoals SwiftDate, die nog meer gemak en functionaliteit kunnen bieden. Maar voor de meesten werken Swift's ingebouwde tools prima.

Datums zijn complex. Het zijn niet gewoon nummers om te verhogen of verlagen; ze betrekken kalenders, lokale specifieke zaken, en tijdzones. Apple's Foundation framework pakt deze complexiteit aan, zorgend dat je toekomstige en verleden datum berekeningen wereldwijd zinvol zijn.
