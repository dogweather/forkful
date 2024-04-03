---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:25.297210-07:00
description: "Een datum parseren uit een tekstreeks betekent het omzetten van de tekstuele\
  \ representatie van een datum (zoals \"2023-04-01\") naar een Date-object.\u2026"
lastmod: '2024-03-13T22:44:51.167001-06:00'
model: gpt-4-0125-preview
summary: Een datum parseren uit een tekstreeks betekent het omzetten van de tekstuele
  representatie van een datum (zoals "2023-04-01") naar een Date-object.
title: Een datum uit een string parsen
weight: 30
---

## Wat & Waarom?

Een datum parseren uit een tekstreeks betekent het omzetten van de tekstuele representatie van een datum (zoals "2023-04-01") naar een Date-object. Programmeurs doen dit om met data te manipuleren, berekeningen uit te voeren of in verschillende formaten weer te geven.

## Hoe:

Swift maakt het parseren van data vrij eenvoudig met `DateFormatter`. Hier is een snel voorbeeld:

```Swift
import Foundation

let dateString = "2023-04-01"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"

if let parsedDate = dateFormatter.date(from: dateString) {
    print("Geparste datum: \(parsedDate)")
} else {
    print("Datum parseren mislukt.")
}
```

Een voorbeelduitvoer kan er zo uitzien, afhankelijk van je tijdzone:

```
Geparste datum: 2023-03-31 22:00:00 +0000
```

Onthoud, de uitvoer is standaard in UTC!

## Diepere Duik

Al in Objective-C hadden iOS-ontwikkelaars `NSDateFormatter`, en het werd overgenomen in Swift als `DateFormatter`. Historisch gezien was het omgaan met data een groot probleem vanwege variaties in formaat en tijdzones. Gelukkig standaardiseert `DateFormatter` in Swift dit proces.

Hoewel `DateFormatter` goed is voor gangbare scenario's, bestaan er alternatieven zoals de `ISO8601DateFormatter` voor ISO 8601 formaten, en je kunt zelfs dieper duiken in de lagere-niveau `Cocoa` API met `CFDateFormatter` voor meer controle.

Bij het implementeren van een datum-parser, stel altijd de `locale` in op `posix` (`en_US_POSIX`) om onverwacht gedrag vanwege gebruikersinstellingen te vermijden. Wees ook bewust van prestaties. Datumparsing is kostbaar, dus hergebruik je formatter of overweeg `DateComponents` te gebruiken voor herhaalde taken.

## Zie Ook

- [NSDateFormatter - Apple Developer](https://developer.apple.com/documentation/foundation/nsdateformatter)
