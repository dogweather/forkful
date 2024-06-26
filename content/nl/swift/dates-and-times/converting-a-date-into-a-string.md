---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:33.840811-07:00
description: 'Hoe te: Swift gebruikt `DateFormatter` om `Date` objecten om te zetten
  in leesbare strings. Hier is hoe.'
lastmod: '2024-03-13T22:44:51.169206-06:00'
model: gpt-4-0125-preview
summary: Swift gebruikt `DateFormatter` om `Date` objecten om te zetten in leesbare
  strings.
title: Een datum converteren naar een string
weight: 28
---

## Hoe te:
Swift gebruikt `DateFormatter` om `Date` objecten om te zetten in leesbare strings. Hier is hoe:

```Swift
import Foundation

let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let dateString = formatter.string(from: date)
print(dateString) // Uitvoer: "2023-04-05 14:20:35" (of huidige datum en tijd)
```

Verander de `dateFormat` om aan te passen hoe je datum eruit ziet:

```Swift
formatter.dateFormat = "EEEE, MMM d, yyyy"
print(formatter.string(from: date)) // Uitvoer: "woensdag, apr 5, 2023"
```

## Diepgaand
Voor `DateFormatter` gebruikten Objective-C en vroege versies van Swift `NSDateFormatter`, wat in wezen hetzelfde is maar dan opnieuw gelabeld. Het sleutelpunt is het kennen van ISO 8601, een gangbare datumnotatie-standaard. Ontwikkelaars moeten een balans zien te vinden tussen aangepaste formaten en gebruikerslocale-instellingen. Waarom? Omdat datums wereldwijd anders worden gelezen. Bijvoorbeeld, Amerikanen gebruiken "MM/dd/yyyy", terwijl veel Europese landen "dd/MM/yyyy" gebruiken.

Alternatieven? Zeker. Swift biedt `ISO8601DateFormatter` voor ISO 8601-datums, en `DateComponentsFormatter` voor tijdsduren in strings, zoals "42 minuten". Je kunt ook zelf aan de slag met `.formatted()` in Swift 5.5 en hoger:

```Swift
let formattedDate = date.formatted(.dateTime.year().month().day().hour().minute().second())
print(formattedDate) // Uitvoer is afhankelijk van je locale-instellingen
```

Let op: Aangepaste stringcreatie kan leiden tot lokalisatiehoofdpijn en foutgevoelige code. Houd je waar mogelijk aan formatters en standaarden.

## Zie Ook
- [Datumopmaak](https://developer.apple.com/documentation/foundation/dateformatter) - Apple's Documentatie over DateFormatter
