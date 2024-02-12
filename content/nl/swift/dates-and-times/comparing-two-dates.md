---
title:                "Twee datums vergelijken"
aliases: - /nl/swift/comparing-two-dates.md
date:                  2024-01-28T21:56:49.517361-07:00
model:                 gpt-4-0125-preview
simple_title:         "Twee datums vergelijken"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Twee datums vergelijken is als vragen, "Wie was er eerst, de kip of het ei?" maar dan met kalenderdatums. Programmeurs doen dit om evenementen te sorteren, acties te triggeren en periodes te beoordelen.

## Hoe doe je dat:
Swift gebruikt het `Date` type voor datum en tijd. Hier is een simpele benadering voor het vergelijken van twee datums:

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"

// Twee datumobjecten aanmaken
let date1 = dateFormatter.date(from: "2023/01/01 09:00")!
let date2 = dateFormatter.date(from: "2023/02/01 10:00")!

// Data vergelijken
if date1 == date2 {
    print("Data zijn hetzelfde")
} else if date1 < date2 {
    print("Datum1 is eerder dan Datum2")
} else {
    print("Datum1 is later dan Datum2")
}
```

Voorbeelduitvoer:

`Datum1 is eerder dan Datum2`

Vergelijkingsoperators kunnen worden gebruikt omdat `Date` voldoet aan het `Comparable` protocol.

## Diepere Duik:
Datums kwamen niet altijd in handige objecten. Oorspronkelijk moest je afzonderlijke componenten zoals jaar, maand en dag wringen. Veel erger. Nu, `Date` objecten in Swift handelen het zware werk en ze vergelijken is eenvoudig met ingebouwde operatoren.

Voor Swift en Cocoa's `Date`, gebruikte Objective-C `NSDate`, maar deze zijn te overbruggen, dus oude code kan nog steeds goed samenwerken.

En hé, niet alleen `<`, `>`, en `==` — je kunt ook `timeIntervalSince(_:)` gebruiken voor meer gedetailleerde controle, zoals:

```Swift
let timeInterval = date2.timeIntervalSince(date1)
```

Dit geeft je het verschil in seconden. Positieve waarde: datum2 ligt voor; negatieve: het ligt achter; nul: ze zijn identiek. Super handig voor timers, aftellingen en het bijhouden van tijdsduren. Onder de motorkap zijn datums gewoon referentiepunten in tijd—denk aan ze als chique tijdstempels.

## Zie Ook:
- Apple's Date documentatie: [https://developer.apple.com/documentation/foundation/date](https://developer.apple.com/documentation/foundation/date)
- Date Formatting Guide: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html)
