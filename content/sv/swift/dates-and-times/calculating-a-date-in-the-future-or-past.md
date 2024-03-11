---
date: 2024-01-20 17:32:09.106086-07:00
description: "Ber\xE4kning av framtida eller f\xF6rflutna datum \xE4r en process att\
  \ l\xE4gga till eller dra fr\xE5n ett givet datum. Programmerare g\xF6r detta f\xF6\
  r att hantera\u2026"
lastmod: '2024-03-11T00:14:11.661083-06:00'
model: gpt-4-1106-preview
summary: "Ber\xE4kning av framtida eller f\xF6rflutna datum \xE4r en process att l\xE4\
  gga till eller dra fr\xE5n ett givet datum. Programmerare g\xF6r detta f\xF6r att\
  \ hantera\u2026"
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkning av framtida eller förflutna datum är en process att lägga till eller dra från ett givet datum. Programmerare gör detta för att hantera deadlines, agendor eller tidsbaserade funktioner i appar.

## Så här gör du:
```Swift
import Foundation

// Nuvarande datum
let currentDate = Date()

// Datumformaterare för output
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"

// Beräkna framtida datum (5 dagar framåt)
var dateComponent = DateComponents()
dateComponent.day = 5
if let futureDate = Calendar.current.date(byAdding: dateComponent, to: currentDate) {
    print("Framtida datum: \(dateFormatter.string(from: futureDate))")
}

// Beräkna förflutet datum (10 dagar bakåt)
dateComponent.day = -10
if let pastDate = Calendar.current.date(byAdding: dateComponent, to: currentDate) {
    print("Förflutet datum: \(dateFormatter.string(from: pastDate))")
}
```
Sample Output:
```
Framtida datum: 2023-04-10
Förflutet datum: 2023-03-26
```

## Djupdykning
I början av dataprogrammering var datumhantering ett komplext problem på grund av olika tidszoner och datumformat. Swift använder `Date` och `Calendar` för att hantera detta, göra det enklare för utvecklare att jobba med datum. Alternativ inkluderar tredjepartsbibliotek som `DateTools` och `SwiftDate`, men `Foundation`-ramverket täcker de flesta behoven. Att räkna ut datum handlar mycket om att förstå `DateComponents` och `Calendar`-klasserna, där man definierar vilka komponenter (år, månad, dag etc.) man vill lägga till eller ta bort för att nå önskat datum. Noggrannhet är viktig för att hantera skottår och andra komplexiteter.

## Se även
- Apple Developer Documentation for [Date](https://developer.apple.com/documentation/foundation/date)
- Apple Developer Documentation for [Calendar](https://developer.apple.com/documentation/foundation/calendar)
