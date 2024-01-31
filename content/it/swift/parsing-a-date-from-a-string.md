---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:38:30.448728-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"

category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Analizzare una data significa trasformarla da stringa a tipo `Date` per manipolarla a piacere. Facciamo ciò per confrontare date, calcolare intervalli temporali o semplicemente per visualizzarle in formati diversi.

## How to:
Il `DateFormatter` in Swift rende il parsing un gioco da ragazzi. Ecco un esempio:

```Swift
import Foundation

let dateString = "22-03-2023"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy"
if let date = dateFormatter.date(from: dateString) {
    print("La data è \(date)")
} else {
    print("C'è stato un errore nel parsing della data.")
}
// Output: La data è 2023-03-22 00:00:00 +0000
```

Cambia il `dateFormat` per adattarlo al formato della tua stringa.

## Deep Dive
Prima dell'introduzione del `DateFormatter` di Swift, il parsing delle date era un'impresa più complicata. Adesso è integrato e uniforma il trattamento delle date a livello mondiale. Non ignorare la configurazione del fuso orario (`timeZone`) e della località (`locale`) per ottenere risultati accurati. Alternativamente, puoi anche usare il framework `ISO8601DateFormatter` per lavorare con date in formato ISO 8601, che può essere più adatto a stringhe date con un ora e timezone.

```Swift
let isoDateString = "2023-03-22T15:40:00Z"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: isoDateString) {
    print("La data ISO convertita è \(date)")
} else {
    print("Errore nel parsing della data ISO.")
}
// Output: La data ISO convertita è 2023-03-22 15:40:00 +0000
```
Ricordati che il parsing può fallire: sempre controlla il risultato.

## See Also
1. Apple Developer Documentation – DateFormatter: [https://developer.apple.com/documentation/foundation/dateformatter](https://developer.apple.com/documentation/foundation/dateformatter)
2. Apple Developer Documentation – ISO8601DateFormatter: [https://developer.apple.com/documentation/foundation/iso8601dateformatter](https://developer.apple.com/documentation/foundation/iso8601dateformatter)
3. Swift Date Formatting – [https://nsscreencast.com/episodes/367-swift-dates](https://nsscreencast.com/episodes/367-swift-dates)
