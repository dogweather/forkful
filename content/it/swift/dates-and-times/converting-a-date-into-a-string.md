---
title:                "Conversione di una data in una stringa"
aliases:
- /it/swift/converting-a-date-into-a-string/
date:                  2024-01-20T17:37:33.484879-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una data in una stringa"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Tradurre una data in una stringa vuol dire trasformarla in un formato leggibile per gli umani. I programmatori lo fanno per mostrare le date in un'app o per memorizzarle in un formato standardizzato.

## How to:
In Swift, usiamo `DateFormatter` per convertire le date in stringhe. Ecco un esempio:

```Swift
import Foundation

let adesso = Date()
let formatDatario = DateFormatter()
formatDatario.dateStyle = .short
formatDatario.timeStyle = .short

let dataStringa = formatDatario.string(from: adesso)
print(dataStringa) // Es: "12/3/23, 14:11"
```

Cambia `dateStyle` e `timeStyle` per formati diversi.

## Deep Dive
Swift gestisce la data e l'ora con la classe `Date`, ma per rappresentarla come una stringa usa `DateFormatter`. Prima di Swift, Objective-C usava `NSDate` e `NSDateFormatter`.

Gli alternative includono stringhe ISO 8601 con `ISO8601DateFormatter` o stampare le componenti della data manualmente. Oggi, `DateFormatter` ti permette di specificare il locale e il fuso orario per la massima precisione.

Come dettaglio di implementazione, usa sempre `DateFormatter` con prudenza perché è pesante in termini di risorse. Per performance migliori, riutilizza l'istanza del formatter quando possibile.

## See Also
- [Apple Developer DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
