---
title:                "Calcolo di una data futura o passata"
aliases:
- /it/swift/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:55.088701-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcolo di una data futura o passata"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Calcolare una data futura o passata significa determinare una data specifica che sia a una certa distanza temporale da oggi o da una data di partenza. I programmatori fanno ciò per gestire scadenze, eventi, promemoria e per tracciare periodi in app e sistemi.

## Come fare:
Swift offre `Date`, `Calendar`, e `DateComponents` per lavorare con le date. Ecco come si fa:
```Swift
import Foundation

// Date attuale
let oggi = Date()

// Calcolo di una data futura (5 giorni dopo)
var dataFutura = Calendar.current.date(byAdding: .day, value: 5, to: oggi)!

// Calcolo di una data passata (5 giorni fa)
var dataPassata = Calendar.current.date(byAdding: .day, value: -5, to: oggi)!

// Formattazione e stampa
let dateFormatter = DateFormatter()
dateFormatter.dateStyle = .medium
print("Data futura: \(dateFormatter.string(from: dataFutura))")
print("Data passata: \(dateFormatter.string(from: dataPassata))")
```
Output potrebbe essere:
```
Data futura: Feb 18, 2023
Data passata: Feb 8, 2023
```

## Approfondimento
Inizialmente i calcoli di data e ora erano complessi, si basavano su operazioni manuali e algoritmi personalizzati. Ora, Swift gestisce il calendario e i fusi orari; quindi, le operazioni sono più sicure. Alternative includevano librerie come `DateTools` e `SwiftDate`, ma Swift standard ha chiuso il divario. Durante l'implementazione, considera le regole del calendario (gregoriano, ecc.), i fusi orari e la localizzazione. Usa `DateComponents` per precisione e `TimeInterval` per operazioni più semplici.

## Vedi Anche
- Documentazione di Swift sul lavoro con date e calendari: [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/calendar)
- Guida di NSHipster su `Date` e `Calendar`: [NSHipster Date & Calendar](https://nshipster.com/datecomponents/)
- Sito di Ray Wenderlich su manipolazione delle date in Swift: [Ray Wenderlich Date Manipulation](https://www.raywenderlich.com/1178-date-and-time-manipulation-in-swift)
