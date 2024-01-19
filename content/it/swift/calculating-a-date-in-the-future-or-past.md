---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Swift: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Calcolare una data nel futuro o passato significa determinare una data specifica a partire da un dato giorno, aggiungendo o sottraendo un detto numero di giorni, settimane, mesi o anni. Questa funzionalità è utile ai programmatori per gestire le scadenze, i promemoria e i calendari.

## Come fare:
In Swift, è possibile modificare le date usando le funzioni `addingTimeInterval()`, `byAdding()` o `advanced(by:)`. Qui ci sono esempi di come usarle.

```Swift
import Foundation

// Creazione di una data di partenza - ad esempio "oggi"
let oggi = Date()

// Aggiunta di 5 giorni con addingTimeInterval()
let cinqueGiorniInPiù = oggi.addingTimeInterval(5 * 24 * 60 * 60)

// Sottrazione di una settimana con byAdding()
let unaSettimanaFa = Calendar.current.date(byAdding: .weekOfYear, value: -1, to: oggi)!

// Aggiunta di 2 anni con advanced(by:)
let dueAnniFa = Calendar.current.date(byAdding: .year, value: 2, to: oggi)!
```
Output:
```
Oggi: 2021-09-20 20:28:09 +0000
Tra 5 giorni : 2021-09-25 20:28:09 +0000
Una settimana fa: 2021-09-13 20:28:09 +0000
Tra 2 anni: 2023-09-20 20:28:09 +0000
```


## Approfondimento
Calculare una data in passato o futuro significa spostarsi avanti e indietro nel tempo, che è fondamentale per molte app. Historicamente, queste operazioni erano fatte manualmente con le funzioni matematiche nelle prime lingue di programmazione.

In Swift, la classe `Date` presenta strumenti che semplificano queste operazioni. Questa classe misura i punti specifici nel tempo indipendentemente dal fuso orario, rendendo facile contare i secondi tra le date o creare un oggetto `Date` da un determinato numero di secondi.

Un'altra alternativa nella gestione delle date è l'uso delle librerie di terze parti come `DateHelper` e `Timepiece`. Queste offrono ulteriori funzionalità e una sintassi più leggibile per manipolare le date.

## Fonti utili
1. [Apple Developer Documentation: This is Date](https://developer.apple.com/documentation/foundation/date)
2. [Spostarsi attraverso il tempo con Swift Date e Time](https://www.raywenderlich.com/5817-encoding-decoding-and-serialization-in-swift-4)
3. [DateHelper su GitHub](https://github.com/melvitax/DateHelper)
4. [Timepiece su GitHub](https://github.com/naoty/Timepiece)