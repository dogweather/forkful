---
title:                "Swift: Calcolare una data nel futuro o nel passato"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere estremamente utile in diverse situazioni, come ad esempio per gestire scadenze o per pianificare eventi importanti.

## Come fare

Per calcolare una data nel futuro o nel passato in Swift, possiamo utilizzare la classe `Calendar` e il metodo `date(byAdding:value:to:)`. Ad esempio, per ottenere la data di oggi più 7 giorni, possiamo scrivere il seguente codice:

```Swift
let today = Date()

let calendar = Calendar.current
let futureDate = calendar.date(byAdding: .day, value: 7, to: today)

print(futureDate) // Output: 2021-09-20 19:02:53 +0000
```

In questo esempio, abbiamo utilizzato `.day` come componente di data e 7 come valore, indicando così che vogliamo aggiungere 7 giorni alla data di oggi. Possiamo utilizzare lo stesso metodo per calcolare una data nel passato, specificando un valore negativo.

## Approfondimento

Il metodo `date(byAdding:value:to:)` ci permette di aggiungere o sottrarre una qualsiasi unità di tempo, come giorni, mesi, anni, ore, minuti, secondi, ecc. per ottenere una nuova data. Inoltre, possiamo anche specificare una data di riferimento diversa da quella odierna, passando al metodo un'altra data come parametro `to`.

Un'altra opzione per calcolare date nel futuro o nel passato è utilizzare `TimeInterval`, che rappresenta una quantità di tempo in secondi e può essere aggiunta o sottratta da una data. Ad esempio:

```Swift
let today = Date()

let futureDate = today + (7 * 24 * 60 * 60) // Aggiungiamo 7 giorni in secondi

print(futureDate) // Output: 2021-09-20 19:02:53 +0000
```

## Vedi anche

- [La classe `Calendar` in Swift](https://developer.apple.com/documentation/foundation/calendar)
- [Il tipo `Date` in Swift](https://developer.apple.com/documentation/foundation/date)
- [La struttura `DateComponents` in Swift](https://developer.apple.com/documentation/foundation/datecomponents)