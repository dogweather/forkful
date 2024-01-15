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

## Perché
Calcolare una data futura o passata può risultare utile per molte applicazioni, come la gestione delle scadenze o la programmazione di eventi futuri.

## Come fare
Per calcolare una data in Swift, è necessario utilizzare il framework `Foundation` e la classe `Calendar` per gestire le date e il fuso orario. Ecco un esempio di come ottenere la data odierna utilizzando il calendario gregoriano:

```Swift
let calendar = Calendar(identifier: .gregorian)
let currentDate = Date()
let currentComponents = calendar.dateComponents([.day, .month, .year], from: currentDate)
```

Per calcolare una data futura o passata, è possibile utilizzare il metodo `date(byAdding:to:wrappingComponents:)` della classe `Calendar`. Questo metodo accetta come parametri gli elementi da aggiungere o sottrarre alla data di partenza e restituisce la data risultato. Ad esempio, per ottenere la data a 30 giorni dall'oggi:

```Swift
let futureDate = calendar.date(byAdding: .day, value: 30, to: currentDate)
```

Per calcolare una data con un determinato fuso orario, è necessario utilizzare la classe `TimeZone` e specificarla nell'istanza del calendario:

```Swift
let losAngelesTimeZone = TimeZone(identifier: "America/Los_Angeles")
calendar.timeZone = losAngelesTimeZone
```

### Esempio di output

```
currentDate = 2021-10-25 08:00:00 +0000
futureDate = 2021-11-24 08:00:00 +0000
```

## Approfondimento
Il framework `Foundation` offre una vasta gamma di opzioni per calcolare date in base a diversi calendari e fusi orari. È anche possibile utilizzare la classe `DateInterval` per ottenere un intervallo di date, il metodo `isDate(_:equalTo:toGranularity:)` per confrontare due date e il metodo `dateComponents(_:from:to:)` per ottenere il numero di elementi (come giorni, mesi o anni) tra due date.

## Vedi anche
- [Documentazione ufficiale di Apple su Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Guida di Hacking with Swift su come gestire le date in Swift](https://www.hackingwithswift.com/articles/153/how-to-handle-date-and-time-in-swift)