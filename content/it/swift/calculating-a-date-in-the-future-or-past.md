---
title:                "Swift: Calcolando una data nel futuro o nel passato"
simple_title:         "Calcolando una data nel futuro o nel passato"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in molti scenari di programmazione. Ad esempio, potresti voler creare un'app che tenga traccia delle scadenze o delle date di consegna, oppure potresti dover calcolare l'età di una persona in una determinata data.

## Come fare

Per calcolare una data nel futuro o nel passato, dobbiamo prima definire una data di partenza. Possiamo fare ciò utilizzando l'oggetto `Date` di Swift e impostando la data desiderata utilizzando il suo inizializzatore. Ad esempio, se vogliamo calcolare una data nel futuro, possiamo utilizzare il metodo `addingTimeInterval` per aggiungere un determinato intervallo di tempo alla data di partenza.

```Swift
let startDate = Date() // abbiamo impostato la data di partenza ad oggi
let futureDate = startDate.addingTimeInterval(86400) // 86400 secondi = 1 giorno
print(futureDate) // output: 2022-01-16 12:00:00 +0000
```

Invece, se vogliamo calcolare una data nel passato, possiamo utilizzare il metodo `addingTimeInterval` con un valore negativo, indicando quanto tempo vogliamo sottrarre alla data di partenza.

```Swift
let startDate = Date() // abbiamo impostato la data di partenza ad oggi
let pastDate = startDate.addingTimeInterval(-604800) // 604800 secondi = 1 settimana
print(pastDate) // output: 2022-01-09 12:00:00 +0000
```

## Approfondimento

Per calcolare una data più specifica nel futuro o nel passato, possiamo utilizzare il metodo `date(byAdding:to:wrappingComponents:)` della classe `Calendar` di Swift. Questo metodo ci consente di aggiungere diverse componenti di tempo, come anni, mesi, giorni, ore, minuti e secondi, alla data di partenza. Inoltre, possiamo specificare se vogliamo che la data sia considerata nel calendario gregoriano o in un altro calendario specifico.

```Swift
let startDate = Date() // abbiamo impostato la data di partenza ad oggi
let futureDate = Calendar.current.date(byAdding: .day, value: 7, to: startDate) // aggiungiamo 7 giorni alla data di partenza
print(futureDate) // output: 2022-01-23 12:00:00 +0000
```

## Vedi anche

- [Documentazione di Apple su Date](https://developer.apple.com/documentation/foundation/date)
- [Video tutorial su Date in Swift by Code With Chris](https://www.youtube.com/watch?v=SWLu89wXrZg)
- [Articolo su come calcolare le differenze tra date in Swift by AppCoda](https://www.appcoda.com/swift-date-difference/)