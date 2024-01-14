---
title:    "Swift: Cálcolare una data nel futuro o nel passato"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in diversi contesti, come per la pianificazione di eventi o per il calcolo del tempo tra due date. In questo articolo impareremo come farlo utilizzando il linguaggio di programmazione Swift.

## Come Fare

In Swift, possiamo utilizzare la classe `Calendar` e il metodo `date(byAdding:value:to:)` per calcolare una data nel futuro o nel passato. Vediamo un esempio pratico:

```Swift
let calendar = Calendar.current
let currentDate = Date()

// Calcola la data tra 7 giorni
let sevenDaysFromNow = calendar.date(byAdding: .day, value: 7, to: currentDate)
print("Tra 7 giorni sarà il \(sevenDaysFromNow)")

// Calcola la data 3 mesi fa
let threeMonthsAgo = calendar.date(byAdding: .month, value: -3, to: currentDate)
print("Tre mesi fa era il \(threeMonthsAgo)")
```

Questo esempio utilizza la data corrente come punto di partenza e aggiunge o sottrae un determinato valore in base all'unità di tempo specificata (giorno, mese, anno, ecc.). Il risultato sarà una nuova istanza di `Date` con la data calcolata.

## Deep Dive

Nella classe `Calendar` ci sono molte opzioni disponibili per il metodo `date(byAdding:value:to:)` che possono essere utili per calcoli più complessi. Ad esempio, il parametro `value` può essere negativo per ottenere una data nel passato e possiamo utilizzare anche un'unità di tempo diversa da quelle presenti di default (come gli anni o le settimane).

Inoltre, possiamo anche utilizzare il metodo `date(from: DateComponents)` per costruire una data specifica a partire da diverse componenti, come il giorno, il mese, l'anno, ecc.

Vale la pena esplorare la documentazione ufficiale di Apple per avere un quadro completo delle opzioni disponibili per il calcolo di date in Swift.

## Vedi Anche

- [Documentazione ufficiale di Apple su Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Video tutorial su calcoli di date in Swift](https://www.youtube.com/watch?v=Hw1gEsyiBm4)
- [Altro esempio pratico su calcoli di date in Swift](https://www.hackingwithswift.com/example-code/system/how-to-convert-dates-and-times-to-a-string-using-dateformatter)