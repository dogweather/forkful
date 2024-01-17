---
title:                "Confronto tra due date"
html_title:           "Swift: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Comparare due date è un'operazione comune nella programmazione, che consente di determinare se una data è precedente, successiva o uguale a un'altra. I programmatori lo fanno per confrontare date di eventi, per ordinare cronologicamente gli elementi e per gestire programmi futuri.

## Come Fare:

```Swift
let today = Date() 
let eventDate = Date(timeIntervalSinceReferenceDate: 626598400) 
if today > eventDate { 
    print("L'evento è già passato.") 
} else if today < eventDate { 
    print("L'evento deve ancora arrivare.") 
} else { 
    print("L'evento è oggi!") 
}
```

Output: L'evento deve ancora arrivare.

```Swift
let date1 = Date(timeIntervalSinceReferenceDate: 100000) 
let date2 = Date(timeIntervalSinceReferenceDate: 200000) 
if date1.compare(date2) == .orderedAscending { 
    print("La prima data è precedente alla seconda.") 
}
```

Output: La prima data è precedente alla seconda.

## Approfondimento:

In passato, la comparazione di date era un compito meno semplice, ma grazie alle funzioni integrate di Swift come `Date()` e `compare()`, è diventato molto più facile.
Un'altra opzione per confrontare date è quella di utilizzare librerie esterne, come "DateTools" o "SwiftDate", che offrono funzionalità aggiuntive e maggiore flessibilità.
Il confronto di date si basa sui timestamp, che rappresentano il numero di secondi trascorsi da una data di riferimento (1 gennaio 2001, mezzanotte) a una data specifica. Questo numero può essere positivo (per date successive alla data di riferimento) o negativo (per date precedenti).

## Vedi Anche:

[Documentazione ufficiale Apple su Date](https://developer.apple.com/documentation/foundation/date)
[Libreria "DateTools" per Swift](https://github.com/MatthewYork/DateTools)
[Libreria "SwiftDate" per Swift](https://github.com/malcommac/SwiftDate)