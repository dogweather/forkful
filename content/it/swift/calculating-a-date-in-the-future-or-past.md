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

## Cosa & perché?
Calcolare una data nel futuro o nel passato si riferisce all'abilità di un programmatore di determinare una data che sia un certo numero di giorni, settimane o mesi avanti o indietro rispetto a una data di riferimento fornita. I programmatori spesso fanno questo per svolgere operazioni di data, come creare un calendario o calcolare la scadenza di un progetto.

## Come:
```
Swift let currentDate = Date() let dateInFuture = Calendar.current.date(byAdding: .day, value: 7, to: currentDate) print(dateInFuture) // Output: 2021-11-05 13:45:23 +0000
```

Nell'esempio sopra, utilizziamo la funzione `date(byAdding:to:)` per calcolare una data nel futuro aggiungendo 7 giorni alla data corrente. È possibile utilizzare anche altri componenti di tempo, come le settimane o i mesi, e specificare una data di riferimento diversa dalla data corrente.

## Approfondimento:
Calcolare le date nel futuro o nel passato è utile quando si lavora con programmi che dipendono dal tempo, come calendari o promemoria. In passato, questa operazione era molto più complicata e richiedeva la conoscenza di algoritmi complessi. Oggi, con l'aiuto delle funzioni integrate di Swift e del framework `Foundation`, siamo in grado di calcolare facilmente le date in modo più efficiente.

## Vedi anche:
- [Funzione `date(byAdding:to:)` di Apple](https://developer.apple.com/documentation/foundation/calendar/2294041-date)
- [Calendario gregoriano](https://it.wikipedia.org/wiki/Calendario_gregoriano)