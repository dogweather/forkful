---
title:                "Estrazione di una data da una stringa"
html_title:           "Swift: Estrazione di una data da una stringa"
simple_title:         "Estrazione di una data da una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Il parsing di una data da una stringa è il processo di estrarre una data da una stringa di testo. I programmatori spesso lo fanno quando devono manipolare date all'interno dei loro codici.

## Come fare:
Il parsing di una data da una stringa può essere fatto utilizzando la classe `DateFormatter` di Swift. Di seguito un esempio di codice che mostra come convertire una stringa in una data:
```Swift
let dateString = "10/23/2021"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "MM/dd/yyyy"
let date = dateFormatter.date(from: dateString)
print(date) // Output: 2021-10-23 00:00:00 +0000
```

## Approfondimento:
Il parsing di una data da una stringa ha una lunga storia nel mondo della programmazione. In passato, era spesso un processo complesso che richiedeva l'utilizzo di librerie aggiuntive. Tuttavia, con l'avvento di Swift, la classe `DateFormatter` semplifica notevolmente questo processo. Un altro modo per fare il parsing di una data da una stringa è utilizzando l'operatore di casting `as Date?`.

## Vedi anche:
- La documentazione ufficiale di Apple su come fare il parsing di una data: https://developer.apple.com/documentation/foundation/dateformatter
- Un articolo di Ray Wenderlich sulla manipolazione delle date in Swift: https://www.raywenderlich.com/9079191-dateformatter-tutorial-for-ios-getting-started