---
title:                "Organizzare il codice in funzioni"
aliases: - /it/swift/organizing-code-into-functions.md
date:                  2024-01-26T01:16:11.258966-07:00
model:                 gpt-4-0125-preview
simple_title:         "Organizzare il codice in funzioni"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Raggruppare il codice in funzioni significa scomporre compiti in blocchi riutilizzabili. Ciò rende il codice più pulito, meno incline a errori e più facile da debuggare o rifattorizzare.

## Come fare:
Immagina un compito: calcolare la media di un array. Senza funzioni, inseriresti tutto nel main. Con le funzioni, faresti così:

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// Utilizzo
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("La media dei punteggi è \(averageScore)")
```

L'output di esempio sarebbe:
```
La media dei punteggi è 87.6875
```

## Approfondimento
Storicamente, con l'aumentare della complessità della programmazione, le funzioni sono diventate una pietra angolare per la gestione della complessità. Le alternative includono la codifica in linea e il copia-incolla del codice (codice spaghetti), ora considerate prevalentemente cattive pratiche. In Swift, le funzioni sono cittadini di prima classe; possono essere assegnate a variabili, passate come argomenti e ritornate da altre funzioni, rendendo il codice più modulare e flessibile.

In termini di implementazione, progetta le tue funzioni per fare bene una cosa sola. Punta a funzioni con uno scopo chiaro e un nome che lo rifletti. Attenzione al numero di parametri: se sono troppi, probabilmente stai facendo troppo. Gestione degli errori? Considera l'uso di funzioni con throw e gestisci i problemi con grazia. Ricorda: Swift punta tutto sulla leggibilità e sulla facilità di manutenzione.

## Vedi Anche
- [Guida al Linguaggio di Programmazione Swift - Funzioni](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Guida allo Stile di Swift di Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
- [Refactoring: Migliorare il design del codice esistente di Martin Fowler](https://martinfowler.com/books/refactoring.html)
