---
title:    "Swift: Confrontare due date"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
Comparare due date è un'attività comune nella programmazione, soprattutto quando si lavora con dati temporali come ad esempio date di scadenza o di creazione. Imparare a confrontare due date può aiutare a organizzare e analizzare meglio i dati, rendendo il codice più efficiente e accurato.

## Come Fare
Per confrontare due date in Swift, è necessario utilizzare l'operatore "==" che controlla se due date sono uguali. Per esempio, se si vuole confrontare due date di tipo Date:

```Swift
let firstDate = Date()
let secondDate = Date()

if firstDate == secondDate {
    print("Le date sono uguali!")
} else {
    print("Le date sono diverse.")
}
```
Output: Le date sono uguali!

Possiamo anche utilizzare l'operatore ">" o "<" per confrontare due date e determinare quale è successiva o precedente. Per esempio:

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600)

if date1 > date2 {
    print("La prima data è successiva alla seconda.")
} else if date1 < date2 {
    print("La prima data è precedente alla seconda.")
} else {
    print("Le date sono uguali.")
}
```
Output: La prima data è precedente alla seconda.

## Approfondimento
Se si vuole confrontare due date più in dettaglio, è possibile utilizzare il metodo "compare()" che restituisce una enum con tre possibili valori: OrderedAscending, OrderedSame, OrderedDescending. Grazie a questa enum, si può facilmente determinare quale data è successiva o precedente all'altra. Per esempio:

```Swift
let date3 = Date()
let date4 = Date(timeIntervalSinceNow: 7200)

let comparison = date3.compare(date4)
switch comparison {
case .orderedAscending:
    print("La prima data è successiva alla seconda.")
case .orderedSame:
    print("Le date sono uguali.")
case .orderedDescending:
    print("La prima data è precedente alla seconda.")
}
```
Output: La prima data è precedente alla seconda.

## Vedi Anche
- [Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Hacking with Swift - How to compare dates](https://www.hackingwithswift.com/example-code/system/how-to-compare-dates)
- [Swift by Sundell - Comparing dates in Swift](https://www.swiftbysundell.com/articles/comparing-dates-in-swift/)