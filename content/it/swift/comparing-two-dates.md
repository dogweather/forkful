---
title:                "Confronto di due date"
html_title:           "Swift: Confronto di due date"
simple_title:         "Confronto di due date"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Swift, sicuramente ti sarà capitato di dover confrontare due date in diversi scenari, come ad esempio quando si lavora con dati temporali o si desidera implementare una funzione per ordinare gli eventi in base alla data. In questo articolo vedremo come confrontare due date utilizzando il linguaggio Swift e quali sono i principi fondamentali da conoscere.

## Come fare

Per confrontare due date in Swift, è possibile utilizzare il metodo `compare` della classe `Date`. Questo metodo confronta due date e restituisce un valore `ComparisonResult`, che può essere `orderedAscending` (se la prima data è precedente alla seconda), `orderedSame` (se le date sono uguali) o `orderedDescending` (se la prima data è successiva alla seconda).

Per esempio:

```Swift
let data1 = Date()
let data2 = Date(timeIntervalSinceNow: 3600) // aggiunge un'ora alla data corrente
let result = data1.compare(data2) // restituisce .orderedAscending
```

È anche possibile utilizzare l'operatore `==` per confrontare direttamente due date e il metodo `isLess(than:)` per verificare se una data è precedente a un'altra.

```Swift
let data1 = Date()
let data2 = Date(timeIntervalSinceNow: 3600)
if data1 == data2 {
    print("Le date sono uguali!")
} else if data1.isLess(than: data2) {
    print("La data 1 è precedente alla data 2.")
} else {
    print("La data 2 è precedente alla data 1.")
}
```

## Approfondimento

Esistono diverse considerazioni da fare quando si confrontano due date in Swift. In primo luogo, è importante ricordare che le date possono avere una precisione diversa a seconda della loro rappresentazione, quindi un confronto diretto potrebbe non essere preciso al centesimo di secondo.

Inoltre, quando si lavora con date in diversi fusi orari, è consigliato utilizzare il tipo `DateComponents` per estrarre le informazioni necessarie (come anno, mese, giorno, orario) e confrontarle invece che utilizzare l'oggetto `Date` nel suo complesso.

Infine, è importante tenere presente che le date sono oggetti che rappresentano un momento specifico nel tempo e non possono essere utilizzate per effettuare calcoli matematici come l'aggiunta o la sottrazione di date. Per questo, è consigliato utilizzare il tipo `TimeInterval` per eseguire operazioni temporali.

## Vedi anche

- [Documentazione ufficiale di Swift per la classe `Date`](https://developer.apple.com/documentation/foundation/date)
- [Come confrontare due date in Java](https://www.baeldung.com/java-compare-dates)