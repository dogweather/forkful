---
title:                "Swift: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date è un'operazione comune nella programmazione Swift che può aiutare a gestire le informazioni sul tempo in modo efficace. Può essere utile in casi come la pianificazione di eventi, la creazione di notifiche o la gestione delle scadenze.

## Come fare

Per confrontare due date in Swift, possiamo utilizzare il metodo `compare()` della classe `Date`. Questo metodo restituisce un oggetto di tipo `ComparisonResult` che può assumere 3 valori: `.orderedAscending`, `.orderedSame` o `.orderedDescending`, a seconda che la prima data sia più piccola, uguale o più grande della seconda data.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"

let date1 = dateFormatter.date(from: "15/06/2021")
let date2 = dateFormatter.date(from: "20/06/2021")

let comparison = date1!.compare(date2!)

if comparison == .orderedAscending {
    print("La prima data è più piccola della seconda")
} else if comparison == .orderedDescending {
    print("La prima data è più grande della seconda")
} else {
    print("Le due date sono uguali")
}
```

Output:

```
La prima data è più piccola della seconda
```

## Approfondimento

Nella programmazione, le date vengono memorizzate sotto forma di timestamp, che rappresenta il numero di secondi trascorsi dal 1 gennaio 1970. Questo timestamp viene convertito in data leggibile dall'utente utilizzando un formato specifico, come nel nostro esempio `dd/MM/yyyy`.

Una cosa importante da notare è che le date vengono confrontate in base al loro timestamp, quindi se due date hanno lo stesso giorno, ma orari diversi, il confronto potrebbe non essere accurato. Per evitare questo problema, è possibile utilizzare il metodo `startOfDay` per impostare gli orari a mezzanotte prima di confrontare le date.

## Vedi anche

- [Documentazione ufficiale di Swift su come confrontare date](https://developer.apple.com/documentation/foundation/date/1409708-compare)
- [Tutorial su come gestire le date in Swift](https://www.hackingwithswift.com/example-code/language/how-to-compare-dates)
- [Esempi pratici di confronto tra date in Swift](https://www.swiftbysundell.com/articles/working-with-dates-in-swift/)