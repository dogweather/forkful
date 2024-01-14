---
title:    "Swift: Confrontare due date"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

La comparazione di due date è fondamentale per molte applicazioni, come ad esempio per monitorare eventi o per gestire le prenotazioni. Imparare a confrontare due date in Swift può essere utile per creare un software più robusto e performante.

## Come fare

Per comparare due date in Swift, dobbiamo utilizzare il metodo `compare` dell'oggetto `Date`. Questo metodo restituisce un `ComparisonResult` che può assumere tre valori: `.orderedDescending` se la prima data è successiva alla seconda, `.orderedAscending` se la seconda data è successiva alla prima, e `.orderedSame` se le due date sono uguali.
Ecco un esempio di codice per confrontare due date:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"

let primaData = dateFormatter.date(from: "01/01/2020")!
let secondaData = dateFormatter.date(from: "15/01/2020")!

let risultato = primaData.compare(secondaData)

switch risultato {
    case .orderedDescending:
        print("La prima data è successiva alla seconda")
    case .orderedAscending:
        print("La seconda data è successiva alla prima")
    case .orderedSame:
        print("Le due date sono uguali")
}
```

L'output di questo codice sarà "La seconda data è successiva alla prima".

## Approfondimento

Per una comparazione più precisa delle date, è possibile specificare un `Calendar` nel metodo `compare`. Inoltre, è importante tenere presente che le date possono essere influenzate da fusi orari diversi, quindi è necessario considerare questa possibilità nel nostro codice. Per ulteriori informazioni su come gestire i fusi orari e le date in Swift, consulta la documentazione ufficiale.

## Vedi anche
- [Documentation - Comparing Dates in Swift](https://developer.apple.com/documentation/foundation/date/1407748-compare)
- [Ray Wenderlich - Working with Date and Time in Swift](https://www.raywenderlich.com/3058154-working-with-date-and-time-in-swift)
- [Hacking with Swift - How to compare dates](https://www.hackingwithswift.com/example-code/system/how-to-compare-dates)