---
title:                "Swift: Confrontare due date"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
C'è spesso la necessità di confrontare due date durante la programmazione di un'applicazione. Comparare due date può essere utile per determinare eventi futuri, verificare se un'azione è stata eseguita entro una scadenza o semplicemente per visualizzare le informazioni in un formato facilmente leggibile. In questo articolo, impareremo come confrontare facilmente due date utilizzando il linguaggio di programmazione Swift.

## Come Fare
Per confrontare due date in Swift, è necessario convertirle in oggetti del tipo `Date` e poi utilizzare il metodo `compare` per determinare la loro relazione. Ecco un esempio di codice che confronta due date e stampa il risultato:

```Swift
let dateFormatter = DateFormatter()

dateFormatter.dateFormat = "dd/MM/yyyy"
let firstDate = dateFormatter.date(from: "15/01/2021")
let secondDate = dateFormatter.date(from: "20/01/2021")

if let first = firstDate, let second = secondDate {
    let comparisonResult = first.compare(second)
    print(comparisonResult.rawValue) // stampa -1 perché la prima data è prima della seconda
}
```

Nell'esempio sopra, abbiamo utilizzato un `DateFormatter` per convertire due stringhe in oggetti `Date`. Successivamente, abbiamo utilizzato il metodo `compare` per confrontare le due date e abbiamo stampato il risultato utilizzando il valore restituito `rawValue`, dove -1 indica che la prima data è prima della seconda.

## Approfondimento
Il metodo `compare` restituisce tre possibili valori:

- `.orderedAscending` se la prima data è antecedente alla seconda data
- `.orderedDescending` se la prima data è successiva alla seconda data
- `.orderedSame` se le due date sono uguali

Inoltre, è possibile utilizzare il modificatore `operator` per confrontare direttamente due date senza dover utilizzare il metodo `compare`. Ad esempio:

```Swift
let firstDate = Date() // data corrente
let secondDate = Date(timeInterval: 3600, since: firstDate) // aggiungiamo un'ora alla prima data

if firstDate > secondDate {
    print("La prima data è successiva alla seconda")
} else {
    print("La prima data è precedente alla seconda") // viene stampato questo output
}
```

## Vedi Anche
- [Documentazione ufficiale di Apple su Date](https://developer.apple.com/documentation/foundation/date)
- [Tutorial su come utilizzare Date in Swift](https://www.hackingwithswift.com/read/15/3/working-with-dates)
- [Confrontare date e tempi in Swift](https://www.codingexplorer.com/swiftly-sorting-dates-and-times/)