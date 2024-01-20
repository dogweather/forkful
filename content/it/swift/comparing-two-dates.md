---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Quando e Perché Confrontare Due Date?

Il confronto tra due date consiste nel determinare quale data risulta essere prima o successiva, o se sono le stesse. Questa operazione è molto frequente nei programmi: i sistemi di prenotazione, ad esempio, confrontano le date per verificare la disponibilità.

# Come Fare:

In Swift, possiamo confrontare due date utilizzando gli operatori di confronto: `<`, `>` e `==`. Di seguito un esempio di codice:

```Swift
import Foundation

let primaData = Date()
let secondaData = Date(timeIntervalSinceNow: 3600) // 1 ora avanti

if primaData < secondaData {
    print("La prima data è prima della seconda data")
} else if primaData == secondaData {
    print("Le date sono le stesse")
} else {
    print("La seconda data è prima della prima data")
}
```

Questo fornirà l'output: "La prima data è prima della seconda data".

# Approfondimento

Sebbene il confronto diretto delle date in Swift sia piuttosto semplice grazie agli operatori di confronto standard, non è sempre stato così. In effetti, nelle versioni precedenti di Swift, il confronto delle date richiedeva l'uso di metodi specializzati come `earlierDate(_:)` e `laterDate(_:)` di `NSDate`. 

Ci sono diverse alternative per confrontare le date in Swift, come l'uso di `compare(_ :)` di `Date`, che restituisce un `ComparisonResult`, o `timeIntervalSince(_ :)`, che restituisce la differenza di tempo tra due date come un `TimeInterval`.

Dettagli importanti sulla comparazione di date in Swift: la classe `Date` in Swift rappresenta un punto singolo nel tempo, indipendentemente dal fuso orario. Di conseguenza, quando confronti le date, stai confrontando i momenti assoluti, non i momenti locali.

# Vedi Anche

Se desideri approfondire la gestione delle date in Swift, potresti trovare utili le seguenti risorse:

- [Documentazione Apple ufficiale su Date](https://developer.apple.com/documentation/foundation/date)