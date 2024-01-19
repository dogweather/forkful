---
title:                "Ottenere la data corrente"
html_title:           "Java: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Ottenere la data corrente significa rilevare il momento esatto in cui un evento si verifica nel codice. I programmatori lo fanno per mantenere traccia di eventi temporali, per registrazioni di log, per generare un timestamp univoco e per molte altre ragioni.

## Come fare:
Ecco un esempio di codice su come utilizzare il linguaggio Swift per ottenere la data e l'ora correnti:

``` Swift
import Foundation

let adesso = Date()
print("Adesso:", adesso)
```

L'output del codice sarà simile a questo:

``` Swift
Adesso: 2022-03-22 12:23:45 +0000
```

## Approfondimento:
Ottenere la data corrente è un concetto antico come la programmazione stessa. Nel corso degli anni, sono state sviluppate varie tecniche e funzioni per ottenere la data e l'ora. Nel contesto di Swift, possiamo ottenere la data corrente attraverso l'oggetto `Date()`.

Un'altra alternativa per ottenere la data e l'ora correnti è l'utilizzo della classe `Calendar`. Questo metodo fornisce un maggiore controllo sulla manipolazione dei dati di date e ore.

Ecco un esempio di come utilizzare la classe `Calendar`:

``` Swift
import Foundation

let adesso = Calendar.current.dateComponents([.year, .month, .day, .hour, .minute, .second], from: Date())
print("Anno: \(adesso.year!), Mese: \(adesso.month!), Giorno: \(adesso.day!), Ora: \(adesso.hour!):\(adesso.minute!):\(adesso.second!)")
```

L'output del codice sarà simile a questo:

``` Swift
Anno: 2022, Mese: 3, Giorno: 22, Ora: 12:23:45
```

Swift utilizza l'orario universale coordinato (UTC) come formato di tempo predefinito.

## Vedi anche:
Eccoti dei collegamenti a articoli e guide correlate: 

1. [Guida alla Data e ora in Swift](https://www.hackingwithswift.com/articles/141/8-essential-swift-date-and-time-types)
2. [Documentazione Apple su Date](https://developer.apple.com/documentation/foundation/date)
3. [Documentazione Apple su Calendar](https://developer.apple.com/documentation/foundation/calendar)
4. [Il formato di date e ore ISO 8601](https://en.wikipedia.org/wiki/ISO_8601).