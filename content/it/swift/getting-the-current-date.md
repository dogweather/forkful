---
title:                "Swift: Ottenere la data attuale"
simple_title:         "Ottenere la data attuale"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Ci sono diverse ragioni per cui un programmatore potrebbe voler ottenere la data corrente nel suo codice Swift. Può essere utile per tenere traccia dei tempi di esecuzione dei programmi, per creare log di eventi o per fornire informazioni temporali ai propri utenti.

## Come fare

Per ottenere la data corrente in Swift, è possibile utilizzare l'oggetto `Date`. Possiamo creare un'istanza di questa classe utilizzando il costruttore di default, che ci darà la data e l'ora attuali nel nostro fuso orario locale. Ad esempio:

```Swift
let currentDate = Date()
print(currentDate)
```

Questo produrrà un output simile a questo: `2021-06-15 16:37:13 +0000`. Tieni presente che l'ora mostrata sarà diversa a seconda del tuo fuso orario.

Possiamo anche impostare un fuso orario diverso utilizzando l'oggetto `TimeZone`, ad esempio:

```Swift
let usTimeZone = TimeZone(identifier: "America/New_York")
let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.timeZone = usTimeZone
dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
print(dateFormatter.string(from: currentDate))
```

Questo produrrà un output simile a questo: `2021-06-15 12:37:13`.

## Approfondimento

Il motivo per cui abbiamo usato `dateFormatter` nel secondo esempio è perché l'oggetto `Date` non ha una rappresentazione visiva predefinita in Swift. Puoi usare un oggetto `DateFormatter` per formattare la stringa in un formato più leggibile. Inoltre, puoi utilizzare il metodo `DateFormatter.string(from: Date)` per ottenere la stringa formattata dalla data.
 
Vale la pena notare che l'oggetto `Date` rappresenta solo la data e l'ora in un determinato istante. Se vuoi lavorare con date specifiche, come il tuo compleanno o una data di scadenza, dovresti utilizzare l'oggetto `Calendar` e i suoi metodi per creare date e confrontarle tra loro.

## Vedi anche

- [Documentazione Apple su Date](https://developer.apple.com/documentation/foundation/date)
- [Documentazione Apple su DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Tutorial su come lavorare con date in Swift](https://betterprogramming.pub/how-to-work-with-dates-in-swift-8dafca19a1ca)