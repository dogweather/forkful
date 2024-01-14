---
title:                "Swift: Trasformare una data in una stringa"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Molte volte, quando si lavora con date in un programma, può essere necessario convertire la data in una stringa per poterla visualizzare nel modo desiderato. Ad esempio, si può voler mostrare una data in un formato diverso da quello utilizzato dal sistema operativo o si può voler aggiungere del testo o dei simboli alla data stessa. In questo articolo, vedremo come convertire una data in una stringa utilizzando il linguaggio di programmazione Swift.

## Come fare
Per convertire una data in una stringa, dobbiamo utilizzare la classe `DateFormatter` di Swift. Questa classe ci permette di formattare una data in un modo specifico e poi convertirla in una stringa. Ecco un esempio di codice che mostra come utilizzare `DateFormatter` per ottenere una stringa nella forma "giorno/mese/anno":

```Swift
// Creiamo una data
let date = Date()

// Creiamo un DateFormatter
let dateFormatter = DateFormatter()

// Impostiamo il formato desiderato
dateFormatter.dateFormat = "dd/MM/yyyy"

// Convertiamo la data in una stringa
let dateString = dateFormatter.string(from: date)

// Stampiamo la stringa
print(dateString)

// Output: 25/05/2021
```

In questo esempio, abbiamo creato un'istanza di `DateFormatter` e impostato il formato della data come "dd/MM/yyyy". Quindi, abbiamo utilizzato il metodo `string(from:)` per convertire la data in una stringa e infine stampato il risultato. Vediamo un altro esempio con un altro formato di data:

```Swift
// Creiamo una data
let date = Date()

// Creiamo un DateFormatter
let dateFormatter = DateFormatter()

// Impostiamo il formato desiderato
dateFormatter.dateFormat = "HH:mm:ss"

// Convertiamo la data in una stringa
let dateString = dateFormatter.string(from: date)

// Stampiamo la stringa
print(dateString)

// Output: 12:45:36
```

In questo caso, abbiamo utilizzato il formato "HH:mm:ss" per ottenere una stringa contenente solo l'ora, i minuti e i secondi della data.

## Approfondimento
Esistono molti altri formati di data che si possono utilizzare con `DateFormatter`, come ad esempio "MMM dd, yyyy" per ottenere una stringa del tipo "Mag 25, 2021" o "yyyy/MM/dd HH:mm" per ottenere una stringa del tipo "2021/05/25 12:45".

Inoltre, oltre al formato della data, è possibile anche impostare altre opzioni come la lingua, la zona oraria e il calendario da utilizzare nella conversione. Per ulteriori informazioni su tutte le opzioni disponibili con `DateFormatter`, si consiglia di consultare la documentazione ufficiale di Swift.

## Vedi anche
- [Documentazione ufficiale di Swift su DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Tutorial di Ray Wenderlich su DateFormatter](https://www.raywenderlich.com/8619-date-formatting-in-swift-an-introduction)