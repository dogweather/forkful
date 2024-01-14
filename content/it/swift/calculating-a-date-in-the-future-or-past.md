---
title:    "Swift: Calcolare una data nel futuro o nel passato"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere necessario per pianificare eventi, tenere traccia di scadenze o semplicemente per capire in che giorno della settimana cadrà una certa data.

## Come

Per calcolare una data in Swift, è necessario utilizzare la classe `Date` e il metodo `addingTimeInterval` per aggiungere o sottrarre secondi, minuti, ore, giorni, settimane o mesi alla data attuale. Ecco un esempio di codice:

```Swift
// Calcolare una data nel futuro, aggiungendo 7 giorni alla data attuale
let currentDate = Date()
let futureDate = currentDate.addingTimeInterval(60 * 60 * 24 * 7) // rappresenta 7 giorni in secondi
print(futureDate) // output: 2021-06-09 12:00:00 +0000

// Calcolare una data nel passato, sottraendo 1 mese alla data attuale
let pastDate = currentDate.addingTimeInterval(-60 * 60 * 24 * 30) // rappresenta 1 mese in secondi
print(pastDate) // output: 2021-04-10 12:00:00 +0000
```

## Approfondimento

Per calcolare una data nel futuro o nel passato, è importante tenere conto del fuso orario della data attuale. Utilizzando il codice sopra come esempio, se il fuso orario della data attuale è impostato su GMT+2, l'output delle date future e passata sarà rispettivamente il 9 giugno e il 10 aprile alle 14:00.

Inoltre, è possibile utilizzare il metodo `addingComponents` per aggiungere o sottrarre specifiche componenti di una data, come ad esempio giorni della settimana o settimane di un mese.

## Vedi anche

- [Documentazione Apple su Date](https://developer.apple.com/documentation/foundation/date)
- [Tutorial su calcolare una data in Swift](https://www.codementor.io/@abiodunosesi/how-to-calculate-date-in-swift-3-lbomjv802)
- [Esempi di codice per calcolare date in Swift](https://www.hackingwithswift.com/example-code/system/how-to-calculate-one-month-later-from-a-given-date)