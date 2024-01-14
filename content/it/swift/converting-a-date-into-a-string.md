---
title:    "Swift: Convertire una data in una stringa"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando si lavora con date all'interno di un'applicazione Swift, si ha la necessità di convertire una data in una stringa per poterla visualizzare in modo leggibile per l'utente. In questo articolo, impareremo come eseguire questa operazione in modo semplice ed efficace.

## Come

Per convertire una data in una stringa in Swift, è possibile utilizzare il metodo `DateFormatter`, che è disponibile sulla classe `Date`. Vediamo un esempio pratico:

```Swift
let date = Date()

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let dateString = dateFormatter.string(from: date)

print(dateString)
// Output: 04/08/2021
```

Nell'esempio sopra, abbiamo creato un'istanza di `DateFormatter` e abbiamo impostato il formato desiderato della stringa di output utilizzando il formato `dd/MM/yyyy`. Infine, abbiamo utilizzato il metodo `string(from:)` per convertire la data in una stringa e stamparla a schermo.

## Deep Dive

Il metodo `DateFormatter` offre molte opzioni di formattazione per le date, ad esempio, è possibile modificare la posizione dei mesi e dei giorni della settimana, utilizzare abbreviazioni per le parole o aggiungere degli zeri iniziali per i numeri inferiori a 10. È possibile trovare la lista completa dei formati disponibili nella documentazione ufficiale di Apple.

Inoltre, è possibile utilizzare anche i `Locale` di sistema per ottenere la formattazione delle date in base alla lingua del dispositivo dell'utente. Ad esempio, se un utente italiano visualizzerà la data in italiano, mentre un utente inglese la vedrà in inglese.

## See Also

- [Documentazione Apple su DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Tutorial su come formattare le date in Swift](https://www.raywenderlich.com/1520790-date-formatting-in-swift)
- [Utilizzare i locale in Swift](https://www.swiftbysundell.com/articles/using-locale-in-swift/)