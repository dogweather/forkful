---
title:    "Swift: Scrivere su errore standard"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché scrivere a standard error

Scrivere a standard error può essere utile per ottenere informazioni più dettagliate sulle attività del tuo codice. Inoltre, può aiutare a identificare errori o problemi durante l'esecuzione del programma.

## Come scrivere a standard error

Per scrivere a standard error in Swift, puoi utilizzare la funzione `write` sull'oggetto `FileHandle` passando come parametro la stringa da scrivere. Ad esempio:

```Swift
let errorOutput = FileHandle.standardError
errorOutput.write("Errore: la variabile X non può essere uguale a 0")
```
Questo scriverà la stringa "Errore: la variabile X non può essere uguale a 0" a standard error.

## Approfondimento

Spesso si utilizza la scrittura a standard error per segnalare errori o eccezioni durante l'esecuzione del codice. Tuttavia, è importante notare che standard error viene utilizzato per scrivere informazioni di sistema o di debug, mentre standard output viene utilizzato per scrivere output regolare del programma.

Inoltre, esistono diverse altre funzioni disponibili in Swift per scrivere a standard error come `debugPrint` o `NSLog`.

## Vedi anche

- [Funzioni di debug in Swift](https://www.swiftbysundell.com/articles/debugging-in-swift/)
- [Documentazione ufficiale di Apple su FileHandle](https://developer.apple.com/documentation/foundation/filehandle)