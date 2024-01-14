---
title:    "Swift: Trovare la lunghezza di una stringa"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

La lunghezza di una stringa è un aspetto fondamentale della programmazione in Swift. Sapere come trovare la lunghezza di una stringa è importante per poter manipolare e gestire correttamente le informazioni all'interno del nostro codice.

## Come fare

Per trovare la lunghezza di una stringa in Swift, possiamo utilizzare il metodo `count` su una variabile di tipo `String`. Ad esempio:

```Swift
let str = "Ciao a tutti!"
let lunghezza = str.count
print(lunghezza) // Output: 13
```

Il metodo `count` ci restituisce il numero di caratteri all'interno della stringa. È importante notare che i caratteri di spaziatura e i caratteri di punteggiatura vengono conteggiati come parte della lunghezza della stringa.

## Approfondimento

In Swift, la lunghezza di una stringa è in realtà il numero di caratteri Unicode contenuti in essa. Ciò significa che, se ad esempio utilizziamo delle emoji all'interno di una stringa, queste verranno conteggiate come un singolo carattere all'interno della lunghezza della stringa.

Per manipolare correttamente le stringhe contenenti emoji o altri caratteri Unicode, è importante comprendere questo aspetto della lunghezza delle stringhe in Swift.

## Vedi anche

- [Documentazione Apple su String.count](https://developer.apple.com/documentation/swift/string/1643076-count)
- [Tutorial su gestione delle stringhe in Swift](https://www.raywenderlich.com/1910118-swift-string-cheat-sheet-and-reference)