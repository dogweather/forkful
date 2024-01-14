---
title:                "Swift: Eliminazione di caratteri corrispondenti a un modello"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

C'è un'utilissima funzione in Swift che permette di eliminare i caratteri corrispondenti a un certo pattern. Scopriamo insieme come usarla per semplificare il nostro codice!

## Come fare

Per eliminare i caratteri che corrispondono a un pattern, dobbiamo utilizzare la funzione `replacingOccurrences(of:with:)` sul nostro stringa. Segui il codice di esempio qui sotto e guarda l'output per vedere come funziona:

```Swift
let string = "Questa è una stringa di prova!"
let newString = string.replacingOccurrences(of: "a", with: "")
print(newString) // Output: Quest è un string di prov!
```

Come puoi vedere, abbiamo eliminato tutti i caratteri "a" dalla nostra stringa originale. Ovviamente, possiamo cambiare il pattern e il carattere di sostituzione a nostro piacimento.

```Swift
let string = "009 8765 4321"
let newString = string.replacingOccurrences(of: "[0-9]", with: "-", options: .regularExpression)
print(newString) // Output: --- ---- -----
```

In questo esempio, abbiamo utilizzato una espressione regolare per sostituire tutti i numeri con il carattere "-". Ci sono molte altre possibilità di utilizzo, quindi assicurati di fare qualche ricerca per scoprire cosa puoi fare con questa funzione.

## Deep Dive

La funzione `replacingOccurrences(of:with:)` è estremamente utile in molti casi, sopratutto quando abbiamo bisogno di manipolare le stringhe. Essendo in grado di utilizzare espressioni regolari come pattern, ci permette di effettuare operazioni molto complesse in modo semplice.

Un'altra interessante caratteristica di questa funzione è che non modifica la stringa originale, ma ne restituisce una copia modificata. Questo è particolarmente utile quando dobbiamo lavorare con dati sensibili, evitando di modificarli accidentalmente.

## Vedi anche

- [Documentazione Apple su `replacingOccurrences(of:with:)`](https://developer.apple.com/documentation/foundation/nsstring/1413213-replacingoccurrences)
- [Tutorial su espressioni regolari in Swift](https://www.appcoda.com/regular-expression-swift/)