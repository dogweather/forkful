---
title:                "Swift: Ricerca e sostituzione di testo"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
La sostituzione di testo è una pratica comune nella programmazione Swift, che può aiutare a risparmiare tempo e a mantenere il codice ordinato e leggibile. In questo articolo, esploreremo come effettuare ricerche e sostituire testo all'interno del tuo codice, fornendo esempi concreti e approfondendo l'argomento per una migliore comprensione.

## Come fare
Per eseguire una ricerca e sostituzione di testo in Swift, possiamo utilizzare il metodo built-in `replacingOccurrences(of:with:)`. Questo metodo accetta due parametri: il testo da cercare e il testo da sostituire. Ad esempio, se vogliamo sostituire tutte le occorrenze della parola "gatto" con "cane" in una stringa, possiamo utilizzare il seguente codice:

```Swift
let string = "Il mio gatto è molto carino!"
let nuovaStringa = string.replacingOccurrences(of: "gatto", with: "cane")
```

Il valore di `nuovaStringa` sarà "Il mio cane è molto carino!".

Possiamo anche specificare un range all'interno della stringa su cui effettuare la sostituzione, utilizzando il parametro opzionale `options`. Ad esempio, se vogliamo sostituire solo la prima occorrenza della parola "cane" con "gatto", possiamo utilizzare il seguente codice:

```Swift
let string = "Ho un cane molto divertente e un gatto molto pigro."
let nuovaStringa = string.replacingOccurrences(of: "cane", with: "gatto", options: .literal, range: nil)
```

In questo caso, il valore di `nuovaStringa` sarà "Ho un gatto molto divertente e un gatto molto pigro.".

## Approfondimento
Invece di passare una stringa come primo parametro a `replacingOccurrences(of:with:)`, possiamo utilizzare un'espressione regolare per specificare un pattern da cercare. Questo ci consente di effettuare una sostituzione in base a determinate condizioni. Ad esempio, se vogliamo sostituire tutte le vocali con il carattere "a" all'interno di una stringa, possiamo utilizzare il seguente codice:

```Swift
let string = "ciao a tutti!"
let nuovaStringa = string.replacingOccurrences(of: "[aeiou]", with: "a", options: .regularExpression, range: nil)
```

In questo caso, il valore di `nuovaStringa` sarà "caaa a taaa!".

## Vedi anche
- [La documentazione ufficiale di Apple su replacingOccurrences(of:with:)](https://developer.apple.com/documentation/foundation/nsstring/1410038-replacingoccurrences)
- [Un tutorial su come utilizzare le espressioni regolari in Swift](https://www.raywenderlich.com/10863835-regular-expressions-tutorial-getting-started)
- [Un altro articolo su come effettuare una ricerca e sostituzione di testo in Swift](https://theswiftdev.com/find-replace-in-a-string-with-swift/)