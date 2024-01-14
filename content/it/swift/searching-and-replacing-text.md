---
title:    "Swift: Cercare e sostituire il testo"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Quando si programma in Swift, è comune dover modificare il testo all'interno di una stringa, per esempio per correggere un errore di ortografia o sostituire una parola con un'altra. Per farlo in modo rapido ed efficiente, è necessario utilizzare il metodo di ricerca e sostituzione.

## Come fare

Per eseguire una ricerca e sostituzione in Swift, è possibile utilizzare il metodo `replacingOccurrences(of:with:)` su una stringa. Ecco un esempio di codice che sostituisce la parola "cane" con "gatto":

```Swift
let frase = "Il mio cane è dolce"
let nuovaFrase = frase.replacingOccurrences(of: "cane", with: "gatto")

print(nuovaFrase) // Output: Il mio gatto è dolce
```

In questo esempio, la stringa `frase` viene sostituita dalla nuova stringa `nuovaFrase`, in cui la parola "cane" è stata sostituita con "gatto". Si può anche specificare un parametro opzionale `options` per personalizzare la ricerca e la sostituzione.

## Approfondimento

Il metodo `replacingOccurrences(of:with:)` è molto utile quando si deve sostituire una singola occorrenza di una parola all'interno di una stringa. Tuttavia, se si vuole sostituire più occorrenze o eseguire una ricerca con espressione regolare, si può utilizzare il metodo `replacingOccurrences(of:with:options:range:)`. In questo caso, è possibile specificare un range di caratteri in cui eseguire la ricerca e la sostituzione, utilizzando il tipo `Range<String.Index>`.

## Vedi anche

- Documentazione Apple sul metodo `replacingOccurrences(of:with:)`: https://developer.apple.com/documentation/foundation/nsstring/1408974-replacingoccurrences
- La guida completa alla gestione delle stringhe in Swift: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html