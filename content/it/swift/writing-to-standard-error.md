---
title:                "Swift: Scrittura su standard error"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché
Scrivere su standard error è un utile strumento per la gestione degli errori e il debug durante la programmazione in Swift.

## Come Fare
Per scrivere su standard error in Swift, è possibile utilizzare il metodo di stampa `print(_:to:)`, specificando come secondo parametro l'output desiderato, ad esempio `standardError`. Esempio:

```Swift
print("Errore: Array vuoto", to: &standardError)

// Output: Errore: Array vuoto
```

## Approfondimento
Scrivere su standard error è particolarmente utile quando si vuole visualizzare degli errori o degli avvisi in modo diverso rispetto alla stampa standard su console. Inoltre, il contenuto scritto su standard error viene visualizzato in modalità di "non buffering", quindi viene mostrato immediatamente senza attendere la chiusura del programma.

## Vedi Anche
- [Documentazione Apple su standard error in Swift](https://developer.apple.com/documentation/swift/standarderror)
- [Articolo Medium su come gestire gli errori in Swift](https://medium.com/@analogbird/error-handling-in-swift-99b0137b4d05)
- [Tutorial su debug e gestione errori in Swift](https://codewithchris.com/swift-error-handling/)