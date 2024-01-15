---
title:                "Cancellazione di caratteri corrispondenti a un modello"
html_title:           "Swift: Cancellazione di caratteri corrispondenti a un modello"
simple_title:         "Cancellazione di caratteri corrispondenti a un modello"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

C'è spesso la necessità di eliminare determinati caratteri da una stringa in Swift. Ad esempio, rimuovere le vocali da una parola o i caratteri speciali da una frase. In questo articolo, vedremo come utilizzare il metodo `replacingOccurrences(of:with:)` per eliminare i caratteri che corrispondono a un determinato modello all'interno di una stringa.

## Come Fare

Per eliminare caratteri corrispondenti a un modello in una stringa, è possibile utilizzare il metodo `replacingOccurrences(of:with:)` di Swift. Questo metodo prende in input il modello da cercare e la stringa sostitutiva da utilizzare per quei caratteri corrispondenti. Ad esempio, se vogliamo eliminare tutte le vocali dalla parola "banana", possiamo scrivere il seguente codice:

```Swift
let parola = "banana"
let parolaSenzaVocali = parola.replacingOccurrences(of: "[aeiou]", with: "", options: [.regularExpression])
print(parolaSenzaVocali) // risultato: "bnn"
```

Nell'esempio, abbiamo utilizzato un'espressione regolare `[aeiou]` per rappresentare tutte le vocali in lingua inglese. Ma è possibile utilizzare qualsiasi espressione regolare o stringa come modello di ricerca.

L'utilizzo del metodo `replacingOccurrences(of:with:)` è molto utile quando si lavora con stringhe complesse, come ad esempio i dati provenienti da un feed RSS o un testo scaricato da Internet. È possibile utilizzare questa tecnica per ripulire il testo e ottenere solo i dati importanti.

## Approfondimento

Il metodo `replacingOccurrences(of:with:)` è stato introdotto nel linguaggio Swift 3.0 e fa parte del framework Foundation. Non solo è possibile utilizzarlo per eliminare caratteri corrispondenti a un modello, ma anche per sostituirli con una stringa diversa. Per ulteriori informazioni su come utilizzare espressioni regolari in Swift, si consiglia di consultare la documentazione ufficiale Apple.

## Vedi Anche

- [Documentazione ufficiale Apple su espressioni regolari in Swift] (https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Codice di esempio su come utilizzare il metodo `replacingOccurrences` in Swift] (https://swiftdevcenter.github.io/replacing-occurrences-of-string-in-string-swift/)
- [Esempi di espressioni regolari per Swift] (https://javarocks.com/guides/swift-regular-expression-cheat-sheet)