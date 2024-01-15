---
title:                "Maiuscòlare una stringa"
html_title:           "Swift: Maiuscòlare una stringa"
simple_title:         "Maiuscòlare una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con una stringa che contiene parole in minuscolo e vuoi che appaiano con la lettera maiuscola, allora la funzione di "capitalizzazione" è ciò che ti serve.

## Come fare

Per capitalizzare una stringa in Swift, basta utilizzare il metodo `capitalized` su una variabile o costante di tipo stringa. Ecco un esempio:

```Swift
let nome = "mario"
let nomeCapitalizzato = nome.capitalized
print(nomeCapitalizzato)

// Output: Mario
```

## Approfondimenti

Il metodo `capitalized` è uno dei tanti metodi disponibili nella classe `String` di Swift. Per capire meglio come funziona, è importante comprendere come Swift gestisce le stringhe. In Swift, le stringhe sono descritte come una serie di caratteri Unicode, il che significa che possono contenere caratteri di qualsiasi alfabeto o lingua. Quando viene utilizzato il metodo `capitalized`, Swift converte il primo carattere della stringa in maiuscolo e lascia invariato tutto il resto.

Inoltre, esistono varianti del metodo `capitalized` che permettono di capitalizzare anche il secondo o l'ultimo carattere di una stringa, oppure di rimuovere i caratteri di spaziatura delle parole. Puoi trovare ulteriori informazioni su queste varianti nella documentazione ufficiale di Swift.

## Vedi anche

- [Documentazione di Swift sul metodo `capitalized`](https://developer.apple.com/documentation/swift/string/3126643-capitalized)
- [Tutti i metodi disponibili nella classe `String` di Swift](https://developer.apple.com/documentation/swift/string)
- [Come gestire stringhe Unicode in Swift](https://www.hackingwithswift.com/articles/140/how-to-handle-unicode-code-points-in-swift-best-practices)