---
title:                "Convertire una stringa in minuscolo"
html_title:           "Swift: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Perché potresti voler convertire una stringa in minuscolo? Ci sono diverse ragioni che potrebbero portarti a farlo. Ad esempio, potresti voler confrontare due stringhe senza considerare le maiuscole e minuscole, o potresti doverle formattare in un modo specifico per alcune funzionalità del tuo programma.

## Come
```Swift
let myString = "CIAO A TUTTI"
let lowerCaseString = myString.lowercased()
print(lowerCaseString)
// Output: ciao a tutti
```

Per convertire una stringa in minuscolo in Swift, puoi utilizzare il metodo `lowercased()` sulla stringa di cui vuoi ottenere la versione in minuscolo. Questo metodo restituisce una nuova stringa in cui tutte le lettere sono state convertite in minuscolo.

## Approfondimento
In Swift, c'è anche il metodo `uppercased()` che converte una stringa in maiuscolo. Inoltre, è possibile utilizzare il metodo `capitalized()` per convertire la prima lettera di ogni parola in maiuscolo e le rimanenti in minuscolo.

È importante notare che questi metodi utilizzano le regole di localizzazione per la conversione, quindi il risultato può variare in base alla lingua e al sistema operativo utilizzati.

## Vedi anche
- [Documentazione Apple su stringhe in Swift](https://developer.apple.com/documentation/swift/string)
- [Come confrontare stringhe in Swift](https://www.built.io/blog/swift-tutorial-how-to-compare-strings-in-swift)
- [Come formattare una stringa in Swift](https://www.appcoda.com/swift-string-formatting/)