---
title:                "Swift: Convertire una stringa in minuscolo"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché 

Convertire una stringa in minuscolo può essere utile quando si vuole standardizzare il testo, rendendolo tutto in caratteri minuscoli per facilitare la comparazione tra stringhe.

## Come Fare

```Swift
let stringa = "QUESTA STRINGA CONTIENE CARATTERI MAIUSCOLI"
let stringaInMinuscolo = stringa.lowercased()

print(stringaInMinuscolo)
```

```
questa stringa contiene caratteri maiuscoli
```

## Approfondimento

La funzione `lowercased()` è disponibile su tutti gli oggetti di tipo `String` in Swift. Utilizzando questa funzione, il computer esegue una trasformazione della stringa, sostituendo tutti i caratteri maiuscoli con i corrispondenti caratteri minuscoli. È importante notare che questa funzione non modifica la variabile originale, ma restituisce una nuova stringa in minuscolo.

## Vedi Anche

- [Documento ufficiale di Apple su le stringhe in Swift](https://developer.apple.com/documentation/swift/string)
- [Approfondimento sulle funzioni di manipolazione delle stringhe](https://medium.com/ios-os-x-development/swift-strings-and-characters-46fa021fe8a6)
- [Funzioni di conversione delle stringhe in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-convert-a-string-to-integer)