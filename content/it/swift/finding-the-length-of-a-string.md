---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Swift: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché viene utilizzato?
Trovare la lunghezza di una stringa è una operazione comune in programmazione che permette di determinare il numero di caratteri all'interno di una stringa. I programmatori utilizzano questa funzione per svolgere operazioni come il controllo di validità di input utente o il conteggio dei caratteri in una parola o frase.

## Come si fa?
Per trovare la lunghezza di una stringa in Swift, è possibile utilizzare il metodo `count` su una variabile di tipo `String` o su una costante di tipo `String`. Ad esempio:

```Swift
let stringa = "Ciao amici"
print(stringa.count)
```
Output: 11

## Approfondimento
La funzione per trovare la lunghezza di una stringa è stata introdotta per la prima volta nel linguaggio di programmazione C nel 1972 da Dennis Ritchie. In passato, i programmatori dovevano scrivere manualmente il codice per contare i caratteri in una stringa, ma grazie alla funzione incorporata nei linguaggi moderni come Swift, questa operazione è diventata molto più semplice e veloce.

Un'alternativa per trovare la lunghezza di una stringa in Swift è utilizzare il metodo `characters.count`, ma questo metodo è obsoleto a partire dalla versione 4 del linguaggio e si consiglia di utilizzare il metodo `count` al suo posto.

## Vedi anche
Per ulteriori informazioni su come utilizzare il metodo `count` per trovare la lunghezza di una stringa in Swift, si consiglia di consultare la documentazione ufficiale del linguaggio disponibile su [Swift.org](https://docs.swift.org).