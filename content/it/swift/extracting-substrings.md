---
title:                "Swift: Estrazione di sottostringhe"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
Estrarre sottostringhe è una pratica utile nella programmazione Swift perché ti permette di manipolare e ottenere solo una parte di una stringa più grande, rendendo il codice più leggibile e efficiente.

## Come fare
Per estrarre una sottostringa in Swift, utilizzare il metodo `substring(from:)` o `substring(to:)` a seconda che si voglia ottenere una sottostringa a partire dall'inizio o dalla fine della stringa originale. Ad esempio:
```Swift
let stringa = "Ciao a tutti!"
let sottostringa = stringa.substring(from: 5)
print(sottostringa)
// Output: a tutti!
```
In questo esempio, la sottostringa ottenuta inizia dalla quinta posizione della stringa originale, "a tutti!".

## Approfondimento
Ci sono molti altri modi per estrarre sottostringhe in Swift, come utilizzando il metodo `substring(with:)` per ottenere una sottostringa in base a un intervallo di posizioni, o utilizzando la notazione del "closed range" per ottenere una sottostringa di lunghezza specifica. Inoltre, è anche possibile utilizzare espressioni regolari per estrarre sottostringhe basate su determinati pattern all'interno di una stringa.

## Vedi anche
- [Documentazione Apple su metodi di estrazione delle sottostringhe](https://developer.apple.com/documentation/swift/string/3018574-substring)
- [Tutorial su espressioni regolari in Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)