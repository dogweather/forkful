---
title:                "Estrazione di sottostringhe"
html_title:           "Swift: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Swift, probabilmente hai già incontrato situazioni in cui devi manipolare e analizzare stringhe di testo. Ecco perché è importante sapere come e quando utilizzare il metodo di estrazione delle sottostringhe in Swift.

## Come

Il metodo di estrazione delle sottostringhe in Swift si chiama `substring`. Per utilizzarlo, devi fornire una posizione di indice iniziale e finale all'interno della stringa di origine per definire quale parte della stringa vuoi estrarre. Puoi farlo sia utilizzando gli indici numerici che gli indici di caratteri.

```Swift
let stringa = "Hello World"

// Utilizzando gli indici numerici
let sottostringa1 = stringa.substring(from: 2, to: 7)
print(sottostringa1)
// Output: llo W

// Utilizzando gli indici di caratteri
let indiceIniziale = stringa.index(stringa.startIndex, offsetBy: 3)
let indiceFinale = stringa.index(stringa.endIndex, offsetBy: -2)
let sottostringa2 = stringa.substring(from: indiceIniziale, to: indiceFinale)
print(sottostringa2)
// Output: lo Wo
```

## Deep Dive

Il metodo di estrazione delle sottostringhe in Swift offre molteplici opzioni per personalizzare l'output della tua estrazione. Ad esempio, puoi utilizzare gli indici di caratteri per definire la posizione iniziale e la lunghezza della sottostringa desiderata.

```Swift
let stringa = "Hello World"

// Utilizzando gli indici di caratteri
let indiceIniziale = stringa.index(stringa.startIndex, offsetBy: 3)
let lunghezza = 5
let sottostringa = stringa.substring(from: indiceIniziale, length: lunghezza)
print(sottostringa)
// Output: lo Wo
```

Inoltre, puoi utilizzare anche il metodo `substring(from:)` per ottenere una sottostringa a partire da una determinata posizione iniziale fino alla fine della stringa di origine.

```Swift
let stringa = "Hello World"

let indiceIniziale = stringa.index(stringa.startIndex, offsetBy: 3)
let sottostringa = stringa.substring(from: indiceIniziale)
print(sottostringa)
// Output: lo World
```

## See Also

Per ulteriori informazioni sul metodo di estrazione delle sottostringhe in Swift, puoi consultare la documentazione ufficiale di Apple [qui](https://developer.apple.com/documentation/swift/string/1641819-substring). Se vuoi saperne di più sulle stringhe in Swift, dai un'occhiata a questi articoli su [raywenderlich.com](https://www.raywenderlich.com/1914-strings-tutorial-in-swift-4-2-for-strings-that-you-work-with#toc-anchor-012) e [hackingwithswift.com](https://www.hackingwithswift.com/read/0/overview).