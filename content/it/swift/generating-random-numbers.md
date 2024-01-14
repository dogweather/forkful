---
title:    "Swift: Generazione di numeri casuali"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Generare numeri casuali è un'abilità importante per i programmatori che vogliono creare applicazioni divertenti con una componente casuale, come un gioco mobile.

## Come
La funzione `random` in Swift permette di generare numeri casuali in modo semplice e rapido. Ecco un esempio:
```Swift
let randomNumber = Int.random(in: 1...10)
print(randomNumber)
```
Questo codice genererà un numero casuale compreso tra 1 e 10 e lo stamperà in console.

## Approfondimento
La funzione `random` in Swift utilizza un generatore di numeri casuale a 32-bit e una distribuzione uniforme. Ciò significa che ogni numero ha la stessa probabilità di essere generato. Inoltre, è possibile utilizzare la funzione anche per generare numeri in virgola mobile o booleani.

## Vedi anche
- Documentazione ufficiale di Swift sulla funzione `random`: https://developer.apple.com/documentation/swift/int/2995645-random
- Tutorial di Ray Wenderlich su come generare numeri casuali in Swift: https://www.raywenderlich.com/11053630-random-numbers-in-swift 
- Altri metodi per generare numeri casuali in Swift: https://www.hackingwithswift.com/articles/68/how-to-generate-random-numbers-in-swift