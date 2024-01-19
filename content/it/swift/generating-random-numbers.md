---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Generare numeri casuali in programmazione significa creare un output che non può essere previsto logicamente. È utile per creare dati di test, animazioni diverse, elementi di gameplay o anche per la crittografia.

## Come fare:

Ecco un esempio di come generare un numero casuale tra 1 e 10 in Swift.

```Swift
import Foundation

let numeroCasuale = Int.random(in: 1..<11)
print(numeroCasuale)
```

In esecuzione, questo programma produrrà un numero casuale tra 1 e 10 ogni volta.

```Swift
4
```

## Approfondimenti:

Nel contesto storico, i numeri casuali venivano generati inizialmente mediante metodi fisici, come il lancio di dadi o l'uso del rumore di fondo radio. Oggigiorno in informatica, generiamo numeri casuali (RNG, Random Number Generators) usando specifici algoritmi.

Per quanto riguarda le alternative, in Swift hai a disposizione i metodi `arc4random_uniform(UInt32)` per generare un numero casuale entro un specifico range, e `drand48()` per un valore Double casuale tra 0 e 1.

A proposito di dettagli di implementazione, il metodo `random(in:)` in Swift utilizza l'algoritmo Tausworthe "Taus88" versione LCG (Linear Congruential Generator) - un tipo di generatore di numeri pseudocasuali.

## Vedi Anche:

Per ulteriori informazioni su `random(in:)` consulta la [documentazione Swift ufficiale](https://developer.apple.com/documentation/swift/int/2994310-random).

Per approfondimenti sulla generazione di numeri casuali in generale, ecco un [articolo interessante da Wikipedia](https://it.wikipedia.org/wiki/Generatore_di_numeri_casuali).