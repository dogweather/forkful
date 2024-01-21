---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:50.265242-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali significa creare valori numerici che non seguono un pattern prevedibile. I programmatori li usano per tutto, dai giochi ai sistemi di sicurezza, per aggiungere elementi di sorpresa o simulare eventi casuali.

## How to:
Swift fornisce funzioni integrate per numeri casuali. Ecco come usarli:

```Swift
// Generare un numero casuale tra 0 e un limite superiore (escluso)
let randomInt = Int.random(in: 0..<10)
print(randomInt) // Potrebbe stampare: 5

// Generare un numero casuale in virgola mobile tra 0 e 1
let randomDouble = Double.random(in: 0...1)
print(randomDouble) // Potrebbe stampare: 0.874634

// Generare un numero casuale Boolean
let randomBool = Bool.random()
print(randomBool) // Potrebbe stampare: true
```

## Deep Dive:
Prima che Swift offrisse il suo generatore di numeri casuali, i programmatori dovevano affidarsi a funzioni C come `rand()` e `arc4random()`. Oggi, Swift utilizza un generatore di numeri casuali sicuro che evita i bias e i problemi comuni delle funzioni precedenti. Oltre alle funzioni standard, puoi anche creare il tuo generatore di numeri casuali se hai esigenze specifiche implementando il protocollo `RandomNumberGenerator`.

## See Also:
- [Documentazione ufficiale di Swift sul Random Number Generator](https://developer.apple.com/documentation/swift/randomnumbergenerator)