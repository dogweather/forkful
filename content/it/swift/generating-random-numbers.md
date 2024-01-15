---
title:                "Generazione di numeri casuali"
html_title:           "Swift: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è utile in molte situazioni di programmazione, ad esempio quando si vuole creare giochi, testare algoritmi o generare dati di test. Con Swift, è semplice generare numeri casuali grazie alle funzionalità integrate nel linguaggio.

## Come fare

Per ottenere un numero casuale in Swift, è possibile utilizzare la funzione `arc4random()`, che restituisce un intero casuale compreso tra 0 e 2^32 - 1. Ad esempio, per generare un numero casuale compreso tra 1 e 100, è possibile utilizzare la seguente espressione:

```Swift
let randomNum = Int(arc4random_uniform(100)) + 1
```

Per generare un numero casuale in un range specifico, è possibile utilizzare la funzione `random()` che accetta due parametri: il lower bound e il upper bound del range. Ad esempio, per generare un numero casuale compreso tra 5 e 10, è possibile utilizzare la seguente espressione:

```Swift
let randomNum = Int.random(in: 5...10)
```

In entrambi i casi, il tipo di dato del numero casuale sarà un intero.

Per generare un numero casuale con un tipo di dato diverso, è possibile utilizzare la funzione `random()` seguita dal tipo di dato desiderato. Ad esempio, per ottenere un numero casuale di tipo Double compreso tra 0 e 1, è possibile utilizzare la seguente espressione:

```Swift
let randomNum = Double.random(in: 0..<1)
```

## Approfondimenti

Il motivo per cui Swift ha due funzioni diverse per generare numeri casuali è legato alla sicurezza: `arc4random()` utilizza un algoritmo crittografico che rende più difficile prevedere il numero successivo, mentre `random()` utilizza un algoritmo pseudo-casuale più veloce ma meno sicuro.

Inoltre, è possibile personalizzare il seed, ovvero il valore utilizzato come punto di partenza per generare numeri casuali. Se si vuole ottenere una sequenza di numeri casuali sempre uguale, è possibile impostare un seed specifico utilizzando la funzione `srand48()` prima di chiamare `arc4random()` o `random()`. In questo modo, ogni volta che si esegue il codice si otterrà la stessa sequenza di numeri casuali.

## Vedi anche

Per ulteriori approfondimenti su come generare numeri casuali in Swift, è possibile consultare la documentazione ufficiale di Apple sulle funzioni `arc4random()` e `random()`. Inoltre, si consiglia di approfondire il concetto di sicurezza informatica e di algoritmi crittografici per comprendere meglio il funzionamento di `arc4random()`.