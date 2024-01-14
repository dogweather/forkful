---
title:                "Swift: Generazione di numeri casuali"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'attività molto comune nella programmazione e può essere utile in una varietà di situazioni. Ad esempio, può essere utilizzato per creare password casuali, per generare dati di prova durante lo sviluppo di software o per creare una sfida in un gioco.

## Come

Per generare numeri casuali in Swift, è possibile utilizzare la funzione `arc4random_uniform()`. Questa funzione accetta un parametro che rappresenta il limite superiore dei numeri da generare e restituisce un numero casuale compreso tra 0 e il limite fornito.

```
Swift
let randomNum = arc4random_uniform(10)
print(randomNum) // Output: 3 (può variare in base al caso)
```

Se si desidera includere anche numeri negativi, è possibile utilizzare la funzione `Int.random(in:lowerBound:upperBound:)`. Questa funzione accetta un parametro che rappresenta il limite inferiore e un parametro che rappresenta il limite superiore dei numeri da generare e restituisce un numero casuale compreso tra questi due limiti.

```
Swift
let randomNum = Int.random(in: -50...50)
print(randomNum) // Output: -27 (può variare in base al caso)
```

## Deep Dive

È importante notare che, a differenza di altri linguaggi di programmazione, Swift non fornisce una funzione per generare numeri casuali con virgola mobile. Tuttavia, è possibile ottenere lo stesso risultato moltiplicando il numero casualo intero generato dalle funzioni sopra menzionate per un valore decimale.

```
Swift
let randomDecimal = Double(arc4random_uniform(100)) / 10.0
print(randomDecimal) // Output: 8.7 (può variare in base al caso)
```

Inoltre, se si necessita di una maggiore precisione nella generazione di numeri casuali, è possibile utilizzare la libreria `GameplayKit` di Apple, che offre una varietà di funzioni per la generazione di numeri casuali più avanzata.

## Vedi anche

- [Documentazione Swift: Generazione di numeri casuali](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID337)
- [Tutorial di Ray Wenderlich: Generazione di numeri casuali in Swift](https://www.raywenderlich.com/5497-random-numbers-in-swift)