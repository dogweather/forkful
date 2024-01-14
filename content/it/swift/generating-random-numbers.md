---
title:                "Swift: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è spesso utile in programmazione per creare scenari di gioco, testare algoritmi o semplicemente per fornire una variazione di dati in un programma.

## Come fare

La generazione di numeri casuali in Swift è semplice grazie alla funzione `arc4random_uniform`, che restituisce un numero intero casuale compreso tra 0 e il valore specificato come parametro. Vediamo un esempio:

```Swift
let randomNum = arc4random_uniform(100) // Restituisce un numero casuale tra 0 e 99
print(randomNum) // Output: 35
```

Possiamo anche generare un numero casuale compreso tra due valori specificati utilizzando la funzione `arc4random` e l'operatore modulo (%). Ad esempio, se vogliamo un numero casuale tra 50 e 100:

```Swift
let randomNum = 50 + arc4random() % 51 // Restituisce un numero casuale tra 50 e 100
print(randomNum) // Output: 87
```

È inoltre possibile generare numeri casuali di tipo `Double` utilizzando la funzione `Double(arc4random())` e specificando il range desiderato.  Ad esempio, se vogliamo un numero casuale tra 0 e 1:

```Swift
let randomDouble = Double(arc4random()) / Double(UInt32.max)
print(randomDouble) // Output: 0.3215643
```

## Approfondimento

Se vogliamo generare numeri casuali in un intervallo specificato, possiamo utilizzare la funzione `random(in:)` disponibile a partire da Swift 4.2. Ad esempio, se vogliamo un numero casuale tra 10 e 20:

```Swift
let randomNum = Int.random(in: 10...20)
print(randomNum) // Output: 15
```

Inoltre, è possibile utilizzare la libreria Foundation per generare numeri casuali di tipo `Float`, `CGFloat` e `CGFloat80` tramite la funzione `arc4random_uniform`.

## Vedi anche

- [Documentazione ufficiale di Swift su generazione di numeri casuali](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID334)