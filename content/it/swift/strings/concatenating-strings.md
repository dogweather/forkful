---
date: 2024-01-20 17:35:35.089770-07:00
description: "How to: Swift rende la concatenazione di stringhe semplice e diretta.\
  \ Puoi usare l'operatore `+` per unire le stringhe o `\\()` per inserire valori\u2026"
lastmod: '2024-03-13T22:44:43.762378-06:00'
model: gpt-4-1106-preview
summary: Swift rende la concatenazione di stringhe semplice e diretta.
title: Concatenazione di stringhe
weight: 3
---

## How to:
Swift rende la concatenazione di stringhe semplice e diretta. Puoi usare l'operatore `+` per unire le stringhe o `\()` per inserire valori all'interno di una stringa.

```Swift
let saluto = "Ciao"
let mondo = "mondo"
let messaggioCompleto = saluto + ", " + mondo + "!"
print(messaggioCompleto) // Output: Ciao, mondo!
```

Un altro modo è usare l'interpolazione di stringhe:

```Swift
let nome = "Marco"
let eta = 30
let salutoPersonale = "Ciao \(nome), hai \(eta) anni."
print(salutoPersonale) // Output: Ciao Marco, hai 30 anni.
```

## Deep Dive
Concatenare le stringhe è una funzione fondamentale in quasi tutti i linguaggi di programmazione e anche in Swift, introdotto nel 2014. Le alternative includono l'uso di metodi come `append()` o operazioni con array di stringhe.

In passato, soprattutto nei vecchi linguaggi, la concatenazione poteva essere poco efficiente se fatta ripetutamente a causa della creazione di molte stringhe intermedie. Swift ottimizza questo processo con una gestione della memoria efficace.

Esempio con `append()`:
```Swift
var messaggio = "Benvenuto"
messaggio.append(", come va?")
print(messaggio) // Output: Benvenuto, come va?
```

Uno sguardo agli array:
```Swift
let parole = ["Arrivederci", "e", "grazie", "per", "tutto", "il", "pesce!"]
let frase = parole.joined(separator: " ")
print(frase) // Output: Arrivederci e grazie per tutto il pesce!
```

## See Also
Per ulteriori informazioni sulla concatenazione di stringhe in Swift, consulta:

- [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift Programming: The Big Nerd Ranch Guide](https://www.bignerdranch.com/books/swift-programming/)
