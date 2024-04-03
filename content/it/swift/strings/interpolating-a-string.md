---
date: 2024-01-20 17:51:49.813670-07:00
description: "L'interpolazione di stringhe \xE8 l'atto di inserire valori di variabili\
  \ all'interno di stringhe di testo. I programmatori la usano per costruire messaggi\u2026"
lastmod: '2024-03-13T22:44:43.756656-06:00'
model: gpt-4-1106-preview
summary: "L'interpolazione di stringhe \xE8 l'atto di inserire valori di variabili\
  \ all'interno di stringhe di testo."
title: Interpolazione di una stringa
weight: 8
---

## What & Why? - Cosa & Perché?
L'interpolazione di stringhe è l'atto di inserire valori di variabili all'interno di stringhe di testo. I programmatori la usano per costruire messaggi dinamici senza ricorrere a operazioni di concatenazione complicate.

## How to: - Come fare:
```Swift
let nome = "Mondo"
let saluto = "Ciao, \(nome)!"
print(saluto) // Output: Ciao, Mondo!

let temperatura = 23.5
let messaggioMeteo = "La temperatura attuale è \(temperatura)°C."
print(messaggioMeteo) // Output: La temperatura attuale è 23.5°C.

let taskCompletati = 7
let taskTotali = 10
let progresso = "Hai completato \(taskCompletati) su \(taskTotali) compiti."
print(progresso) // Output: Hai completato 7 su 10 compiti.
```

## Deep Dive - Approfondimento
L'interpolazione di stringhe in Swift è stata introdotta con la prima versione del linguaggio, come parte di una sintassi snella ed espressiva. Prima di Swift, in linguaggi come Objective-C, si usavano metodi come `stringWithFormat:` per ottenere risultati simili, ma in modo più verboso.

Alternativamente, potresti usare la concatenazione, ma rischia di rendere il codice più difficile da leggere e mantenere. L'interpolazione di stringhe è anche più performante rispetto alla concatenazione quando si lavora con stringhe complesse.

Riguardo all'implementazione, Swift compila l'interpolazione di stringhe convertendo ciascuna espressione all'interno di \(\) in una stringa e poi concatenando le parti risultanti. Questo processo è ottimizzato dall'ambiente di esecuzione di Swift per essere efficiente.

## See Also - Vedi Anche
- The Swift Programming Language Guide on String Interpolation: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
