---
date: 2024-01-20 17:51:49.813670-07:00
description: "How to: - Come fare: L'interpolazione di stringhe in Swift \xE8 stata\
  \ introdotta con la prima versione del linguaggio, come parte di una sintassi snella\
  \ ed\u2026"
lastmod: '2024-04-05T22:50:57.550068-06:00'
model: gpt-4-1106-preview
summary: "- Come fare: L'interpolazione di stringhe in Swift \xE8 stata introdotta\
  \ con la prima versione del linguaggio, come parte di una sintassi snella ed espressiva.\
  \ Prima di Swift, in linguaggi come Objective-C, si usavano metodi come `stringWithFormat:`\
  \ per ottenere risultati simili, ma in modo pi\xF9 verboso. Alternativamente, potresti\
  \ usare la concatenazione, ma rischia di rendere il codice pi\xF9 difficile da leggere\
  \ e mantenere. L'interpolazione di stringhe \xE8 anche pi\xF9 performante rispetto\
  \ alla concatenazione quando si lavora con stringhe complesse. Riguardo all'implementazione,\
  \ Swift compila l'interpolazione di stringhe convertendo ciascuna espressione all'interno\
  \ di \\(\\) in una stringa e poi concatenando le parti risultanti. Questo processo\
  \ \xE8 ottimizzato dall'ambiente di esecuzione di Swift per essere efficiente."
title: Interpolazione di una stringa
weight: 8
---

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
