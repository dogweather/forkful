---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:31.363892-07:00
description: "Come fare: Le strutture `String` di Swift vengono fornite con un paio\
  \ di metodi integrati per manipolare il caso delle stringhe. Ecco alcuni approcci\
  \ per\u2026"
lastmod: '2024-03-13T22:44:43.753707-06:00'
model: gpt-4-0125-preview
summary: Le strutture `String` di Swift vengono fornite con un paio di metodi integrati
  per manipolare il caso delle stringhe.
title: Capitalizzare una stringa
weight: 2
---

## Come fare:
Le strutture `String` di Swift vengono fornite con un paio di metodi integrati per manipolare il caso delle stringhe. Ecco alcuni approcci per capitalizzare le stringhe in Swift, inclusi l'uso di metodi standard e di librerie di terze parti se necessario.

### Usando metodi integrati
Per capitalizzare la prima lettera di una stringa e mettere in minuscolo il resto:

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // Output: "Hello, world"
```

Per capitalizzare la prima lettera di ogni parola in una frase, puoi usare la proprietà `capitalized`:

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // Output: "Hello, World"
```

### Usando una libreria di terze parti
Anche se la libreria standard di Swift è piuttosto completa, alcuni formati di capitalizzazione specifici potrebbero richiedere operazioni più complesse o possono essere semplificati utilizzando librerie di terze parti. Una delle più popolari per la manipolazione delle stringhe è SwiftRichString. (Nota: Assicurati sempre di includere le librerie di terze parti tramite Swift Package Manager, CocoaPods o Carthage, e di importarle nel tuo file.)

Prima, dovresti aggiungere `SwiftRichString` al tuo progetto. Una volta installato, puoi usarlo per eseguire varie operazioni sulle stringhe, includendo esigenze di capitalizzazione specifiche. Tuttavia, ad oggi, i metodi integrati di Swift coprono adeguatamente la maggior parte dei casi d'uso di capitalizzazione senza la necessità di librerie esterne solo per capitalizzare le stringhe.

Fai sempre riferimento all'ultima documentazione della libreria per eventuali aggiornamenti o modifiche nei metodi.
