---
date: 2024-01-20 17:57:15.608388-07:00
description: "Leggere gli argomenti della riga di comando significa estrarre i dati\
  \ inseriti dagli utenti quando avviano il tuo programma da terminale. I programmatori\u2026"
lastmod: '2024-03-11T00:14:17.402177-06:00'
model: gpt-4-1106-preview
summary: "Leggere gli argomenti della riga di comando significa estrarre i dati inseriti\
  \ dagli utenti quando avviano il tuo programma da terminale. I programmatori\u2026"
title: Lettura degli argomenti della riga di comando
---

{{< edit_this_page >}}

## What & Why?
Leggere gli argomenti della riga di comando significa estrarre i dati inseriti dagli utenti quando avviano il tuo programma da terminale. I programmatori lo fanno per rendere le applicazioni interattive e per passare parametri al volo, senza hardcoding.

## How to:
Swift rende la lettura degli argomenti da riga di comando un gioco da ragazzi. Ecco come:

```Swift
// main.swift
import Foundation

// Accedere agli argomenti della riga di comando
let arguments = CommandLine.arguments

// Stamparli uno per uno
for arg in arguments {
    print(arg)
}

// Usare gli argomenti per fare qualcosa di utile
if arguments.count > 1 {
    print("Ciao, \(arguments[1])!")
} else {
    print("Hey! Mi aspettavo un nome da salutare.")
}
```

Esempio di output se esegui `swift main.swift Giovanni` nel terminale:

```
/path/to/main.swift
Giovanni
Ciao, Giovanni!
```

## Deep Dive
Prima che Swift comparisse sulla scena, Objective-C era il re di Cupertino. Tuttavia, dalle ceneri è emerso Swift, con un modo più accessibile per accedere agli argomenti della riga di comando. In C, sareste stati accolti da `int main(int argc, char * argv[])`. Alternativamente, le librerie come `Cocoa` in Objective-C nascondevano questi dettagli dietro classi di app alto livello.

In Swift, `CommandLine` è un'enumerazione piuttosto diretta che fornisce accesso agli argomenti come un array di stringhe. È importante notare che il primo argomento è sempre il percorso eseguibile del programma. Questo differisce da altri linguaggi come Python, dove potresti usare `sys.argv` per ottenere gli argomenti e il primo argomento è il comando utilizzato per eseguire lo script.

Sebbene l'approccio di Swift sia semplice, per applicazioni complesse potrebbe essere necessario parse più sofisticati. In questi casi, si potrebbe ricorrere a framework di terze parti come `Swift Argument Parser` per una gestione robusta degli argomenti della riga di comando.

## See Also
- Documentazione ufficiale Swift sul `CommandLine`: https://developer.apple.com/documentation/swift/commandline
- GitHub del progetto `Swift Argument Parser`: https://github.com/apple/swift-argument-parser
- Tutorial interattivo su Swift: https://www.hackingwithswift.com/
