---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Leggere gli Argomenti della Riga di Comando in Swift

## Cos’è e perché?

Leggere gli argomenti della riga di comando significa interpretare i dati inseriti quando si esegue un programma da terminale. I programmatori lo fanno per passare argomenti ai loro programmi al momento dell'esecuzione, rendendoli più flessibili e riutilizzabili.

## Come fare:

Per leggere gli argomenti della riga di comando in Swift, usiamo la costante globale `CommandLine.arguments`. Ecco un esempio semplice:

```Swift
// File: main.swift
for arg in CommandLine.arguments {
    print(arg)
}
```
Eseguendo il programma con l'argomento "ciao", vedrai "ciao" stampato sul terminale.

## Approfondimento:

### Contesto storico

Gli argomenti della riga di comando esistono sin dai primi giorni dei sistemi operativi. Insegnare a Swift come leggerli ci torna indietro alle origini dei principi di programmazione.

### Alternative:

Per usi più complessi, potrebbe essere necessario utilizzare un parser di argomenti della riga di comando, come Swift Argument Parser.

### Implementazione:

`CommandLine.arguments` è un array di stringhe. Il primo elemento dell'array è il nome del programma che è in esecuzione. Gli elementi successivi sono gli argomenti passati alla riga di comando.

## Vedi anche:

Se vuoi saperne di più, dai un'occhiata a queste risorse:

1. [Apple's Documentation on CommandLine](https://developer.apple.com/documentation/swift/commandline)
2. [Swift Argument Parser](https://github.com/apple/swift-argument-parser)
3. [Example of CommandLine arguments in Swift](https://www.hackingwithswift.com/example-code/language/how-to-read-command-line-arguments-using-commandline)