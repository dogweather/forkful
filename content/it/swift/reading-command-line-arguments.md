---
title:                "Swift: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un programma non significa solo creare un codice e farlo funzionare, ma anche comprendere come esso interagisce con l'utente. Una delle modalità più comuni per l'utente di fornire input al programma è attraverso gli argomenti della riga di comando. In questo articolo vedremo come leggere questi argomenti in Swift.

## Come fare

Per prima cosa, è necessario definire una variabile di tipo `CommandLine` per poter accedere agli argomenti della riga di comando. Successivamente, possiamo utilizzare il metodo `arguments` per ottenere un array contenente tutti gli argomenti passati al programma. Ecco un esempio di come leggere e stampare gli argomenti forniti dall'utente:

```Swift
let arguments = CommandLine.arguments

for argument in arguments {
    print(argument)
}
```

Se si esegue il programma con il comando `swift nomeDelProgramma.swift uno due tre`, l'output dovrebbe essere:

```
nomeDelProgramma.swift
uno
due
tre
```

Per accedere a un argomento specifico, è possibile utilizzare la sua posizione nell'array. Ad esempio, se si vuole ottenere il terzo argomento, si può usare `arguments[2]`.

## Approfondimento

Oltre alla lettura degli argomenti della riga di comando, è possibile anche aggiungere dei parametri di opzioni, che rendono il programma più flessibile. Per aggiungere un parametro di opzione, basta utilizzare il formato `--nomeDelParametro=valore`. Ad esempio:

```Swift
let options = CommandLine.options

if options.keys.contains("--nome") {
    let name = options["--nome"]
    print("Ciao \(name)!")
}
```
In questo caso, se il parametro `--nome` è presente, il programma saluta l'utente usando il nome fornito come valore.

## Vedi anche
- [Documentazione di Swift per la classe `CommandLine`](https://developer.apple.com/documentation/swift/commandline)
- [Tutorial su come leggere argomenti della riga di comando in Swift](https://www.hackingwithswift.com/example-code/system/how-to-read-command-line-arguments-using-commandline)
- [Esempi di utilizzi avanzati degli argomenti della riga di comando in Swift](https://www.swiftbysundell.com/posts/using-the-command-line-in-swift)