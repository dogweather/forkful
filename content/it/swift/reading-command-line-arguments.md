---
title:                "Swift: Lettura degli argomenti della riga di comando"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché leggere gli argomenti della riga di comando?

Se stai sviluppando un'applicazione da riga di comando in Swift, probabilmente avrai bisogno di leggere gli argomenti passati dall'utente al momento dell'esecuzione del programma. Questo ti aiuterà a gestire gli input dell'utente in modo dinamico e a creare un'esperienza più fluida per l'utente. In questa guida, vedremo come leggere gli argomenti della riga di comando in Swift.

## Come farlo:

In Swift, ci sono diverse opzioni per leggere gli argomenti della riga di comando. Una delle opzioni più comuni è utilizzare il metodo `CommandLine.arguments`. Questo metodo restituisce un array di stringhe che rappresentano gli argomenti passati alla riga di comando. Vediamo un esempio:

```Swift
let arguments = CommandLine.arguments
print(arguments)
```

Se eseguiamo questo programma con alcuni argomenti, ad esempio `swift main.swift arg1 arg2`, otterremo il seguente output:

```
["main.swift", "arg1", "arg2"]
```

Possiamo quindi utilizzare i valori all'interno di questo array per gestire l'input dell'utente e eseguire determinate azioni in base agli argomenti passati.

## Approfondimento

Per una maggiore flessibilità, possiamo anche utilizzare la libreria `ArgumentParser` di Apple, che ci permette di definire opzioni e argomenti personalizzati per il nostro programma da riga di comando. Possiamo utilizzare questa libreria per creare un'interfaccia più intuitiva e facile da usare per l'utente. Ad esempio, possiamo definire una variabile `verbose` per specificare se l'utente vuole o meno output dettagliati. Vediamo un esempio di utilizzo di `ArgumentParser`:

```Swift
import ArgumentParser

struct MyCommand: ParsableCommand {
    @Flag(help: "Prints verbose output")
    var verbose: Bool

    func run() throws {
        if verbose {
            print("Verbose output enabled")
        }
    }
}

MyCommand.main()
```

Con questo codice, possiamo avviare il nostro programma con l'argomento `--verbose` per ottenere l'output dettagliato.

## Vedi anche

- [Documentazione di Apple su CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Documentazione di Apple su ArgumentParser](https://developer.apple.com/documentation/swift-argumentparser)
- [Tutorial su come scrivere un'applicazione da riga di comando in Swift](https://dev.to/yagiz/creating-a-command-line-tool-in-swift-5-eop)

Speriamo che questa guida ti sia stata utile per imparare come leggere gli argomenti della riga di comando in Swift. Con questo, puoi creare applicazioni più interattive e personalizzate per i tuoi utenti. Continua a esplorare il mondo della programmazione in Swift e buon coding!