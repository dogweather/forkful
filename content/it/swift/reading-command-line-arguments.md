---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Swift: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché leggere gli argomenti della riga di comando in Swift

Se stai scrivendo un programma in Swift che deve essere eseguito da riga di comando, è importante essere in grado di leggere gli argomenti della riga di comando per poter gestire l'input dell'utente. In questo articolo, impareremo come farlo utilizzando codice Swift e vedremo alcuni esempi di output.

## Come leggere gli argomenti della riga di comando in Swift

Per leggere gli argomenti della riga di comando in Swift, possiamo utilizzare l'array `CommandLine.arguments`. Possiamo accedere agli argomenti specifici utilizzando gli indici dell'array, dove l'indice 0 è il nome del programma in esecuzione. Vediamo un esempio di come stampare tutti gli argomenti passati alla riga di comando:

```Swift
for argument in CommandLine.arguments {
    print(argument)
}
```

Se eseguiamo questo programma passando "Swift is awesome" come argomenti nella riga di comando, otterremo il seguente output:

```
./programma
Swift
is
awesome
```

## Approfondimento

Oltre ad accedere agli argomenti specifici, possiamo anche controllare il numero totale di argomenti passati utilizzando `CommandLine.argc` e ottenere il nome del programma con `CommandLine.arguments[0]`. Inoltre, possiamo utilizzare l'API CommandLine opzionale di Swift per specificare opzioni e argomenti con caratteri speciali come `-` e `--`, rendendo il nostro programma più versatile.

## Vedi anche

- Documentazione ufficiale di Swift su [CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Parsing Command-Line Arguments in Swift](https://medium.com/swift-programming/swift-command-line-arguments-parsing-made-simple-d6a669fdef5f) di Martina Fasano su Medium