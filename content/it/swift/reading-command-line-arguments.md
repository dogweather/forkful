---
title:                "Lettura degli argomenti della linea di comando"
html_title:           "Swift: Lettura degli argomenti della linea di comando"
simple_title:         "Lettura degli argomenti della linea di comando"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Leggere gli argomenti della riga di comando è una pratica comune tra i programmatori, che consente al software di accettare input direttamente dalla riga di comando anziché da un'interfaccia utente. Questo può essere utile per automatizzare processi, testare il software o fornire un'esperienza più avanzata agli utenti.

## Come fare:
Per leggere gli argomenti della riga di comando in Swift, possiamo utilizzare la variabile globale ```CommandLine.arguments```, che restituisce un array contenente tutti gli argomenti passati al programma. Possiamo quindi accedere a ogni argomento tramite l'indice dell'array, con il primo elemento che corrisponde al nome del programma stesso. Di seguito un esempio di codice e il relativo output:

```Swift
let arguments = CommandLine.arguments
print(arguments[0]) //nome del programma
print(arguments[1]) //primo argomento
print(arguments[2]) //secondo argomento
```

Output:
```
programma
argomento1
argomento2
```

## Approfondimento:
L'abilità di leggere gli argomenti della riga di comando è stata introdotta in Swift 3, in precedenza era necessario utilizzare la libreria Foundation. In alternativa, possiamo anche utilizzare librerie di terze parti come Commander o SwiftCLI.

Per implementare un'interfaccia utente da riga di comando più avanzata, possiamo anche utilizzare il framework di Apple CommandLineKit, che fornisce funzionalità come la gestione delle opzioni e dei comandi.

## Vedi anche:
- Documentazione ufficiale di Swift sull'accesso agli argomenti della riga di comando: https://developer.apple.com/documentation/swift/commandline/arguments
- Commander: https://github.com/kylef/Commander
- SwiftCLI: https://github.com/jakeheis/SwiftCLI
- CommandLineKit: https://github.com/GuillaumeSabran/CommandLineKit