---
title:    "Swift: Lettura degli argomenti dalla riga di comando"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

Se stai cercando di diventare un programmatore Swift di successo, probabilmente sei consapevole dell'importanza di imparare a leggere gli argomenti della riga di comando. Questa è una competenza fondamentale per molte applicazioni e ti permetterà di creare programmi più versatili e interattivi per gli utenti.

## Come fare

Per leggere gli argomenti della riga di comando in Swift, è sufficiente utilizzare il metodo `CommandLine.arguments` all'interno del blocco `main`. Ecco un esempio di codice:

```Swift
import Foundation

let arguments = CommandLine.arguments
print(arguments)
```

Utilizzando questo codice, verranno restituiti tutti gli argomenti della riga di comando inseriti dall'utente al momento dell'esecuzione del programma. Per esempio, se l'utente esegue il programma con `swift myProgram.swift arg1 arg2`, l'array `arguments` conterrà i valori `[myProgram.swift, arg1, arg2]`.

## Approfondimento

Oltre al semplice accesso agli argomenti della riga di comando, Swift offre diverse funzionalità avanzate per manipolarli. Ad esempio, è possibile utilizzare l'operatore ternario per fornire valori di default in caso di mancanza di alcuni argomenti, o convertire gli argomenti in tipi di dati specifici usando metodi come `Int()` o `Double()`.

## Vedi anche

- [Documentazione ufficiale di Swift su CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Tutorial su come leggere gli argomenti della riga di comando in Swift](https://www.hackingwithswift.com/example-code/system/how-to-read-command-line-arguments-using-commandline)