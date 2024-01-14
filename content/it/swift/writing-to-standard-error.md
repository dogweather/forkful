---
title:    "Swift: Scrivere su standard error"
keywords: ["Swift"]
---

{{< edit_this_page >}}

##Perché##
Scrivere su standard error è uno strumento importante per segnalare gli errori durante la programmazione. Attraverso questo metodo, è possibile visualizzare messaggi di errore specifici durante l'esecuzione del codice, facilitando così la risoluzione dei problemi.

##Come fare##
Per scrivere su standard error in Swift, è necessario utilizzare la funzione `fatalError()` all'interno del codice. La sintassi è la seguente:

\n````Swift
fatalError("Messaggio di errore")
````
\nQuesta funzione interromperà immediatamente l'esecuzione del programma e stamperà il messaggio di errore su standard error. 

Un esempio pratico potrebbe essere il seguente:

````Swift
let age = -5

if age < 0 {
    fatalError("L'età non può essere un numero negativo.")
}
````
\nIn questo caso, se la variabile `age` avrà un valore negativo, il programma si interromperà e stamperà il messaggio di errore su standard error. Questo evita che il programma continui ad eseguire codice non valido e aiuta a trovare rapidamente e risolvere l'errore.

##Approfondimento##
Esistono molte altre funzioni in Swift che consentono di scrivere su standard error, come ad esempio `preconditionFailure()`, `assertionFailure()` e `debugPrint()`. Ognuna di queste funzioni ha uno scopo specifico e potrebbe essere più adatta a determinate situazioni di debug.

Inoltre, sarà necessario gestire i messaggi di errore su standard error, ad esempio utilizzando l'operatore di concatenazione `+` per visualizzare più informazioni o utilizzando il parametro `file` per specificare il file in cui è presente l'errore.

##Vedi anche##
- [Documentazione di Swift su standard error](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- [Articolo su come gestire gli errori in Swift](https://www.swiftbysundell.com/articles/error-handling-in-swift/)
- [Guida introduttiva su standard error in Swift](https://www.hackingwithswift.com/example-code/system/how-to-send-output-to-the-terminal-using-standard-error)