---
title:                "Go: Scrittura su errore standard"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per scrivere su standard error durante lo sviluppo del codice Go. Questo può aiutare a identificare e risolvere rapidamente eventuali errori o bug nel tuo programma, rendendo il processo di debugging più efficiente. Inoltre, può essere utile per registrare informazioni importanti durante l'esecuzione del programma, in modo da poterle analizzare più tardi.

## Come fare

Scrivere su standard error in Go è molto semplice. Puoi utilizzare il pacchetto "os" per accedere allo stream di standard error e utilizzare la funzione "fmt.Fprintln" per stampare il tuo messaggio su di esso. Ecco un esempio di codice:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Fprintln(os.Stderr, "Questo è un messaggio di errore!")
}
```

L'output di questo programma sarà: "Questo è un messaggio di errore!" su standard error.

## Approfondimento

Scrivere su standard error può essere utile anche quando si lavora con goroutine o routine concorrenti. Invece di stampare i messaggi su standard output, che può risultare confusionario, puoi inviarli su standard error per identificare più facilmente quale goroutine ha prodotto quale output.

Inoltre, puoi anche specificare un indirizzo di file per il pacchetto "os" per scrivere i messaggi di errore su un file invece di visualizzarli in modo immediato. Ciò può essere utile per il debugging di programmi più complessi.

## Vedi anche

- Documentazione ufficiale del pacchetto "os": https://golang.org/pkg/os/
- Documentazione ufficiale del pacchetto "fmt": https://golang.org/pkg/fmt/
- Tutorial di Go su scrivere su standard error: https://gobyexample.com/writing-files