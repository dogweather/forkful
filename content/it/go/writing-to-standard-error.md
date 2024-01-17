---
title:                "Scrivere su standard error"
html_title:           "Go: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere su standard error è un concetto fondamentale nella programmazione professionale. Si tratta di un'operazione che consente ai programmatori di mostrare messaggi di errore e informazioni di debug durante l'esecuzione del codice. È una pratica importante per risolvere eventuali errori e migliorare l'affidabilità e l'efficienza del proprio codice.

## Come fare:

Ecco un semplice esempio di codice in Go che mostra come scrivere su standard error utilizzando la funzione `Fprintln`:

```
// importa il pacchetto "fmt" per utilizzare la funzione Fprintln
import "fmt"

func main() {

    // scrivi "Ciao Mondo" su standard error
    fmt.Fprintln(os.Stderr, "Ciao Mondo")
}
```

Ogni volta che questo codice viene eseguito, verrà stampato il messaggio "Ciao Mondo" su standard error, che di solito è il terminale del sistema.

## Approfondimento:

Scrivere su standard error è una pratica comune nella programmazione moderna. In passato, i programmatori utilizzavano spesso la funzione `printf` per scrivere messaggi di debug e di errore. Tuttavia, l'utilizzo di questa funzione può causare problemi in determinate situazioni, come ad esempio durante il debugging di un programma che utilizza la funzione `printf` per la sua normale esecuzione.

Un'alternativa all'utilizzo della funzione `Fprintln` è l'utilizzo della funzione `Fatal`, che stampa un messaggio di errore e termina immediatamente l'esecuzione del programma. Questa funzione è particolarmente utile quando si desidera interrompere l'esecuzione di un programma in caso di un errore critico.

Per quanto riguarda l'implementazione di scrivere su standard error, ci sono diversi aspetti da considerare, come il modo in cui il sistema operativo gestisce l'output su standard error e le possibili differenze tra i diversi linguaggi di programmazione.

## Vedi anche:

- [Documentazione ufficiale di Go: fmt](https://golang.org/pkg/fmt/)
- [Stack Overflow: Difference between Fmt.Fprintln(os.Stderr) and Fmt.Println()?](https://stackoverflow.com/questions/38036438/difference-between-fmt-fprintlnos-stderr-and-fmt-println)
- [Tutorial: Debugging Go Code with Standard Error](https://blog.golang.org/using-go-x-tools-with-language-server)