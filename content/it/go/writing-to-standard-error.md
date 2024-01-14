---
title:    "Go: Scrivere su standard error"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Perché

Scrivere a standard error è un'opzione utile quando si desidera visualizzare messaggi di errore o di avviso durante l'esecuzione di un programma Go. Invece di interrompere l'esecuzione del programma, i messaggi verranno visualizzati in modo sicuro nella console.

## Come Fare

Per scrivere a standard error in Go, è necessario utilizzare il pacchetto `fmt` e la sua funzione `Fprintf`, specificando `os.Stderr` come primo argomento. Ad esempio:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Fprintf(os.Stderr, "Questo è un messaggio di errore\n")
}
```

Questo produrrà un output simile a questo:

```
Questo è un messaggio di errore
```

## Approfondimento

Utilizzare `os.Stderr` consente di garantire che i messaggi di errore vengano visualizzati anche se il programma viene indirizzato verso un file o un'altra destinazione. Inoltre, è possibile utilizzare `fmt.Fprintf` per formattare i messaggi di errore con variabili.

Un esempio di utilizzo avanzato potrebbe essere il seguente:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    name := "Gianluca"
    age := 30
    fmt.Fprintf(os.Stderr, "Ciao, mi chiamo %s e ho %d anni\n", name, age)
}
```

Questo produrrà un output simile a questo:

```
Ciao, mi chiamo Gianluca e ho 30 anni
```

## Vedi Anche

- [Documentazione ufficiale di Go sull'utilizzo di fmt](https://golang.org/pkg/fmt/)
- [Articolo su Medium sul passaggio di output a standard error in Go](https://medium.com/@spencerwooo/go-how-to-print-to-stderr-30b73985b255)
- [Video tutorial su YouTube sull'utilizzo di fmt e os.Stderr in Go](https://www.youtube.com/watch?v=ne7oUHQvXi0)