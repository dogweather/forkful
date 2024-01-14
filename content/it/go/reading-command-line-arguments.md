---
title:                "Go: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché
##### Perché leggere gli argomenti della riga di comando?

Ci sono molte ragioni per leggere gli argomenti della riga di comando in un programma Go. Ad esempio, potresti voler permettere all'utente di personalizzare l'esecuzione del tuo programma specificando alcuni parametri al momento dell'avvio. Oppure potresti avere bisogno di leggere un file di configurazione fornito come argomento della riga di comando.

## Come
##### Esempi di codice e output per leggere gli argomenti della riga di comando

Per prima cosa, dobbiamo importare il pacchetto `os` che ci permette di accedere agli argomenti della riga di comando:

```Go
import "os"
```

Per leggere gli argomenti della riga di comando, possiamo utilizzare la funzione `Args()` del pacchetto `os`:

```Go
args := os.Args
```

Se vogliamo leggere un argomento specifico, possiamo farlo utilizzando l'indice tra parentesi quadre:

```Go
arg1 := args[1] // legge il secondo argomento
```

Possiamo anche utilizzare la funzione `Len()` per conoscere il numero totale di argomenti passati:

```Go
numArgs := len(args) // restituisce il numero totale di argomenti
```

Ecco un esempio completo di programma Go che legge e stampa gli argomenti della riga di comando:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args

    fmt.Println("Numero totale di argomenti:", len(args))

    fmt.Println("Argomenti:")
    for i, arg := range args {
        fmt.Println(i, ":", arg)
    }
}
```

Esempio di output:

```shell
$ go run main.go hello world
Numero totale di argomenti: 3
Argomenti:
0 : /var/folders/_q/46zn21y52m32kbrkj96yjk680000gq/T/go-build1355320921/b001/exe/main
1 : hello
2 : world
```

## Deep Dive
##### Informazioni più dettagliate sui comandi della riga di comando

Oltre alla funzione `Args()` del pacchetto `os`, esistono anche altri modi per leggere gli argomenti della riga di comando in Go. Ad esempio, si può utilizzare il pacchetto `flag` per definire e leggere flag e opzioni da riga di comando in modo più strutturato.

Inoltre, è importante ricordare che gli argomenti della riga di comando possono includere sia opzioni che argomenti posizionali. È necessario gestire entrambi i tipi in modo adeguato per garantire una corretta lettura dei comandi.

## Vedi anche
- [Documentazione ufficiale sulla gestione degli argomenti della riga di comando in Go](https://golang.org/pkg/os/#pkg-functions)
- [Tutorial su come leggere e gestire gli argomenti della riga di comando in Go](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-golang)
- [Esempio di utilizzo del pacchetto `flag` per gestire gli argomenti della riga di comando in Go](https://golangdocs.com/go-flag-package)