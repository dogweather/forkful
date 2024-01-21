---
title:                "Lettura degli argomenti della riga di comando"
date:                  2024-01-20T17:56:07.668122-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Leggere gli argomenti da linea di comando significa accedere ai dati inseriti un utente al momento dell'esecuzione del programma. I programmatori lo fanno per personalizzare l'esecuzione o per richiedere all'utente specifici input senza un'intefaccia grafica.

## How to (Come fare):
```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    args := os.Args[1:] // Ignora il nome del programma
    fmt.Println("Argomenti da linea di comando:", args)

    for i, arg := range args {
        fmt.Printf("Argomento %d: %s\n", i+1, arg)
    }
}

/*
Esempio di output se eseguito come:
$ go run main.go pizza pasta tiramisù

Argomenti da linea di comando: [pizza pasta tiramisù]
Argomento 1: pizza
Argomento 2: pasta
Argomento 3: tiramisù
*/
```

## Deep Dive (Approfondimento)
Prima di Go, linguaggi come C e Bash permettevano già l'accesso agli argomenti da linea di comando. In Go, `os.Args` è uno slice che tiene gli argomenti: `os.Args[0]` è il nome del programma stesso, mentre `os.Args[1:]` sono gli argomenti passati dall'utente. Altre opzioni includono il package `flag` per opzioni più complesse e pacchetti di terze parti come `cobra` o `urfave/cli`, che offrono funzionalità avanzate per gestire i comandi.

Go usa internamente la variabile `os.Args` per passare i parametri. I parametri passati vengono inseriti nello slice come sequenza di stringhe, lasciando al programmatore la responsabilità della conversione in tipi più specifici se necessario.

## See Also (Vedi Anche)
- Documentazione ufficiale di Go per `flag`: https://golang.org/pkg/flag/
- Pacchetto `cobra`: https://github.com/spf13/cobra
- Pacchetto `urfave/cli`: https://github.com/urfave/cli
- Tutorial su "How to use command line arguments in Go": https://www.digitalocean.com/community/tutorials/how-to-use-command-line-arguments-in-go