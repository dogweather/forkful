---
title:                "Scrivere un file di testo"
date:                  2024-01-19
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere un file di testo vuol dire memorizzare dati in modo leggibile. È fondamentale per salvare configurazioni, log, e per scambiare informazioni tra programmi e utenti.

## How to:
```Go
package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    file, err := os.Create("esempio.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := bufio.NewWriter(file)
    _, err = writer.WriteString("Ciao, mondo!\n")
    if err != nil {
        panic(err)
    }

    // Assicurati che tutto sia scritto sul disco
    writer.Flush()
}

// Output nel file 'esempio.txt'
// Ciao, mondo!
```

## Deep Dive:
La scrittura su file è pratica antica quanto la programmazione. Prima, avveniva su nastri magnetici o cartelloni perforati. Oggi abbiamo alternative come database e archiviazione nel cloud, ma i file di testo restano utili per la loro semplicità e portabilità. Go usa internamente buffer per ottimizzare la scrittura su disco.

## See Also:
- Documentazione ufficiale su come scrivere su file in Go: https://golang.org/pkg/os/ e https://golang.org/pkg/bufio/
- Esempi di scrittura di file nei linguaggi di programmazione storici per confronto: https://en.wikipedia.org/wiki/Hello_world_program_examples
- Approfondimenti sul file system di Go e gestione errori: https://blog.golang.org/go1.13-errors
