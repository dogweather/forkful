---
title:                "Verifica dell'esistenza di una directory"
html_title:           "Go: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Quando si scrive un programma, spesso può essere utile verificare se una directory esiste prima di eseguire operazioni su di essa. Questo può aiutare a evitare errori e a garantire che il programma funzioni correttamente.

## Come fare

Per verificare se una directory esiste in Go, è possibile utilizzare il metodo `os.Stat()` e controllare se si verifica un errore. Se non si verifica alcun errore, significa che la directory esiste.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Definiamo il path della directory da controllare
    directory := "example"

    // Utilizziamo il metodo os.Stat() per controllare se la directory esiste
    _, err := os.Stat(directory)

    // Se si verifica un errore, significa che la directory non esiste
    if err != nil {
        fmt.Println("La directory", directory, "non esiste")
        return
    }

    // Se non si verifica alcun errore, la directory esiste
    fmt.Println("La directory", directory, "esiste")
}
```

Output:

```
La directory example esiste
```

## Approfondimento

Il metodo `os.Stat()` accetta come parametro il path del file o della directory da controllare e restituisce un oggetto `FileInfo` contenente informazioni sul file o sulla directory. Se si verifica un errore, significa che il file o la directory non esistono.

Inoltre, è possibile utilizzare il metodo `os.IsNotExist()` per verificare specificatamente se il file o la directory non esistono. In questo caso, il metodo restituisce `true`.

È importante notare che il metodo `os.Stat()` verifica solo se la directory esiste e non se è possibile accedervi o modificarla. Per questo, potrebbe essere necessario verificare anche i permessi prima di utilizzare la directory nel programma.

## Vedi anche

- [Documentazione di Go sull'utilizzo dei file e delle directory](https://golang.org/pkg/os/#Filesystem_Interfaces)
- [Articolo su come verificare se un file esiste in Go](https://dev.to/shindakun/check-if-a-file-exists-in-go-87g)