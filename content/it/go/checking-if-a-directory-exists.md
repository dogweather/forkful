---
title:    "Go: Verifica se una cartella esiste."
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
Capita spesso di dover creare un programma che gestisca file e cartelle presenti sul nostro computer. In queste situazioni è importante sapere se una determinata cartella esiste o meno, per poter intraprendere azioni appropriate nel nostro codice.

## Come fare
Per fortuna, il linguaggio di programmazione Go fornisce una semplice e affidabile funzione per verificare l'esistenza di una cartella. Utilizzando il pacchetto `os` e il metodo `Stat()`, possiamo ottenere informazioni sul file o sulla cartella specificata e verificare se esiste attraverso il campo `IsDir()`.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    path := "./cartella"
    fileInfo, err := os.Stat(path)
    if err != nil {
        fmt.Println("Errore: cartella non esistente")
    } else {
        if fileInfo.IsDir() {
            fmt.Println("La cartella esiste")
        } else {
            fmt.Println("La cartella non esiste")
        }
    }
}
```

L'output di questo programma sarà "La cartella esiste", se la cartella specificata è effettivamente presente.

## Approfondimento
Per una comprensione più approfondita, possiamo esplorare il pacchetto `os` e il suo metodo `Stat()` in modo dettagliato. Questo metodo restituisce un oggetto di tipo `FileInfo` che contiene informazioni sulla cartella o file specificato, tra cui la dimensione, le autorizzazioni e la data di ultima modifica.

La funzione `IsDir()` di questo oggetto ci consente di verificare se il percorso specificato corrisponde a una cartella o a un file. Inoltre, possiamo anche utilizzare il metodo `IsNotExist()` per capire se il percorso specificato non esiste affatto.

Utilizzando questi metodi possiamo gestire facilmente le situazioni in cui una cartella o un file non esiste nel nostro codice.

## Vedi anche
- [Documentazione ufficiale del pacchetto os](https://golang.org/pkg/os/)
- [Tutorial su come gestire file e cartelle in Go](https://ednsquare.com/story/handling-files-in-golang------basic-use-cases------uvKLUe)
- [Altro esempio di verifica esistenza di una directory in Go](https://golang.hotexamples.com/examples/os/-/FileInfo/IsDir/go-fileinfo-isdir-function-examples.html)