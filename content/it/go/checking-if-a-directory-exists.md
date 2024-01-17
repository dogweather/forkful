---
title:                "Verifica dell'esistenza di una cartella"
html_title:           "Go: Verifica dell'esistenza di una cartella"
simple_title:         "Verifica dell'esistenza di una cartella"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Verificare se una directory esiste è un'azione comune nella programmazione, in quanto consente ai programmatori di controllare l'esistenza di una directory prima di eseguire ulteriori operazioni su di essa. Ad esempio, un programma potrebbe dover creare una nuova directory se questa non esiste già.

## Come si fa:
Il codice seguente mostra come verificare se una directory esiste utilizzando Go:

```Go
pacchetto principale

import "fmt"
import "os"

func main() {
    // directory da verificare
    dir := "cartella_esistente"

    // utilizziamo la funzione os.Stat per accedere alle informazioni sulla directory
    _, err := os.Stat(dir)

    // controlliamo se si è verificato un errore
    if os.IsNotExist(err) {
        fmt.Println("La directory", dir, "non esiste.")
    } else {
        fmt.Println("La directory", dir, "è presente.")
    }
}
```

Ecco un esempio di output:

```bash
$ go run main.go
La directory cartella_esistente è presente.
```

## Dive profonda:
In passato, la verifica delle directory esistenti era spesso fatta utilizzando la funzione `os.Open`, ma questa approccio è considerato obsoleto e viene ora sconsigliato. Un'alternativa valida è la funzione `os.Stat`, che restituisce informazioni sulla directory specificata. È anche possibile utilizzare il pacchetto `path/filepath` per accedere ai metodi di verifica delle directory.

## Vedi anche:
- [Documentazione ufficiale su os.Stat](https://golang.org/pkg/os/#Stat)
- [Documentazione ufficiale su path/filepath](https://golang.org/pkg/path/filepath/)
- [Articolo su come verificare se un file esiste in Go](https://www.digitalocean.com/community/tutorials/how-to-check-if-a-file-or-directory-exists-in-go-it)