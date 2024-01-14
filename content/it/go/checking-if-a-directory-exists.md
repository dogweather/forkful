---
title:                "Go: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché
La verifica dell'esistenza di una directory è uno degli aspetti fondamentali nella gestione dei file e delle risorse di sistema. Conoscere gli strumenti corretti per eseguire questa operazione è importante per garantire che il codice sia robusto e in grado di gestire eventuali errori.

## Come fare
In Go, è possibile utilizzare la funzione predefinita `os.Stat()` per verificare se una determinata directory esiste o meno. Di seguito è riportato un esempio di codice che utilizza questa funzione:

```Go
import (
    "fmt"
    "os"
)

func main() {
    // Path della directory da verificare
    dirPath := "/Users/nomeutente/Desktop/"

    // Utilizzo della funzione os.Stat() per verificare l'esistenza della directory
    _, err := os.Stat(dirPath)

    // Controllo dell'eventuale errore
    if err != nil {
        // Se l'errore è di tipo 'file o directory non trovato', la directory non esiste
        if os.IsNotExist(err) {
            fmt.Printf("La directory %s non esiste\n", dirPath)
        } else {
            // Altrimenti, si è verificato un qualche altro errore
            fmt.Printf("Errore durante la verifica dell'esistenza della directory: %s\n", err)
        }
    } else {
        // L'esecuzione prosegue con la directory esistente
        fmt.Printf("La directory %s esiste\n", dirPath)
    }
}
```

Esempio di output:

```
La directory /Users/nomeutente/Desktop/ esiste
```

## Approfondimento
Quando si utilizza la funzione `os.Stat()` per verificare l'esistenza di una directory, è importante tenere presente che questa funzione restituisce un errore solo se la directory non esiste o se si è verificato un qualche altro errore durante la verifica. Questo significa che, in alcuni casi, una directory potrebbe esistere effettivamente ma potrebbe comunque essere restituito un errore.

Una possibile causa di questo comportamento è dovuta alla mancanza di permessi di lettura sulla directory da parte dell'utente che sta eseguendo il programma. In questi casi, la funzione `os.Stat()` restituirà un errore di tipo `permission denied` anche se la directory esiste effettivamente. Assicurarsi di avere i permessi corretti prima di verificare l'esistenza di una directory può essere una soluzione a questo problema.

## Vedi anche
- Documentazione ufficiale di Go sulla funzione `os.Stat()`: [https://golang.org/pkg/os/#Stat](https://golang.org/pkg/os/#Stat)
- Altri modi per verificare l'esistenza di una directory in Go: [https://golangcode.com/check-if-a-file-exists/](https://golangcode.com/check-if-a-file-exists/)