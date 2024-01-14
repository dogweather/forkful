---
title:                "Go: Verifica se una directory esiste"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si sviluppa un programma, può essere necessario verificare se una directory esiste o meno. Questo può essere utile per garantire che il programma funzioni correttamente o per gestire situazioni di errore.

## Come fare

Per verificare se una directory esiste in Go, possiamo utilizzare la funzione `os.Stat()` che restituisce un `os.FileInfo` se la directory esiste o un errore se non esiste. Possiamo poi controllare se l'errore è nil per determinare se la directory esiste o meno.

Ecco un esempio di codice che utilizza questa funzione:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Controlliamo se la directory "documents" esiste
	if _, err := os.Stat("documents"); err == nil {
		fmt.Println("La directory 'documents' esiste")
	} else if os.IsNotExist(err) { // Se l'errore è di tipo NotExist, la directory non esiste
		fmt.Println("La directory 'documents' non esiste")
	} else { // Altrimenti, c'è stato un errore di sistema
		fmt.Println("Errore:", err)
	}
}
```

Esempio di output: `La directory 'documents' esiste`

## Approfondimento

La funzione `os.Stat()` fa parte del pacchetto `os` di Go e restituisce informazioni su un file o una directory. Se viene utilizzata su una directory, restituirà una `os.FileInfo` contenente informazioni come il nome, le dimensioni e i permessi della directory.

Un altro modo per verificare l'esistenza di una directory potrebbe essere utilizzare la funzione `os.IsExist()` che controlla se l'errore restituito da `os.Stat()` è di tipo Exists. Questo può essere utile se si vuole gestire le directory già esistenti in modo diverso dalle directory che non esistono.

## Vedi anche

- Documentazione ufficiale di `os` in Go: https://golang.org/pkg/os/
- Tutorial su come gestire i file e le directory in Go: https://www.digitalocean.com/community/tutorials/how-to-manage-files-and-directories-in-go-italian