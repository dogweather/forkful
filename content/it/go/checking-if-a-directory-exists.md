---
title:    "Go: Verifica se una directory esiste"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Perché
Spesso, quando si lavora con file e cartelle, è necessario verificare se una determinata cartella esiste o meno. Questo può essere utile per evitare errori durante l'esecuzione del programma o per gestire logicamente i file e le cartelle nel tuo codice.

## Come
Per controllare se una cartella esiste in Go, possiamo utilizzare la funzione `os.Stat()` e controllare se viene restituito un errore o meno. Se l'errore è `nil`, allora la cartella esiste, altrimenti non esiste.

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Definiamo il percorso della cartella che vogliamo controllare
	path := "/percorso/esempio"

	// Utilizziamo la funzione os.Stat() per verificare se la cartella esiste
	_, err := os.Stat(path)
	if err != nil {
		// Se viene restituito un errore, la cartella non esiste
		fmt.Println("La cartella non esiste")
	} else {
		// Altrimenti, la cartella esiste
		fmt.Println("La cartella esiste")
	}
}
```

Esempio di output nel caso in cui la cartella esiste:
```
La cartella esiste
```

Esempio di output nel caso in cui la cartella non esiste:
```
La cartella non esiste
```

## Deep Dive
La funzione `os.Stat()` restituisce anche informazioni sulla cartella come il nome, il percorso, la dimensione e i permessi. Possiamo utilizzare queste informazioni per fare operazioni più avanzate sulle cartelle, come ad esempio spostarle, rinominarle o eliminare quelle che non ci servono più.

Inoltre, possiamo utilizzare la funzione `os.Mkdir()` per creare una nuova cartella e la funzione `os.Chdir()` per cambiare la cartella di lavoro corrente.

## Vedi anche
- Documentazione ufficiale di Go su `os.Stat()`: https://golang.org/pkg/os/#Stat
- Tutorial su come gestire file e cartelle in Go: https://www.digitalocean.com/community/tutorials/how-to-work-with-files-and-directories-in-go-italiano