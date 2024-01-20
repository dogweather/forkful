---
title:                "Verifica se una directory esiste"
html_title:           "Go: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Verificare se una directory esiste è un processo attraverso il quale il tuo programma controlla la presenza di una directory specifica nel sistema di file. È spesso essenziale per prevenire errori e comportamenti inaspettati nel tentativo di leggere, scrivere o manipolare file in una directory inesistente.

## Come fare:

Ecco un esempio semplice utilizzando la funzione `os.Stat` nel pacchetto `os`:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	_, err := os.Stat("/path/to/directory")

	if os.IsNotExist(err) {
		fmt.Println("La directory non esiste!")
	} else {
		fmt.Println("La directory esiste!")
	}
}
```

In questo esempio, se la directory "/path/to/directory" non esiste, verrà stampata la frase "La directory non esiste!". Se invece la directory esiste, verrà stampato "La directory esiste!".

## Deep Dive
Historicamente, i programmatori Go hanno usato la funzione `os.Stat` per verificare l'esistenza di una directory. Questa funzione ritorna un errore se il file o la directory non esistono. Dopo averla usata, possiamo confermare l'esistenza della directory verificando l'assenza dell'errore.

Come alternativa, è possibile usare la funzione `os.IsNotExist`, che è una funzione di convenienza che sfrutta `os.Stat` e verifica direttamente se l'errore è di tipo 'file non trovato'.

In termini di implementazione, `os.Stat` chiama la funzione di sistema `stat`, che ritorna informazioni dettagliate su un file o una directory. Se il file o la directory non sono presenti, `stat` ritorna un errore, che viene poi gestito da `os.Stat`.

## Vedi Anche

- Documentazione ufficiale del pacchetto "os" in Go: https://golang.org/pkg/os/
- Guida di Go alla gestione degli errori: https://blog.golang.org/error-handling-and-go
- Una discussione utile su StackOverflow sugli usi comuni di `os.Stat`: https://stackoverflow.com/questions/12518876/how-to-check-if-a-file-exists-in-go