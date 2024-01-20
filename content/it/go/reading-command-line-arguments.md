---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere gli argomenti della riga di comando significa accedere ai parametri di input che l'utente fornisce quando esegue un programma. Questo è utile per personalizzare l'operazione del programma senza dover modificare il codice sorgente.

## Come fare:
Di seguito è presentato un semplice programma di esempio in Go che legge gli argomenti della riga di comando:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	cmdLineArgs := os.Args
	for index, arg := range cmdLineArgs {
		fmt.Printf("arg %d: %s\n", index, arg)
	}
}
```

Se eseguiamo questo programma con `go run main.go arg1 arg2`, l'output sarà:

```Go
arg 0: main.go
arg 1: arg1
arg 2: arg2
```

## Approfondimento
La funzione `os.Args` in Go è ereditata dal modello di riga di comando UNIX, che risale agli anni '70. Questo approccio è tuttora comunemente utilizzato, nonostante esistano alternative più moderne come le librerie per il parsing della riga di comando.

La funzione `os.Args` restituisce una slice di stringhe. Il primo elemento, `os.Args[0]`, è sempre il nome del file eseguibile. Gli argomenti della riga di comando seguono l'ordine in cui sono stati forniti.

Se hai bisogno di funzionalità avanzate come flag e opzioni con nome, potresti considerare l'uso di pacchetti come flag o cobra.

## Vedere anche:
Per informazioni più dettagliate e consigli sull'uso degli argomenti della riga di comando in Go, si consiglia di consultare le seguenti risorse:

- Documentazione ufficiale di Go su `os.Args`: https://pkg.go.dev/os#pkg-variables
- Guida alla libreria di flag di Go: https://gobyexample.com/command-line-flags
- Documentazione di Cobra, una popolare libreria per le linee di comando: https://github.com/spf13/cobra
- Il modello di riga di comando UNIX: https://en.wikipedia.org/wiki/Command-line_interface#Arguments