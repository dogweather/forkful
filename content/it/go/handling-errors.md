---
title:                "Gestione degli errori"
date:                  2024-01-26T00:52:40.128174-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

La gestione degli errori in Go consiste nel rilevare e rispondere con grazia a problemi di esecuzione. Lo facciamo per prevenire crash e assicurarci che i nostri programmi si comportino in modo prevedibile, anche quando le cose non vanno per il verso giusto.

## Come fare:

Go utilizza una gestione degli errori esplicita. Ciò significa che controllerai se una funzione restituisce un errore ogni volta che la chiami. Niente eccezioni. Ecco come si presenta:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Uh oh:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// Facciamo finta che qualcosa sia andato storto
	return fmt.Errorf("si è verificato un errore")
}
```

Esegui questo e otterrai:

```
Uh oh: si è verificato un errore
```

Ma se va a buon fine?

```Go
func doSomething() error {
	// Tutto bene questa volta
	return nil
}
```

Nessun output. Bene, nessuna notizia è una buona notizia.

## Approfondimento:

In Go, la gestione degli errori è stata un punto di contesa. Fin dall'inizio, Go ha deciso di non utilizzare eccezioni per un approccio più esplicito, che alcuni sviluppatori apprezzano per la sua semplicità e altri trovano verboso. Il tipo incorporato `error` è un'interfaccia. Qualsiasi tipo con un metodo `Error() string` lo soddisfa. Questo è in linea con l'etica di Go della semplicità ed esplicità.

Alternative? Ci sono il duo `panic` e `recover`, ma sono per casi eccezionali (gioco di parole inteso) quando il programma non può proseguire. Pensa a `panic` come al bottone di espulsione che premi quando sai che non c'è ritorno. Usalo con parsimonia.

Per quanto riguarda la gestione degli errori mainstream, Go 1.13 ha introdotto l'error wrapping, rendendo più facile capire la "catena di errori" con funzioni come `errors.Is()` e `errors.As()`.

## Vedi Anche:

Per tutto ciò che riguarda la gestione degli errori in Go:

- Il blog di Go sulla gestione degli errori: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Go efficace – Sezione sulla gestione degli errori: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Documentazione sull'Error Wrapping di Go 1.13: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Il post di Dave Cheney sulle strategie di gestione degli errori: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)