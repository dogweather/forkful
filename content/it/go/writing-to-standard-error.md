---
title:                "Go: Scrivere su standard di errore"
simple_title:         "Scrivere su standard di errore"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché
È importante sapere come scrivere su standard error quando si scrive in Go, poiché può essere utile per visualizzare messaggi di errore durante l'esecuzione del programma o per il debugging.

## Come Fare
Per scrivere su standard error in Go, è possibile utilizzare la funzione `fmt.Fprintln` che stampa un messaggio su un writer in modo simile alla funzione `fmt.Println`, ma invece di stampare su standard output, stamperà su standard error.

Un esempio di codice potrebbe essere il seguente:
```Go
package main

import "fmt"

func main() {
  fmt.Fprintln(os.Stderr, "Errore: il file non è stato trovato")
}
```
Il output di questo codice sarebbe "Errore: il file non è stato trovato" stampato su standard error invece che su standard output.

## Approfondimenti
Per capire meglio il concetto di standard error, è importante comprendere la differenza tra standard output e standard error. Lo standard output è il canale in cui un programma scrive i messaggi di output, mentre lo standard error è il canale in cui vengono scritti i messaggi di errore. Questo rende più semplice rilevare e gestire gli errori durante l'esecuzione di un programma.

È importante notare che in Go, gli errori vengono gestiti principalmente utilizzando il pacchetto `errors` o `fmt.Errorf`. Tuttavia, scrivere su standard error può essere utile in situazioni in cui è necessario stampare messaggi di errore personalizzati o per il debugging.

## Vedi Anche
- [Documentazione ufficiale su Go](https://golang.org/doc/)
- [Pacchetto fmt](https://golang.org/pkg/fmt/)
- [Pacchetto os](https://golang.org/pkg/os/)
- [Standard output vs standard error](https://www.linuxjournal.com/content/understanding-standard-io)