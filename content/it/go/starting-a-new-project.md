---
title:                "Iniziare un nuovo progetto"
html_title:           "Arduino: Iniziare un nuovo progetto"
simple_title:         "Iniziare un nuovo progetto"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Avviare un nuovo progetto di programmazione significa creare da zero una nuova applicazione o un nuovo software. Lo facciamo per risolvere problemi, per migliorare i processi esistenti o per creare nuovi prodotti.

## Come Fare:

Ora ti mostrerò come iniziare un nuovo progetto in Go. Inizieremo con un "Hello World" semplice.

```Go
package main
import "fmt"
func main() {
    fmt.Println("Ciao, Mondo!")
}
```

Ecco quello che vedrai:

```
$ go run main.go
Ciao, Mondo!
```

## Approfondimento 

Go venne creato da Robert Griesemer, Rob Pike e Ken Thompson alla Google nel 2007 per risolvere problemi di scalabilità e produttività nell'industria del software. È una lingua alternativa a C++ e Java, che ti permette di scrivere codice conciso, semplice e allo stesso tempo ad alte prestazioni.

Prima di Go, iniziare un nuovo progetto significava configurare manualmente l'ambiente di sviluppo, le librerie, i compilatori, ecc. Ora, con Go, tutto ciò si è semplificato.

Gli strumenti di Go, come `go build` e `go run`, hanno semplificato il processo di avvio di un progetto. `go mod init`, ad esempio, crea un nuovo modulo, aggiungendo un nuovo file go.mod che definisce il nome del modulo, la sua versione e le sue dipendenze.

## Vedi Anche

1. [Tutorial Ufficiale della Lingua Go](https://tour.golang.org/welcome/1)
2. [Go Project Layout](https://github.com/golang-standards/project-layout)
3. [A Tour of Go](https://tour.golang.org/welcome/1)