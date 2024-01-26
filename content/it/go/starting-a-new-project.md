---
title:                "Avvio di un nuovo progetto"
date:                  2024-01-20T18:03:54.281342-07:00
model:                 gpt-4-1106-preview
simple_title:         "Avvio di un nuovo progetto"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Iniziamo un nuovo progetto per trasformare idee in codice eseguibile. I programmatori lo fanno per creare software che risolva problemi specifici o per sperimentare con nuove tecnologie.

## Come fare:
Per iniziare un nuovo progetto in Go, usa il comando `go mod` per creare un nuovo modulo, seguito dall'aggiunta di un file `.go` con del codice di base. Ecco come si fa:

```Go
// Inizia una nuova sessione del terminale e naviga alla directory dove vuoi che il tuo progetto viva
// Digita il seguente comando per creare una nuova directory per il progetto
mkdir ilMioProgetto
cd ilMioProgetto

// Ora, inizializza un nuovo modulo Go chiamandolo 'github.com/tuo_username/ilMioProgetto'
go mod init github.com/tuo_username/ilMioProgetto

// Crea un nuovo file 'main.go' con il seguente contenuto base:
package main

import "fmt"

func main() {
    fmt.Println("Ciao, mondo!")
}

// Esegui il programma
go run main.go
```

Output previsto:
```
Ciao, mondo!
```

## Approfondimento
Go, noto anche come Golang, è stato creato da Google nel 2007 per migliorare la produttività nella programmazione di sistemi. Questo linguaggio semplifica la creazione di progetti affidabili e performanti. Altre opzioni per gestire i pacchetti in Go includevano `dep` e `godep`, ma ora `go mod` è lo standard raccomandato. Quando inizi un progetto con `go mod`, gestisci le dipendenze in modo più efficace e crei un ambiente chiaro per il tuo codice.

## Vedi anche
- Documentazione ufficiale Go per moduli: [https://golang.org/doc/modules/managing-dependencies](https://golang.org/doc/modules/managing-dependencies)
- "The Go Programming Language" di Alan A. A. Donovan e Brian W. Kernighan: [https://www.gopl.io/](https://www.gopl.io/)
- Tutorial Go su Tour of Go: [https://tour.golang.org/welcome/1](https://tour.golang.org/welcome/1)
