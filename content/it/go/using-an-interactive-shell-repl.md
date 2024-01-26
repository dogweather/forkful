---
title:                "Utilizzo di un interprete interattivo (REPL)"
date:                  2024-01-26T04:14:27.687168-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un interprete interattivo (REPL)"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Un REPL (Read-Eval-Print Loop, Ciclo Leggi-Valuta-Stampa) consente di interagire in tempo reale con il codice; legge l'input, lo valuta, stampa il risultato e ricomincia. I programmatori lo utilizzano per testare frammenti di codice, eseguire debug e imparare nuovi linguaggi in tempo reale.

## Come fare:
Go non include un REPL incorporato, ma è possibile utilizzare strumenti di terze parti. Uno strumento popolare è `gore`:

```go
// Installa gore utilizzando
$ go install github.com/motemen/gore/cmd/gore@latest

// Esegui gore
$ gore
gore version 0.5.0  :help per aiuto
gore> :import fmt
gore> fmt.Println("Ciao, Go REPL!")
Ciao, Go REPL!
nil
```

## Approfondimento
Originariamente sviluppati per Lisp, i REPL sono comuni in linguaggi dinamici come Python o Ruby. Go, essendo un linguaggio staticamente tipizzato, non ne include uno nativamente. Alternative a `gore` includono `go-pry` e `yaegi`. Questi strumenti interpretano il codice Go, permettendo di esplorare e validare idee rapidamente senza dover compilare un'applicazione completa. Sono particolarmente utili per principianti e in contesti educativi dove l'obiettivo è imparare e sperimentare.

## Vedi anche
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)