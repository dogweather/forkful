---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Go: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La lettura degli argomenti della riga di comando è una pratica comune tra i programmatori Go, che consente loro di accedere a variabili e impostazioni specifiche fornite dall'utente durante l'esecuzione del programma. Ciò può essere particolarmente utile per creare applicazioni interattive o personalizzabili.

## Come:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Utilizziamo la funzione os.Args per accedere agli argomenti della riga di comando
	args := os.Args[1:]

	// Stampiamo gli argomenti uno alla volta
	for _, arg := range args {
		fmt.Println(arg)
	}
}
```

Input: ```go run main.go hello world```

Output: 
```
hello
world
```

## Approfondimento:

La lettura degli argomenti della riga di comando è una funzionalità fondamentale dei linguaggi di programmazione moderni e ha una lunga storia dietro di sé. Alcune alternative alla lettura degli argomenti includono l'utilizzo di file di configurazione o l'interazione con l'utente attraverso le interfacce grafiche. In Go, la funzione os.Args è implementata attraverso l'utilizzo di una slice che contiene tutti gli argomenti forniti dall'utente.

## Vedi anche:

- [Documentazione ufficiale di Go su os.Args](https://golang.org/pkg/os/#Args)
- [Un articolo sulle best practice per la lettura degli argomenti della riga di comando in Go](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-golang)