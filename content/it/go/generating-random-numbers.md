---
title:    "Go: Generazione di numeri casuali"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Perché

I numeri casuali sono fondamentali per molte applicazioni informatiche, inclusi giochi, simulazioni e crittografia. Imparare a generare numeri casuali può rendere il tuo codice più versatile e sicuro.

## Come Fare

Per generare numeri casuali in Go, è possibile utilizzare la funzione `rand.Intn(n)` che restituisce un intero non negativo compreso tra 0 e n-1. Ad esempio, per generare un numero casuale compreso tra 1 e 100, puoi utilizzare `rand.Intn(100) + 1`.

Per generare numeri casuali con un numero maggiore di bit, è possibile utilizzare la funzione `rand.Int()` che restituisce un intero a 64 bit. Inoltre, è possibile impostare il seed utilizzando `rand.Seed()` per ottenere numeri casuali diversi ad ogni esecuzione del programma.

Ecco un esempio di codice per generare 10 numeri casuali compresi tra 1 e 100 e stamparli a schermo:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Imposta il seed utilizzando l'ora attuale
	rand.Seed(time.Now().UnixNano())

	// Genera e stampa 10 numeri casuali
	for i := 0; i < 10; i++ {
		fmt.Println(rand.Intn(100) + 1)
	}
}
```

Ecco l'output che potresti ottenere:

```
32
89
15
47
76
98
6
50
23
65
```

## Approfondimento

La generazione di numeri casuali è un processo complesso e importante per garantire una buona distribuzione e non prevedibilità dei numeri generati. In Go, i numeri casuali sono generati utilizzando un algoritmo chiamato "pseudo-random generatore lineare congruenziale". Questo algoritmo utilizza la combinazione di una equazione matematica e il valore di "seed" per generare una successione di numeri casuali.

Per ottenere una maggiore sicurezza nei numeri casuali, è importante utilizzare un seed diverso ad ogni esecuzione del programma. Questo perché utilizzare sempre lo stesso seed può portare a una prevedibilità dei numeri generati.

## Vedi Anche

- [Articolo su generazione di numeri casuali in Go](https://tutorialedge.net/golang/go-random-number-generation/)
- [Documentazione ufficiale sulla funzione `rand` di Go](https://pkg.go.dev/math/rand)
- [Algoritmo lineare congruenziale](https://it.wikipedia.org/wiki/Generatore_lineare_congruenziale)