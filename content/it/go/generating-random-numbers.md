---
title:                "Generazione di numeri casuali"
html_title:           "Go: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
 Generare numeri casuali è un'attività comune per i programmatori, che consiste nel produrre numeri casuali all'interno di un determinato intervallo. Questo viene spesso utilizzato per scopi di test, simulazione o generazione di dati casuali per applicazioni come i giochi.
 
 
## How to:
Il linguaggio di programmazione Go fornisce una funzione incorporata per generare numeri casuali, chiamata ```rand.Intn(n)```. Questa funzione restituisce un numero intero casuale compreso tra 0 e n. Di seguito un esempio di codice: 

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	//genera un numero casuale tra 0 e 100 e lo salva nella variabile "numero"
	numero := rand.Intn(100)
	fmt.Println("Il numero casuale è:", numero)
}
```

Esempio di output:

```
Il numero casuale è: 73
```

## Deep Dive:
La generazione di numeri casuali è un concetto importante nella programmazione e ha una lunga storia. Nel passato, venivano utilizzati diversi algoritmi per generare numeri casuali, spesso basati su calcoli matematici complessi o addirittura su parametri fisici esterni, come il rumore ambientale. Oggi, la maggior parte dei linguaggi di programmazione, incluso Go, offre funzioni semplici e affidabili per generare numeri casuali.

Esistono anche alternative alla funzione ```rand.Intn(n)```. Ad esempio, è possibile utilizzare ```rand.Float64()``` per generare numeri casuali decimali tra 0 e 1. Inoltre, il pacchetto rand di Go fornisce anche funzioni per la generazione di numeri casuali di altri tipi, come interi a 64 bit o numeri booleani.

La generazione di numeri casuali in Go utilizza un algoritmo chiamato "linear congruential generator". Questo metodo è abbastanza veloce e produce una buona distribuzione di numeri casuali, ma potrebbe non essere adatto per applicazioni che richiedono un alto grado di casualità, come la crittografia. In questi casi, è consigliabile utilizzare funzioni più complesse e sicure per la generazione di numeri casuali.

## See Also:
Per ulteriori informazioni sulla generazione di numeri casuali in Go, è possibile consultare la documentazione ufficiale del linguaggio su [golang.org](https://golang.org/pkg/math/rand/). Inoltre, ci sono numerose risorse online che approfondiscono il concetto di numeri casuali e come implementarli in altri linguaggi di programmazione.