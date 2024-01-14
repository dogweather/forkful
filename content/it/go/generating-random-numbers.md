---
title:    "Go: Generazione di numeri casuali"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è una delle funzioni più utili e interessanti della programmazione. Può essere utile in molti scenari, come la generazione di password sicure o l'implementazione di algoritmi di gioco.

## Come fare

Per iniziare, è necessario importare il pacchetto "math/rand" per accedere alle funzioni di generazione dei numeri casuali.

```
package main

import (
   "fmt"
	"math/rand"
)
```

Per generare un numero intero casuale, si può utilizzare la funzione "Intn (n)", dove "n" è il valore massimo del numero che si desidera generare. Ad esempio, per generare un numero casuale tra 1 e 100:

```
rand.Seed(time.Now().UnixNano())
fmt.Println(rand.Intn(100))
```

Per generare un numero decimale casuale tra 0 e 1, è possibile utilizzare la funzione "Float64 ()". Ad esempio:

```
fmt.Println(rand.Float64())
```

Si possono anche generare numeri casuali all'interno di un range specifico utilizzando la funzione "Intn (max-min) + min". Ad esempio, per generare un numero casuale tra 10 e 20:

```
fmt.Println(rand.Intn(20-10) + 10)
```

## Approfondimento

La generazione di numeri casuali in realtà è fatta utilizzando algoritmi algoritmici che producono una sequenza di numeri pseudo-casuali. Ciò significa che, se si utilizza lo stesso seme (seed) per inizializzare il generatore, si otterrà la stessa sequenza di numeri.

Per evitare questo, è importante inizializzare il generatore utilizzando un seme diverso ogni volta. Come nel codice di esempio sopra, si può utilizzare il tempo corrente come seme utilizzando "rand.Seed (time.Now (). UnixNano ())", che utilizza i nanosecondi correnti come seme.

In alternativa, si può anche fornire un seme personalizzato utilizzando "rand.Seed (s)" dove "s" è un numero intero.

## Vedi anche

- Documentazione ufficiale di Go sulla generazione di numeri casuali: https://golang.org/pkg/math/rand/
- Un tutorial dettagliato sulla generazione di numeri casuali in Go: https://www.calhoun.io/creating-random-numbers-in-go/
- Un esempio di utilizzo dei numeri casuali in un gioco scritto in Go: https://medium.com/@jcox250/generating-random-numbers-in-go-eef32e95999c