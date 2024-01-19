---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?

Generare numeri casuali è l'atto di produrre numeri in modo imprevedibile. I programmatori lo fanno per simulare eventi reali, per esempio, per giochi di sorteggio o criptografia.

## Come si fa:

Proporremo il codice di generazione di un numero casuale tra 0 e 100.

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println(rand.Intn(100))
}
```
Dopo l'esecuzione, produrrà un numero casuale tra 0 e 100 come output, ad esempio, "42".

## Approfondimento

Storicamente, la generazione di numeri casuali era un problema matematico e filosofico prima che i computer esistessero. Dopodiché è diventato un problema di hardware e software.

Se si necessita un numero casuale che non cambi ogni volta che si esegue il programma, si può usare `rand.Seed(1)`.

In Go, si usa `rand.Seed(time.Now().UnixNano())` per generare numeri casuali. Questo metodo usa il tempo corrente in nanosecondi dall'epoch come seme per il generatore di numeri pseudo-casuali.

## Vedi Anche

- Documentazione ufficiale della funzione `rand` Go: https://golang.org/pkg/math/rand/
- Maggiori informazioni sulla generazione di numeri casuali: https://it.wikipedia.org/wiki/Numero_casuale
- Generare numeri casuali in altri linguaggi di programmazione: https://www.w3schools.com/python/ref_random_randint.asp (Python), https://www.javatpoint.com/java-math-random-method-example (Java)