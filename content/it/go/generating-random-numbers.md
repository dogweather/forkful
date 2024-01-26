---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:02.646163-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali significa dare al computer l'istruzione di creare numeri che sembrano non seguire un modello. I programmatori usano i numeri casuali per test, giochi, simulazioni e per garantire la sicurezza nei processi crittografici.

## How to:
In Go, possiamo usare il pacchetto `math/rand` per generare numeri casuali:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Numero casuale:", rand.Intn(100)) // genera un numero tra 0 e 99
}
```

Output:
```
Numero casuale: 42
```

## Deep Dive
La generazione di numeri casuali in informatica risale agli albori dei computer. Originariamente, i numeri erano generati tramite processi fisici, poi via hardware e, infine, per mezzo di algoritmi come quelli implementati nel pacchetto `math/rand` di Go.

Considera che i numeri generati da `math/rand` sono pseudocasuali: seguono una sequenza determinata da un valore iniziale, detto "seed". Per questo è essenziale variare il seed, tipicamente utilizzando il tempo corrente con `time.Now().UnixNano()`.

Alternative a `math/rand` includono l'uso di `crypto/rand`, che è adatto per la crittografia perché i suoi numeri casuali sono più difficili da predire.

Detto ciaramente, `math/rand` va bene per i giochi o per simulazioni, ma se stai lavorando su funzionalità dove la sicurezza è fondamentale, scegli `crypto/rand`.

## See Also
- Documentazione Go per `math/rand`: https://pkg.go.dev/math/rand
- Documentazione Go per `crypto/rand`: https://pkg.go.dev/crypto/rand
- Approfondimento sulla sicurezza dei numeri casuali: https://blog.golang.org/random
