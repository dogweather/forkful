---
title:                "Go: Generazione di numeri casuali"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è una funzione utile in molte applicazioni, come nei giochi e nei sistemi di crittografia. In Go, ci sono molte opzioni per generare numeri casuali che consentono di personalizzare e controllare il processo.

## Come Fare

Per generare numeri casuali in Go, utilizzeremo il package "math/rand" e la funzione "Intn" per generare un intero casuale tra 0 e un numero massimo specificato. Il numero massimo è esclusivo, quindi se vogliamo generare un numero tra 1 e 10, dobbiamo specificare 11 come limite superiore.

```
package main

import (
    "fmt"
    "math/rand"
)

func main() {
    // Generare un numero casuale tra 1 e 10
    numeroCasuale := rand.Intn(11)

    fmt.Println(numeroCasuale)
}
```

Questo codice produrrà un numero casuale ogni volta che viene eseguito. Se vogliamo ottenere un numero casuale diverso ogni volta, dobbiamo impostare il seme con la funzione "Seed". Possiamo utilizzare una variabile di tipo int64 per la semente e passarla alla funzione "Seed". Inoltre, per evitare di ottenere sempre lo stesso numero casuale quando eseguiamo il codice, possiamo utilizzare la funzione "Time" del package "time" per ottenere il tempo corrente come seme.

```
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // Impostare il seme con il tempo corrente
    rand.Seed(time.Now().UnixNano())

    // Generare un numero casuale tra 1 e 10
    numeroCasuale := rand.Intn(11)

    fmt.Println(numeroCasuale)
}
```

Possiamo anche utilizzare la funzione "Float64" per generare numeri decimali casuali. In questo caso, dobbiamo specificare un limite inferiore e superiore per il numero.

```
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // Impostare il seme con il tempo corrente
    rand.Seed(time.Now().UnixNano())

    // Generare un numero casuale tra 1 e 10
    numeroCasuale := rand.Float64() * 10 + 1 // genererà un numero tra 1 e 10

    fmt.Println(numeroCasuale)
}
```

## Approfondimento

Ci sono molti altri modi per generare numeri casuali in Go, come l'utilizzo del package "crypto/rand" per generare numeri crittograficamente sicuri. Inoltre, possiamo anche utilizzare le funzioni "Read" e "ReadInt" per generare numeri casuali da un generatore di numeri casuale personalizzato.

## Vedi Anche

- [Package "math/rand" nella documentazione di Go](https://golang.org/pkg/math/rand/)
- [Little Go Snippets: Generare numeri casuali](https://yourbasic.org/golang/random-number-generator/)
- [Risorse sul generatore di numeri casuali in Go](https://hackernoon.com/resources-for-the-go-random-number-generator-5428bf01faf7)