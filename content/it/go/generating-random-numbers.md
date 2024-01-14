---
title:                "Go: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un aspetto importante della programmazione in Go. Può essere utile in diversi contesti, come ad esempio nei giochi, nella crittografia e nell'analisi statistica.

## Come Fare

Per generare numeri casuali in Go, è necessario importare il pacchetto `math/rand`. Poi, si può usare la funzione `rand.Intn()` per generare un intero randomico all'interno di un range specificato. Ad esempio:

```Go
import "math/rand"

func main() {
    // Genera un numero casuale tra 0 e 99
    randNum := rand.Intn(100)
    fmt.Println(randNum)
}
```

Output:

``` 
57
```

Si può anche usare `rand.Float64()` per generare un numero a virgola mobile tra 0.0 e 1.0. Inoltre, si può impostare il seme di generazione dei numeri casuali usando la funzione `rand.Seed()`.

## Approfondimento

In Go, la generazione di numeri casuali è gestita dal generatore di numeri pseudo-casuali (PRNG) fornito dal pacchetto `math/rand`. Questo PRNG utilizza un algoritmo di tipo Linear Congruential Generator per produrre una sequenza di numeri che sembrano casuali, ma che in realtà sono deterministici, dato un seme iniziale.

È importante notare che il generatore di numeri casuali in Go non è considerato sicuro per scopi crittografici, poiché è possibile prevedere i numeri generati se si conosce il seme iniziale. In questi casi, si dovrebbe utilizzare il pacchetto `crypto/rand` che utilizza un generatore di numeri crittograficamente sicuro.

## Vedi Anche

- Documentazione ufficiale di Go sul pacchetto `math/rand`: https://golang.org/pkg/math/rand/
- Tutorial su come generare numeri casuali in Go: https://gobyexample.com/random-numbers 
- Spiegazione dettagliata sul funzionamento del generatore di numeri casuali in Go: https://stackoverflow.com/questions/30667030/how-is-the-go-stdlib-math-rand-prng-seeded