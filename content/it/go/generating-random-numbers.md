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

## Perchè

Generare numeri casuali è un'operazione comune nella programmazione e può essere utile in diversi contesti, come nel creare giochi, testare algoritmi o simulare eventi casuali.

## Come Fare

Per generare numeri casuali in Go, possiamo utilizzare il pacchetto "math/rand", che fornisce funzioni per la generazione di numeri interi e in virgola mobile. Vediamo alcuni esempi:

```Go
// Genera un numero intero casuale tra 0 e 10
rand.Intn(11)

// Genera un numero intero casuale tra 5 e 15
rand.Intn(11) + 5

// Genera un numero in virgola mobile casuale tra 0 e 1
rand.Float64()

// Genera un numero in virgola mobile casuale tra 5 e 10
rand.Float64() * 5 + 5
```

Il pacchetto "math/rand" utilizza il generatore di numeri pseudo-casuali "Linear Congruential Generator" (LCG) per generare numeri casuali. Per ottenere sequenze diverse di numeri casuali, dobbiamo impostare il seme del generatore utilizzando la funzione "Seed" prima di ogni generazione. Ad esempio:

```Go
// Imposta il seme a 42
rand.Seed(42)

// Genera un numero casuale
rand.Intn(100)
```

Possiamo anche creare un generatore con un seme diverso e utilizzarlo per ottenere una sequenza di numeri casuali specifica, come ad esempio quando testiamo un algoritmo che richiede una sequenza prevedibile di numeri casuali:

```Go
// Crea un generatore con il seme 1234
myRand := rand.New(rand.NewSource(1234))

// Genera 5 numeri casuali utilizzando il nostro generatore
myRand.Intn(10)
myRand.Intn(10)
myRand.Intn(10)
myRand.Intn(10)
myRand.Intn(10)
```

## Deep Dive

Come accennato prima, il pacchetto "math/rand" utilizza l'algoritmo LCG per generare numeri casuali. Questo algoritmo utilizza una semplice equazione matematica per produrre una sequenza di numeri che sembrano casuali, ma in realtà sono prevedibili se si conosce il seme. Ciò significa che il pacchetto "math/rand" non è indicato per generare numeri realmente casuali per scopi critici come la generazione di password o la crittografia.

Inoltre, l'algoritmo LCG presenta alcune limitazioni, come ad esempio una periodicità nella sequenza di numeri generati. Ciò significa che dopo un certo numero di generazioni, la sequenza si ripeterà. Inoltre, il pacchetto "math/rand" utilizza un seme prevedibile, il tempo Unix in millisecondi, il che rende ancora più facile la previsione dei numeri casuali generati.

Una possibile soluzione a queste limitazioni è utilizzare il pacchetto "crypto/rand", che utilizza un generatore di numeri realmente casuali basato sull'entropia del sistema operativo. Tuttavia, questo pacchetto è più lento e richiede più risorse del pacchetto "math/rand", quindi è consigliato solo per scopi critici.

## See Also

Maggiori informazioni sui pacchetti "math/rand" e "crypto/rand":

- [Pacchetto "math/rand" su GoDoc](https://golang.org/pkg/math/rand/)
- [Pacchetto "crypto/rand" su GoDoc](https://golang.org/pkg/crypto/rand/)

Altri approfondimenti sulla generazione di numeri casuali in Go:

- [Go: What Does rand.Read() Really Do?](https://medium.com/swlh/go-what-does-rand-read-really-do-f14f7acb6a46)
- [Generating Random Numbers with Go](https://www.digitalocean.com/community/tutorials/how-to-use-the-math-rand-package-in-go)
- [Security and Cryptography in Golang](https://medium.com/rungo/security-and-cryptography-in-golang-76a06264745c)