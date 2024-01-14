---
title:    "Go: Trova la lunghezza di una stringa"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Perché

Una delle funzionalità fondamentali della programmazione è la capacità di manipolare facilmente le stringhe. La lunghezza di una stringa è un concetto importante e spesso richiesto nelle applicazioni. In questa guida, scopriremo come trovare la lunghezza di una stringa utilizzando il linguaggio di programmazione Go.

## Come

Il calcolo della lunghezza di una stringa in Go è estremamente semplice grazie alla funzione integrata `len()`. Basta inserire la stringa all'interno delle parentesi della funzione e il risultato sarà il numero di caratteri presenti nella stringa.

```Go
package main

import (
    "fmt"
)

func main() {
    str := "Ciao mondo!"
    fmt.Println(len(str))
}

// Output: 11
```

In questo semplice esempio, abbiamo dichiarato una variabile `str` contenente la stringa "Ciao mondo!". La `len()` funzione restituisce il valore 11, cioè il numero di caratteri presenti nella stringa.

## Approfondimento

È importante notare che la funzione `len()` conta il numero di byte all'interno della stringa e non il numero di caratteri visibili. Questo significa che alcuni caratteri speciali possono essere rappresentati da più di un byte, causando una differenza tra il numero di caratteri visibili e il numero di byte contati dalla funzione `len()`.

Inoltre, la funzione `len()` può anche essere utilizzata per ottenere la lunghezza di altre strutture di dati, come ad esempio gli array.

```Go
strArr := [3]string{"Ciao", "mondo", "!"}
fmt.Println(len(strArr))

// Output: 3
```

In questo esempio, abbiamo una variabile `strArr` contenente un array di stringhe con tre elementi. Anche in questo caso, la funzione `len()` restituisce il numero di elementi presenti nell'array.

## Vedi anche

- [Documentazione ufficiale Go sulla funzione `len()`](https://golang.org/ref/spec#Length_and_capacity)
- [Esempi di codice per la funzione `len()` in Go](https://gobyexample.com/length)
- [Guida alla programmazione in Go per principianti](https://medium.com/tech-jobs-academy/beginner-guide-programming-in-go-golang-a8e063dd9117)