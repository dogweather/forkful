---
title:                "Go: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

In programmazione, potrebbe essere utile stampare output di debug per aiutare a identificare errori e problemi nel codice. È un modo semplice per capire cosa sta accadendo durante l'esecuzione del programma e come migliorare la sua efficienza. In questo post, esploreremo come utilizzare la stampa di debug in linguaggio Go per migliorare il tuo processo di sviluppo.

## Come fare

Per stampare output di debug in Go, useremo la funzione `fmt.Printf()` inclusa nel pacchetto standard `fmt`. La sintassi di base è `fmt.Printf(format, a, b, ...)` dove il formato specifica come stampare i valori `a`, `b`, ecc. Segue un esempio:

```
package main

import "fmt"

func main() {
	num1 := 5
	num2 := 10
	fmt.Printf("num1 = %d, num2 = %d\n", num1, num2)
}
```

L'output di questo codice sarà:

`num1 = 5, num2 = 10`

Puoi anche utilizzare `%v` per stampare i valori di qualsiasi tipo di dato, `%T` per stampare il tipo di dato e `%v` per stampare il valore del puntatore.

```
func main() {
	str := "Ciao"
	fmt.Printf("str = %s\n", str)
	fmt.Printf("str è di tipo %T\n", str)
	fmt.Printf("&str = %v\n", &str)
}
```

Questa volta l'output sarà:

`str = Ciao`

`str è di tipo string`

`&str = 0xc232004478`

## Approfondimento

Puoi anche utilizzare i formati speciali per rendere l'output di debug più facile da leggere. Ad esempio, `%#v` stampa la rappresentazione di debug dei valori, `%+v` stampa il nome del campo in una struttura, `%#v` stampa il percorso completo della struttura e così via. Usa questi formati in base alle tue esigenze per ottenere informazioni più dettagliate sull'output di debug.

Un altro modo per stampare l'output di debug è utilizzare il pacchetto `log` invece di `fmt`. Questo pacchetto include funzioni come `log.Print()` e `log.Printf()` che stampano output di debug nel formato predefinito del pacchetto. Puoi anche impostare livelli di log come `log.Fatal()` per stampare un messaggio di errore e terminare il programma immediatamente.

## Vedi anche

- [Documentazione ufficiale di fmt](https://golang.org/pkg/fmt/)
- [Documentazione ufficiale di log](https://golang.org/pkg/log/)
- [Tutorial su debug in Go](https://tutorialedge.net/golang/intro-debugging-with-go/)