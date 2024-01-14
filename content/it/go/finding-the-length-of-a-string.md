---
title:                "Go: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è un compito comune nella programmazione, ma può essere utile in molti casi diversi. Potresti voler verificare la validità di un input utente, controllare la lunghezza di un nome o una password, oppure semplicemente per scopi di debug. In questo articolo, mostreremo come farlo in Go.

## Come fare

Per trovare la lunghezza di una stringa in Go, possiamo utilizzare la funzione `len()` insieme alla parola chiave `string` seguita dalla stringa stessa. Ecco un esempio di codice:

```Go
package main

import "fmt"

func main() {
	str := "Ciao mondo!"
	fmt.Println("La lunghezza della stringa è:", len(str))
}
```

L'output di questo codice sarà: `La lunghezza della stringa è: 11`. Come puoi vedere, la funzione `len()` restituisce un numero intero che corrisponde alla lunghezza della stringa in caratteri.

Possiamo anche applicare questa funzione a variabili di tipo `rune`, che rappresentano un singolo carattere Unicode. Ecco un esempio:

```Go
package main

import "fmt"

func main() {
	r := '😊'
	fmt.Println("La lunghezza del carattere Unicode è:", len(string(r)))
}
```

In questo caso, l'output sarà: `La lunghezza del carattere Unicode è: 2`, poiché rappresentiamo un carattere Unicode con due byte.

## Approfondimenti

Oltre alla funzione `len()`, esistono anche alcune altre opzioni per trovare la lunghezza di una stringa in Go. Ad esempio, possiamo utilizzare il pacchetto `unicode/utf8` per determinare la lunghezza di una stringa Unicode in byte o rune.

Inoltre, tenere a mente che Go gestisce automaticamente la gestione della memoria per le stringhe, quindi quando si utilizza la funzione `len()` non è necessario preoccuparsi di allocare memoria o di liberarla successivamente. Questo è uno dei vantaggi principali del linguaggio Go.

## Vedi anche

Per ulteriori informazioni sulla lunghezza delle stringhe in Go, puoi consultare la documentazione ufficiale del linguaggio su [golang.org](https://golang.org/pkg). Inoltre, puoi esplorare diversi esempi pratici e le ultime notizie sulla programmazione in Go su [GoLang Cafe](https://golang.cafe/it/).

Grazie per aver letto questo articolo. Speriamo che ti sia stato utile e ti abbiamo incoraggiato a esplorare il mondo della programmazione in Go. Buona codifica!