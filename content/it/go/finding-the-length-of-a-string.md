---
title:    "Go: Trovare la lunghezza di una stringa"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Trovare la lunghezza di una stringa è spesso un'attività fondamentale nella programmazione. Conoscere la lunghezza di una stringa è utile per molteplici ragioni, come ad esempio la validazione dei dati inseriti dall'utente o la manipolazione di stringhe in operazioni più complesse.

## Come fare

In Go, per trovare la lunghezza di una stringa possiamo utilizzare la funzione predefinita `len()`. Questa funzione accetta come argomento una stringa e restituisce il numero di caratteri presenti all'interno della stringa. Vediamo un esempio pratico:

```Go
stringa := "Ciao mondo"
lunghezza := len(stringa)
fmt.Println(lunghezza)
```

In questo esempio, la variabile `lunghezza` conterrà il valore 10, poiché la stringa "Ciao mondo" è composta da 10 caratteri.

Possiamo anche utilizzare la funzione `len()` per trovare la lunghezza di una stringa contenuta all'interno di una variabile di tipo `slice`. In questo caso, la funzione restituirà il numero di elementi contenuti all'interno della `slice`:

```Go
slice := []string{"pomodoro", "patata", "carota"}
lunghezza := len(slice)
fmt.Println(lunghezza)
```

Nell'esempio sopra, la lunghezza della `slice` sarà di 3 elementi.

## Approfondimento

Oltre alla funzione `len()`, possiamo anche utilizzare il package `"strings"` di Go per avere maggiori opzioni per la manipolazione di stringhe. Ad esempio, il metodo `Count()` di questo package ci permette di trovare il numero di occorrenze di una determinata sottostringa all'interno di una stringa più grande.

## Vedere anche

- Documentazione ufficiale Go: https://golang.org/pkg/strings/
- Tutorial sulla manipolazione di stringhe in Go: https://www.callicoder.com/golang-strings-tutorial/
- Altro esempio pratico su come trovare la lunghezza di una stringa in Go: https://yourbasic.org/golang/find-string-length/