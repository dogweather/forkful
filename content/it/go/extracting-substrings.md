---
title:                "Go: Estrazione di sottostringhe"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché 
Spesso nella programmazione, ci troviamo di fronte alla necessità di manipolare o analizzare una stringa di testo. In questi casi, estrarre una sottostringa può essere un'operazione utile per ottenere solo le informazioni di nostro interesse. In questo articolo, esploreremo come fare ciò utilizzando il linguaggio di programmazione Go.

## Come fare

Per estrarre una sottostringa in Go, possiamo utilizzare la funzione `substring()` della libreria `strings`. Questa funzione richiede come argomenti la stringa da cui vogliamo estrarre la sottostringa, e gli indici di inizio e fine della sottostringa desiderata. Ad esempio, se volessimo estrarre la sottostringa "ciao" dalla stringa "ciao mondo", il codice sarebbe il seguente:

```Go
package main

import "fmt"
import "strings"

func main() {
  testo := "ciao mondo"
  sottostringa := strings.substring(testo, 0, 3)

  fmt.Println(sottostringa)
}
```

L'output di questo codice sarebbe:

```
ciao
```

Possiamo anche utilizzare la funzione `len()` per ottenere la lunghezza della stringa e ottenere facilmente la sottostringa desiderata a partire da indici relativi alla fine della stringa. Ad esempio, per ottenere gli ultimi 3 caratteri di una stringa, possiamo fare:

```Go
package main

import "fmt"
import "strings"

func main() {
  testo := "ciao mondo"
  lunghezza := len(testo)
  sottostringa := strings.substring(testo, lunghezza-3, lunghezza)

  fmt.Println(sottostringa)
}
```

L'output sarebbe:

```
ndo
```

## Deep Dive

È importante notare che in Go gli indici di una stringa iniziano da 0, come nella maggior parte dei linguaggi di programmazione. Inoltre, è possibile utilizzare numeri negativi come indici nella funzione `substring()`, che indicano l'indice a partire dalla fine della stringa (ad esempio, -1 corrisponde all'ultimo carattere della stringa).

Inoltre, la libreria `strings` offre molte altre funzioni utili per lavorare con le stringhe, come ad esempio `contains()` per verificare se una sottostringa è presente in una stringa, e `replace()` per sostituire una sottostringa con un'altra.

## Vedi anche

Ecco alcuni link utili per approfondire l'argomento:

- Documentazione ufficiale sulle funzioni di `strings` in Go: https://golang.org/pkg/strings/
- Esempi di codice su come utilizzare le funzioni per estrarre substrings in Go: https://gobyexample.com/substring
- Tutorial su come manipolare stringhe in Go: https://www.digitalocean.com/community/tutorials/how-to-manipulate-strings-in-go