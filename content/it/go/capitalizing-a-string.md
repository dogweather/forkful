---
title:    "Go: Capitalizzazione di una stringa"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Perché

In questo articolo parleremo di come capitalizzare una stringa in Go. Potrebbe sembrare un'operazione semplice ma è importante saperlo fare correttamente per una corretta manipolazione dei dati all'interno del nostro codice.

## Come Fare

Per capitalizzare una stringa in Go, possiamo utilizzare la funzione `strings.ToUpper()` che ci permette di convertire una stringa in maiuscolo. Vediamo un esempio:

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  str := "ciao mondo"
  fmt.Println(strings.ToUpper(str)) // OUTPUT: CIAO MONDO
}
```

Se invece volessimo solo capitalizzare la prima lettera di una stringa, possiamo utilizzare la funzione `strings.Title()` che capitalizza la prima lettera di ogni parola all'interno della stringa. Ecco un altro esempio:

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  str := "ciao mondo"
  fmt.Println(strings.Title(str)) // OUTPUT: Ciao Mondo
}
```

In questo modo possiamo capitalizzare una stringa intera o solo la prima lettera, a seconda delle nostre esigenze.

## Approfondimento

Esistono anche altre funzioni all'interno del pacchetto `strings` che ci permettono di manipolare le stringhe, ad esempio `strings.ToLower()` per convertire una stringa in minuscolo o `strings.Trim()` per rimuovere caratteri iniziali o finali. È importante conoscere queste funzioni per gestire correttamente le stringhe all'interno del nostro codice.

## Vedi Anche

- [Documentazione ufficiale di Go sul pacchetto "strings"](https://golang.org/pkg/strings/)
- [Tutorial su come manipolare le stringhe in Go](https://tutorialedge.net/golang/working-with-strings-in-go/)
- [Un approfondimento sulle funzioni disponibili per le stringhe in Go](https://blog.learngoprogramming.com/golang-strings-cheat-sheet-30226c919f98)