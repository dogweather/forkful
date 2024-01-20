---
title:                "Capitalizzare una stringa"
html_title:           "Go: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizzare una stringa in Go

## Cos'è e perché?
Capitalizzare una stringa significa convertire la prima lettera di ogni parola in maiuscolo e lasciare inalterate le altre lettere. Questo è utile per formattare correttamente il testo nei programmi, come i nomi propri, titoli, ecc.

## Come fare:
Il pacchetto `strings` di Go fornisce la funzione `Title()` che può essere usata per capitalizzare una stringa. Ecco come funziona:

```Go
package main
import (
	"fmt"
	"strings"
)

func main() {
	var s string = "ciao mondo! è una bella giornata oggi."
	s = strings.Title(s)
	fmt.Println(s)
}
```
Il risultato sarà:

```Go
Ciao Mondo! È Una Bella Giornata Oggi.
```

## Approfondimento
La funzionalità di capitalizzazione delle stringhe era piuttosto rara nei linguaggi di programmazione storici, ma è diventata più comune con la crescente enfasi sulle interfacce utente amichevoli e l'internazionalizzazione.

Come alternativa alla funzione `Title()`, si può anche usare la funzione `ToTitle()` del pacchetto `unicode` di Go. Non cambierà solo la prima lettera di ogni parola in maiuscolo, ma anche tutte le altre lettere della parola.

Per quanto riguarda i dettagli di implementazione, la funzione `Title()` di Go utilizza l'Unicode per capire dove si trovano i confini delle parole.

Pertanto, la funzione è in grado di gestire correttamente la capitalizzazione in stringhe al di là dell'Inglese, a patto che siano formattate correttamente secondo le regole Unicode.

## Vedi anche
- [Documentazione ufficiale del pacchetto `strings` di Go](https://golang.org/pkg/strings/)
- [Documentazione ufficiale del pacchetto `unicode` di Go](https://golang.org/pkg/unicode/)
- [Stack Overflow: Come capitalizzare una stringa in Go](https://stackoverflow.com/questions/38554353/how-to-make-a-string-uppercase-in-go)