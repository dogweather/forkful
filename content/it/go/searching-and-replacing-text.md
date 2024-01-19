---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Cercare e sostituire testo in Go

## Che cosa & Perché?
Cercare e sostituire del testo è una funzionalità comune delle stringhe che ci permette di cambiarne il contenuto. Gli sviluppatori la usano per manipolare i dati, correggere errori, ecc.

## Come fare:
Vediamo come utilizzare la funzione `Replace` del pacchetto `strings` in Go per cercare e sostituire del testo.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "Ciao Mondo"
	ns := strings.Replace(s, "Mondo", "Gophers", -1)
	fmt.Println(ns)
}
```

Questo codice cerca "Mondo" nella stringa s e lo sostituisce con "Gophers". L'ultimo parametro, -1, indica di sostituirlo tutte le volte che viene trovato. L'output sarà:

```
Ciao Gophers
```

## Approfondimento
La funzione `Replace` in Go è abbastanza potente e versatile. Possiamo utilizzarla per cercare e sostituire qualsiasi sottostringa in una stringa. Questa semplice ma potente funzionalità è stata introdotta per la prima volta nel linguaggio di programmazione sed negli anni '70 e da allora è presente in quasi tutti i linguaggi di programmazione.

Esistono alternative per cercare e sostituire del testo, come l'utilizzo di espressioni regolari attraverso il pacchetto `regexp`. Questo approccio offre maggiore flessibilità ma è un po' più complesso.

Quando si chiama `strings.Replace`, Go crea una nuova stringa senza alterare la stringa originale. Questo perché in Go le stringhe sono immutabili, cosa che le rende più sicure ma potenzialmente meno efficienti se si fanno molte operazioni di sostituzione su una grande stringa.

## Vedere anche
Per saperne di più sulla manipolazione delle stringhe in Go, consulta questi collegamenti:

- Documentazione Go sul pacchetto `strings`: https://golang.org/pkg/strings/
- Un tutorial su come utilizzare le espressioni regolari in Go: https://gobyexample.com/regular-expressions
- Post del blog di Dave Cheney sulla manipolazione delle stringhe in Go: https://dave.cheney.net/2018/01/18/how-to-handle-io-readers-and-io-writers-in-go