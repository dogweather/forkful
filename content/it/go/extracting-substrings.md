---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cos'é e Perché?

Estrarre sottostringhe è l'operazione di prendere una porzione di una stringa esistente. I programmatori lo fanno per manipolare e analizzare dati testuali più facilmente.

## Come fare:

Ecco un esempio di come estrarre una sottostringa in Go:

```Go
package main

import (
	"fmt"
)

func main() {
	str := "Benvenuto al tutorial su Go"
	subs := str[10:22]

	fmt.Println(subs) // stampa: "tutorial su"
}
```
In questo esempio, prendiamo la stringa da indice 10 al 22 (gli indici iniziano da 0).

## Approfondimenti:

1. **Storicamente**, l'abilità di estrarre sottostringhe è stata essenziale in mancanza di metodi più moderni per la manipolazione delle stringhe. 
2. **Alternative**: metodo `strings.Split()`. Questa funzione divide una stringa in base a un separatore che ti fornisce come stringa.
3. **Dettagli di implementazione**: Go implementa le stringhe come slice di byte, quindi l'estrazione di una sottostringa è semplicemente una questione di slicing l'array sottostante.

```Go
str := "Benvenuto al tutorial su Go"
parts := strings.Split(str, " ")
fmt.Println(parts[2:]) // stampa: ["tutorial", "su", "Go"]
```
In questo esempio, usiamo `strings.Split()` per dividere la frase in parole, e poi stampiamo le parole da indice 2 in poi.

## Vedi Anche:

1. La documentazione ufficiale di Go sulle stringhe: [link](https://golang.org/pkg/strings/)
2. Un tutorial più dettagliato sulla manipolazione delle stringhe in Go: [link](https://gobyexample.com/string-functions)
3. Un post su stackoverflow con molte discussioni sulla manipolazione delle stringhe in Go: [link](https://stackoverflow.com/questions/47302331/split-a-string-into-substrings-in-golang)