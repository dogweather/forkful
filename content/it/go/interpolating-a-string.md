---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?
L'interpolazione delle stringhe è una tecnica per inserire variabili all'interno di una stringa. I programmatori lo fanno per creare stringhe dinamiche senza dover concatenare manualmente.

## Come Fare:
Nel Go, utilizziamo la funzione `fmt.Sprintf` per interpolare le stringhe. Ecco un esempio:

```Go
package main

import "fmt"

func main() {
    nome := "Mario"
    eta := 30
    msg := fmt.Sprintf("Ciao, sono %s e ho %d anni.", nome, eta)

    fmt.Println(msg)
}
```
Questo genera l'output:

```
Ciao, sono Mario e ho 30 anni.
```

## Approfondimenti
Storicamente, l'interpolazione delle stringhe è molto usata nei linguaggi di scripting come Perl e Ruby. In Go, non abbiamo un vero e proprio operatore di interpolazione delle stringhe, quindi usiamo `fmt.Sprintf`.

Ci sono alternative all'interpolazione delle stringhe. Ad esempio, potresti usare la concatenazione di stringhe o la funzione `fmt.Print`. Tuttavia, `fmt.Sprintf` è generalmente preferita perché è più leggibile e versatile.

In termini di implementazione, `fmt.Sprintf` funziona utilizzando gli specifiers di formato (%s, %d, ecc.) per determinare come formattare le variabili. Le variabili passate dopo la stringa di formato vengono inserite al posto degli specifiers corrispondenti.

## Vedere Anche
Per ulteriori informazioni sull'interpolazione delle stringhe in Go, consulta questi link:

- [Documentazione ufficiale di fmt](https://pkg.go.dev/fmt)
- [Sprintf on Go by Example](https://gobyexample.com/string-formatting)