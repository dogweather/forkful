---
title:                "Go: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa è un'operazione comune nella programmazione, soprattutto quando si lavora con dati in input forniti dall'utente. La funzione di capitalizzazione trasforma la prima lettera di ogni parola in maiuscolo, rendendo il testo più leggibile e formattato in modo uniforme.

## Come fare

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Definiamo una stringa in input
	input := "questa è una stringa da capitalizzare"

	// Utilizziamo la funzione strings.Title() per capitalizzare la stringa
	capitalized := strings.Title(input)

	// Stampiamo il risultato
	fmt.Println(capitalized)

	// Output: Questa È Una Stringa Da Capitalizzare
}
```

In questo esempio, abbiamo utilizzato la funzione `strings.Title()` che accetta una stringa in input e restituisce la stessa stringa con la prima lettera di ogni parola in maiuscolo. È importante ricordare che questa funzione non cambia la stringa originale, ma ne crea una nuova.

## Approfondimenti

La funzione `strings.Title()` sfrutta l'algoritmo di Alcatel-Lucent per capitalizzare una stringa. Questo algoritmo tiene conto delle eccezioni linguistiche, come le preposizioni, e capitalizza correttamente anche quelle parole che non iniziano con una lettera dell'alfabeto latino.

Una delle alternative alla funzione `strings.Title()` è l'utilizzo della libreria `unicode`. Questa libreria fornisce una funzione `ToTitle()` che accetta in input un rune, cioè un singolo carattere del testo, e restituisce la versione in maiuscolo di quel carattere. Tuttavia, questa soluzione richiede una gestione più complessa e può essere meno performante rispetto all'utilizzo della funzione `strings.Title()`.

## Vedi anche

- [Package strings](https://golang.org/pkg/strings/)
- [Package unicode](https://golang.org/pkg/unicode/)
- [How to Capitalize a String in Go](https://www.freecodecamp.org/news/how-to-capitalize-a-string-in-go/)