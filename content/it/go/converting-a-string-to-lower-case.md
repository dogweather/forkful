---
title:                "Go: Convertire una stringa in minuscolo."
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Spesso ci troviamo ad avere una stringa che contiene lettere maiuscole e minuscole e per motivi di uniformità o di confronto, potrebbe essere necessario convertirla interamente in minuscolo.

## Come fare
La conversione di una stringa in minuscolo è molto semplice in Go grazie al pacchetto "strings". Ecco un esempio di codice che illustra come farlo:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Ciao, Mondo!"
	lowerCaseStr := strings.ToLower(str)
	fmt.Println(lowerCaseStr)
}
```

L'output di questo esempio sarà: "ciao, mondo!" come desiderato. Il pacchetto "strings" mette a disposizione il metodo "ToLower" che accetta come parametro una stringa e restituisce una nuova stringa in minuscolo.

## Approfondimento
La conversione di una stringa in minuscolo può sembrare un'operazione banale, ma in realtà nasconde alcuni dettagli interessanti. Ad esempio, il pacchetto "strings" effettua la conversione considerando gli standard di unicode, il che può comportare delle differenze nei risultati rispetto ad altri linguaggi di programmazione. Inoltre, è importante tenere presente che la conversione non viene effettuata in-place, ma viene restituita una nuova stringa. Questo significa che nel caso di stringhe molto lunghe, l'operazione può comportare un certo costo in termini di memoria.

## Vedi anche
- Documentazione ufficiale sul pacchetto strings: https://golang.org/pkg/strings
- Articolo su unicode e la conversione di stringhe in Go: https://blog.golang.org/strings
- Domande frequenti sul pacchetto strings: https://golang.org/pkg/strings/#FAQ