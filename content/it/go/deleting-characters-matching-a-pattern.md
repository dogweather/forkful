---
title:                "Go: Cancellazione di caratteri corrispondenti ad un modello"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molti motivi per cui un programmatore potrebbe voler cancellare i caratteri che corrispondono a un determinato modello. Forse si stanno pulendo i dati grezzi di un file, o si sta cercando di sostituire tutti i numeri con il loro valore opposto. Indipendentemente dal motivo, sapere come eliminare rapidamente e facilmente i caratteri che si desidera è un'abilità utile da avere nella cintola degli strumenti di un programmatore.

## Come Fare

Per eliminare i caratteri che corrispondono a un modello in Go, è necessario utilizzare la funzione `strings.ReplaceAll ()`. Vediamo un esempio di come utilizzare questa funzione per eliminare tutte le vocali da una stringa:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	input := "Questo è un esempio di frase con molte vocali."
	output := strings.ReplaceAll(input, "a", "")
	output = strings.ReplaceAll(output, "e", "")
	output = strings.ReplaceAll(output, "i", "")
	output = strings.ReplaceAll(output, "o", "")
	output = strings.ReplaceAll(output, "u", "")

	fmt.Println(output)
}
```

Nell'esempio sopra, abbiamo definito una stringa di input contenente molte vocali e l'abbiamo assegnata alla variabile `input`. Poi, abbiamo utilizzato la funzione `strings.ReplaceAll ()` per sostituire le vocali con una stringa vuota, effettivamente eliminandole dalla stringa. Abbiamo assegnato il risultato alla variabile `output` e poi stampato il suo valore, che sarà "Qst è n mpxmp d frs c mlt vlcl."

## Approfondimento

Oltre alla funzione `strings.ReplaceAll ()`, esistono altre funzioni utili per eliminare i caratteri che corrispondono a un modello in Go. Ad esempio, la funzione `strings.Trim ()` può essere utilizzata per rimuovere uno o più caratteri specificati da entrambi i lati di una stringa, mentre la funzione `strings.TrimPrefix ()` può essere utilizzata per eliminare un prefisso specificato dalla stringa. Esplorare queste e altre funzioni simili può essere utile per ottenere l'effetto desiderato.

## Vedi Anche

- Documentazione ufficiale di Go sulla funzione `strings.ReplaceAll ()`: https://golang.org/pkg/strings/#ReplaceAll
- Tutorial dettagliato su come eliminare caratteri in Go: https://gobyexample.com/string-functions
- Libreria "strings" di Go che include molte funzioni utili per la manipolazione delle stringhe: https://golang.org/pkg/strings/