---
title:                "Capitalizzare una stringa"
html_title:           "Gleam: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitale di una Stringa in Gleam

## Che Cosa e Perché?
Mettere in maiuscolo una stringa significa convertire tutte le sue lettere minuscole in maiuscole. I programmatori lo fanno per motivi estetici, di formattazione o per confrontare stringhe in modo insensibile al caso.

## Come Fare:
In Gleam, possiamo utilizzare la funzione `string.to_upper` per capitalizzare una stringa. Ecco un esempio:

```
import gleam/string

pub fn main() {
    let message = "ciao mondo"
    let uppercase_message = string.to_upper(message)
    io.println(uppercase_message) // Stampa: "CIAO MONDO"
}
```

## Approfondimento
Historicamente, il concetto di "maiuscolo" e "minuscolo" deriva dalla tipografia. In termini di programmi, non tutte le lingue hanno il concetto di maiuscolo e minuscolo. Dunque, il comportamento di `string.to_upper` può variare a seconda dell'insieme di caratteri utilizzato.

Un'alternativa al metodo `string.to_upper` sarebbe di implementare la tua funzione per percorrere ciascuno carattere nella stringa e convertirlo in maiuscolo.

Ecco un dettaglio importante: la funzione `string.to_upper` di Gleam non modifica la stringa originale. Invece, restituisce una nuova stringa con tutte le lettere convertite in maiuscole. Questo perché le stringhe in Gleam sono immutabili.

## Vedi Anche
Per ulteriori informazioni sulle stringhe in Gleam, consulta la [documentazione ufficiale](https://gleam.run/book/tour/strings.html). Per una trattazione approfondita sulla capitalizzazione delle stringhe, [questo articolo](https://softwareengineering.stackexchange.com/questions/102122/is-avoiding-capitalisation-in-programming-considered-bad-practice) è un buon punto di partenza.