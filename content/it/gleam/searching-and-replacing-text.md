---
title:                "Gleam: Ricerca e sostituzione di testo"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
La ricerca e la sostituzione di testo sono due delle attività più comuni e utili per i programmatori. Questo processo consente di modificare rapidamente molte parti di codice in una sola volta, risparmiando tempo e sforzi.

## Come Fare
Per effettuare una ricerca e sostituire del testo in Gleam, è necessario utilizzare la funzione `replace` e specificare il testo da cercare e il testo di sostituzione. Ecco un esempio:

```
Gleam - replace("ciao", "hello")
```

Ciò sostituirà tutte le istanze della parola "ciao" con "hello" all'interno della stringa di testo. È anche possibile specificare una variabile per contenere il testo da cercare e la sostituzione può anche essere una variabile.

```
let da_cambiare = "ciao"
let sostituire_con = "hello"
let risultato = [Gleam - replace(da_cambiare, sostituire_con)]
```

## Approfondimento
La funzione `replace` di Gleam utilizza regole di rimpiazzo di alta qualità, consentendo di specificare anche dei pattern di rimpiazzo più avanzati. È possibile utilizzare espressioni regolari o codice per determinare quale testo deve essere sostituito. Ciò rende Gleam un linguaggio particolarmente potente per la ricerca e la sostituzione di testo, con un'ampia gamma di opzioni disponibili per soddisfare le esigenze di ogni progetto.

## Guarda anche
- Documentazione ufficiale di Gleam su `replace`: https://gleam.run/docs/strings#replace 
- Tutorial su espressioni regolari in Gleam: https://gleam.run/articles/regular-expressions