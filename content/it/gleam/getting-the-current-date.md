---
title:                "Ottenere la data corrente"
html_title:           "Gleam: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La possibilità di ottenere la data attuale è un'informazione utile in molte applicazioni, come ad esempio nei programmi di gestione della fatturazione, nei sistemi di prenotazione online o nei calendari. Inoltre, utilizzare la data attuale può semplificare il processo di calcolo di scadenze o di generazione di report.

## Come fare

Per ottenere la data attuale in Gleam, è possibile utilizzare la funzione `Calendar.now()`. Si consiglia di assegnare il valore della funzione a una variabile per poterlo utilizzare successivamente nel codice. Un esempio di codice potrebbe essere il seguente:

```
// Importa il modulo Calendar per accedere alle funzioni per la gestione delle date
import gleam/calendar

// Assegna alla variabile `data_attuale` la data attuale
data_attuale = calendar.now()

// Stampa la data attuale nel formato `dd/mm/yyyy`
// In questo esempio, la data verrebbe stampata come `16/09/2021` 
io.println(calendar.format(data_attuale, "%d/%m/%Y"))
```

L'ouput di questo esempio sarebbe `16/09/2021`, ma è possibile utilizzare diversi formati di visualizzazione della data utilizzando la funzione `calendar.format`.

## Approfondimento

La funzione `Calendar.now()` utilizza il fuso orario locale del sistema in cui viene eseguito il codice. Se si vuole ottenere la data in un fuso orario diverso, è possibile specificarlo come argomento della funzione. Ad esempio, `Calendar.now("America/New_York")` restituirà la data attuale nel fuso orario di New York.

## Vedi anche

- API della libreria `gleam/calendar`: https://gleam.run/modules/gleam/calendar/latest
- Documentazione ufficiale di Gleam: https://gleam.run/documentation