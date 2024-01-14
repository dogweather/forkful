---
title:                "Gleam: Convertire una data in una stringa"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Convertire una data in una stringa è un'operazione molto comune nella programmazione e può essere utile per mostrare le date in un formato specifico o per elaborarle in modo più semplice.

## Come fare
Ecco come convertire una data nel formato desiderato utilizzando la lingua di programmazione Gleam:

```Gleam 
import Datetime

// Settare una data 
let my_date = Datetime now()

// Convertire in una stringa nel formato "yyyy-mm-dd"
let date_string = Datetime format my_date "%Y-%m-%d"
```

L'output sarà qualcosa come `2020-12-01`, a seconda della data attuale.

## Approfondimento
Per comprendere meglio come funziona la conversione di una data in una stringa in Gleam, è utile sapere che Gleam utilizza la libreria `Datetime` di Erlang per la gestione delle date e degli orari. Questa libreria offre una vasta gamma di funzioni utili per manipolare date e orari e può essere utile per eseguire operazioni più complesse come la conversione di una data in un formato personalizzato.

## Vedi anche
- [Documentazione ufficiale di Gleam](https://gleam.run/documentation/)
- [Libreria Datetime di Erlang](https://erlang.org/doc/man/datetime.html)
- [Guide Gleam su Medium](https://medium.com/gleam-lang)