---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Stampare output di debug è l'atto di visualizzare informazioni dettagliate sullo stato del nostro programma mentre è in esecuzione. E' uno strumento cruciale per i programmatori, in quanto ci aiuta a capire cosa sta succedendo nel nostro codice e a risolvere eventuali problemi.

## Come fare:

Ecco un semplice esempio di come eseguire la stampa di debug in Gleam:

```Gleam
import gleam/io

pub fn main(args: List(String)) {
  let _ = io.println("Hello, Gleam!")
}
```

Quando esegui questo codice, vedrai un output simile a questo:

```Gleam
Hello, Gleam!
```

## Approfondimento

La stampa di debug ha le sue radici storiche nelle prime giornate della programmazione, quando i programmatori dovevano fisicamente esaminare i codici binari per trovare problemi. Con lo sviluppo del software, la stampa di debug è diventata un punto fermo negli strumenti di debugging.

Alternativamente, invece di utilizzare la stampa standard, è possibile utilizzare il modulo `gleam/log` per eseguire la stampa di debug con informazioni aggiuntive come l'ora e la data.

Gleam implementa la stampa di debug attraverso il modulo `gleam/io`. Questo modulo fornisce una serie di funzioni di input/output che possono essere usate per scrivere sullo standard output o leggere dallo standard input.

## Vedi anche

1. [Guida ufficiale alla programmazione in Gleam](https://gleam.run/book/)
3. [Articolo sull'utilizzo della stampa di debug nel linguaggio Gleam](https://joseconsdorf.medium.com/debug-print-statements-in-gleam-3a153e175f87)