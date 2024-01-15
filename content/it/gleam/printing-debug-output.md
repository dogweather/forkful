---
title:                "Stampa dell'output di debug"
html_title:           "Gleam: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore di Gleam, potresti trovarti spesso a dover gestire errori o bug nel tuo codice. In questi casi, la stampa in output dei tuoi processi può essere un utile strumento per il debugging e la ricerca di eventuali errori.

## Come fare

Per stampare l'output dei tuoi processi in Gleam, puoi utilizzare la funzione `IO.debug/1` seguita dal valore o dalla variabile che desideri visualizzare. Ad esempio:

```Gleam
let nome = "Mario"
IO.debug(nome)

// Output: "Mario"
```

Puoi anche stampare più valori in una singola riga utilizzando la funzione `IO.inspect/1` e una lista di valori separati da virgola. Ad esempio:

```Gleam
let numero1 = 5
let numero2 = 10
IO.inspect(numero1, numero2)

// Output: 5, 10
```

## Approfondimento

La stampa in output dei tuoi processi può essere una pratica utile per comprendere meglio il funzionamento del tuo codice e individuare eventuali errori. Tuttavia, è importante ricordare di rimuovere o commentare queste istruzioni di debug una volta che hai risolto il problema, altrimenti possono rallentare le prestazioni del tuo programma.

## Vedi anche

- [Documentazione ufficiale di Gleam](https://gleam.run/)
- [Esercitazioni su Gleam](https://github.com/gleam-lang/website/tree/master/tutorials)
- [Altri articoli su Gleam](https://dev.to/t/gleam)