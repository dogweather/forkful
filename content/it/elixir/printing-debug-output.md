---
title:                "Stampa dell'output di debug"
html_title:           "Elixir: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

Elixir è un linguaggio di programmazione funzionale che è cresciuto rapidamente in popolarità negli ultimi anni. Grazie alla sua sintassi elegante e alla sua capacità di gestire concorrenza, Elixir è diventato uno dei principali strumenti utilizzati dalla comunità dei programmatori per sviluppare applicazioni web scalabili e affidabili.

## Cosa è e perché?

Stampare debug output è una pratica comune tra i programmatori di tutti i linguaggi di programmazione. Consiste nel visualizzare sullo schermo informazioni utili sulle variabili, i valori di ritorno delle funzioni e altri dati importanti per verificare il corretto funzionamento del codice. È un elemento chiave nella fase di debugging di un programma.

## Come fare:

Per stampare debug output in Elixir, è possibile utilizzare la funzione ```IO.puts/2```, che accetta due parametri: una stringa di testo da stampare e una variabile o espressione da visualizzare. Ad esempio, se vogliamo stampare il valore di una variabile chiamata ```x```, possiamo scrivere:

```Elixir
x = 10

IO.puts("Il valore della variabile x è #{x}")

```

L'output potrebbe essere:

```
Il valore della variabile x è 10
```

Inoltre, Elixir offre anche la funzione ```IO.inspect/2```, che stampa il valore di una variabile o espressione senza doverla convertire in una stringa. Questo può essere utile per il debugging di oggetti complessi o strutture dati.

```Elixir
mappa = %{nome: "Mario", eta: 35}

IO.inspect(mappa)

```

L'output potrebbe essere:

```
%{nome: "Mario", eta: 35}
```

## Approfondimenti:

La stampa di debug output ha origini nei primi anni della programmazione, quando i computer non avevano schermi grafici e la stampa di informazioni su carta era l'unico modo per verificare il funzionamento del codice. Oggi, ci sono anche altri strumenti utili per il debugging come i debugger e i log files.

## Vedi anche:

- Documentazione ufficiale di Elixir sulla funzione ```IO``` (https://hexdocs.pm/elixir/IO.html)
- Esempio di debug output in un progetto reale (https://github.com/elixir-lang/elixir/blob/master/lib/elixir/status.ex)