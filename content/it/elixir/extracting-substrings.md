---
title:                "Estrazione di sottostringhe"
html_title:           "Elixir: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Estrarre sottostringhe è una tecnica di programmazione utilizzata per ottenere una porzione di una stringa più grande. È molto comune tra i programmatori in quanto consente di manipolare e analizzare le stringhe in modo più efficiente e preciso.

## Come fare:
In Elixir, esistono diverse funzioni utili per estrarre sottostringhe. Ecco un esempio di codice:

```Elixir
stringa = "Ciao mondo"
sottostringa = String.slice(stringa, 0, 4)
IO.puts sottostringa
```

Questo codice utilizzerà la funzione `String.slice` per estrarre una sottostringa dalla posizione 0 fino alla posizione 4 (esclusa) della stringa originale. L'output sarà "Ciao".

## Approfondimento:
L'operazione di estrazione di sottostringhe viene utilizzata principalmente per manipolare le stringhe all'interno di un programma. Ciò include la ricerca di una particolare porzione di testo, la sostituzione di una sottostringa con un'altra e la creazione di nuove stringhe combinate da sottostringhe.

Un'alternativa alla funzione `String.slice` è l'utilizzo di espressioni regolari, che permettono una maggiore flessibilità nella ricerca di sottostringhe che coincidono con un determinato pattern. Inoltre, è possibile utilizzare la funzione `String.split` per dividere una stringa in sottostringhe in base a un delimitatore specifico.

L'implementazione di queste funzioni è basata sull'utilizzo dei caratteri Unicode per rappresentare le stringhe, il che permette di gestire anche caratteri non latini.

## Vedi anche:
Per ulteriori informazioni su come utilizzare le funzioni di estrazione di sottostringhe in Elixir, puoi consultare la documentazione ufficiale su [Elixir School](https://elixirschool.com/lessons/basics/strings/#string-slicing), dove troverai esempi e spiegazioni dettagliate. Inoltre, puoi esplorare altre funzionalità del linguaggio utilizzando [Exercism](https://exercism.io/tracks/elixir/exercises), una piattaforma di esercizi guidati per i principali linguaggi di programmazione.