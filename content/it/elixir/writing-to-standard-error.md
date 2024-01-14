---
title:                "Elixir: Scrivere su standard error"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere sulla standard error è un aspetto importante della programmazione in Elixir, poiché fornisce un modo per visualizzare informazioni di errore su un terminale quando si eseguono i programmi. Ciò rende più facile il debug e la risoluzione di eventuali problemi durante lo sviluppo.

## Come fare

Per scrivere sulla standard error in Elixir, è possibile utilizzare la funzione `IO.puts/2` passando come argomenti la stringa di testo da visualizzare e `:stderr` come secondo argomento. Ad esempio:

```
Elixir IO.puts("Errore!", :stderr)
```

Questo scriverà "Errore!" sulla standard error durante l'esecuzione del programma.

## Approfondimento

La scrittura sulla standard error fa parte delle funzioni di input/output di Elixir, che rendono possibile la comunicazione tra un programma e l'ambiente esterno. Con la standard error, si ha a disposizione un canale dedicato alle informazioni di errore, che possono essere facilmente catturate e visualizzate durante l'esecuzione.

## Vedi anche

- [Funzioni di input/output in Elixir](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
- [Documentazione ufficiale su IO](https://hexdocs.pm/elixir/IO.html)