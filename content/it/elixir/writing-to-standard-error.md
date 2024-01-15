---
title:                "Scrivere su standard error"
html_title:           "Elixir: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error può essere utile per la risoluzione dei problemi durante il debugging del codice. Inoltre, può essere utilizzato per notificare l'utente di eventuali errori o informazioni importanti durante l'esecuzione del programma.

## Come fare

Per scrivere su standard error in Elixir, possiamo utilizzare la funzione `IO.puts/2` e specificare come primo argomento il termine `:stderr`. Ad esempio, se vogliamo scrivere "Errore di sistema!" su standard error, possiamo usare il seguente codice:

```
IO.puts(:stderr, "Errore di sistema!")
```

Questo scriverà "Errore di sistema!" nel terminale ma su standard error, invece che su standard output come sarebbe stato se avessimo usato `IO.puts/1`.

## Approfondimento

In Elixir, è possibile usare la macro `__ENV__` per ottenere informazioni relative al contesto in cui si sta chiamando la funzione. Possiamo utilizzarla per stampare il nome del file e il numero della riga in cui è stata chiamata la funzione `IO.puts/2`:

```
IO.puts(:stderr, "Errore in #{__ENV__.file}:#{__ENV__.line}")
```

Ciò può risultare utile per identificare con precisione i punti del codice dove si verificano gli errori. Inoltre, è importante notare che `IO.puts/2` restituisce sempre `:ok`, indipendentemente dal fatto che venga scritto qualcosa su standard error o meno. Per verificare se si è verificato un errore durante la scrittura su standard error, possiamo utilizzare la funzione `IO.puts/1`.

## Vedi anche

- Documentazione ufficiale di Elixir: https://elixir-lang.org/docs.html
- Altri metodi per la gestione degli errori in Elixir: https://pragmaticstudio.com/tutorials/error-handling-in-elixir