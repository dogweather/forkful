---
title:                "Elixir: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Esistono diversi motivi per cui potresti voler scrivere su standard error quando si tratta di programmazione in Elixir. Potresti voler segnalare errori o eccezioni durante l'esecuzione del codice, o semplicemente voler registrare informazioni di debug per aiutarti a risolvere eventuali problemi.

## Come fare

In Elixir, per scrivere su standard error, possiamo utilizzare la funzione `IO.puts/2` impostando il secondo parametro su `:stderr`. Ad esempio:

```Elixir
IO.puts("Questo è un messaggio di errore", :stderr)
```

Questo scriverà il messaggio "Questo è un messaggio di errore" su standard error.

## Approfondimento

Scrivere su standard error in Elixir ha in realtà molte più opzioni rispetto a semplici messaggi di errore. Possiamo anche utilizzare la funzione `IO.error/1` per formattare il messaggio di errore in modo più dettagliato, includendo la data e l'ora in cui si è verificato, la posizione nel codice e il nome del modulo. Ad esempio:

```Elixir
IO.error({:enoent, "file.txt"})
```

Questo produrrà un messaggio di errore simile a questo:

```
** (File.Error) could not read file file.txt. The following reason was given: :enoent
** File.noent: file file.txt does not exist
    (elixir) src/elixir_io.erl:126: :elixir_io.in/4
```

Inoltre, possiamo anche utilizzare il modulo `Logger` per scrivere su standard error, utilizzando il livello di log `:error` per segnalare errori e problemi gravi nel nostro codice.

## Vedi anche

- [Documentazione ufficiale di Elixir su standard error](https://hexdocs.pm/elixir/IO.html#writing-to-standard-error)
- [Guida su come utilizzare il modulo Logger in Elixir](https://elixirschool.com/it/lessons/advanced/logging-with-logger/)
- [Articolo su come gestire gli errori e le eccezioni in Elixir](https://medium.com/thecode/news-flash-elixir-is-awesome-for-handling-errors-e6c454043c17)