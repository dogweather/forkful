---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error (stderr) permette di separare i messaggi di errore dai normali output di un programma (stdout). I programmatori lo fanno per facilitare il debugging e la gestione degli errori.

## How to:
Ecco come scrivere su stderr in Elixir:

```elixir
# Invio di un messaggio semplice a stderr
:erlang.io_putc(:standard_error, "Questo è un errore\n")

# Usare IO.warn per loggare un avviso, che va di default su stderr
IO.warn("Attenzione! Qualcosa non va...\n")
```
Esempio di output:

```
Questo è un errore
Attenzione! Qualcosa non va...
```

## Deep Dive
In Elixir, stderr è un canale standard usato fin dagli albori dei sistemi UNIX-like per reportare errori e avvisi. L'utilizzo di :erlang.io_putc(:standard_error, ...) è diretto ma meno idiomatico in Elixir. `IO.warn/2` è più comune per messaggi di avviso o log e si integra con il logger di Elixir. In alternativa si può usare `:io.format(:standard_error, ...)` per formattazioni più complesse.

## See Also
- [Documentazione ufficiale Elixir IO](https://hexdocs.pm/elixir/IO.html)
- [Guida per la gestione degli errori in Elixir](https://elixir-lang.org/getting-started/try-catch-and-rescue.html)
- [UNIX Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
