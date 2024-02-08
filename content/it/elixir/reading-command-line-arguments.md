---
title:                "Lettura degli argomenti della riga di comando"
date:                  2024-01-20T17:55:35.941248-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura degli argomenti della riga di comando"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Leggere gli argomenti della riga di comando significa acquisire dati inseriti dall'utente quando avvia il tuo programmino da terminale. I programmatori lo fanno per personalizzare l'esecuzione del codice senza dover riscrivere il sorgente.

## Come Fare:

```elixir
# script.exs

# Leggere tutti gli argomenti
args = System.argv()

# Stampare gli argomenti
Enum.each(args, fn arg -> IO.puts(arg) end)

# Uso: elixir script.exs ciao mondo
# Output:
# ciao
# mondo
```

## Approfondimento

In Elixir, `System.argv()` è il cavallo di battaglia per acciuffare gli argomenti da riga di comando. Prima di Elixir, altri linguaggi come Ruby o Python facevano una cosa simile ma con le loro twistate. Altre strade? Potresti usar `OptionParser` per argomenti più komplicati, con opzioni e flags. La funzionalità nasce nel cuore della VM di Erlang, che maneggia parametri da quando i telefoni erano grossi come mattoni.

## Vedi Anche:

- [Elixir School - Command Line Applications](https://elixirschool.com/en/lessons/advanced/escripts/)
- [Elixir's official `OptionParser` documentation](https://hexdocs.pm/elixir/OptionParser.html)
- [Erlang --init documentation](http://erlang.org/doc/man/init.html)
