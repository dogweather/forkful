---
date: 2024-01-20 17:55:35.941248-07:00
description: 'Come Fare: .'
lastmod: '2024-03-13T22:44:43.099504-06:00'
model: gpt-4-1106-preview
summary: .
title: Lettura degli argomenti della riga di comando
weight: 23
---

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
