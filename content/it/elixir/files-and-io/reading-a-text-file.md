---
date: 2024-01-20 17:54:02.427029-07:00
description: 'How to: Elixir rende la lettura di file semplice e diretta. Ecco un
  esempio.'
lastmod: '2024-03-13T22:44:43.101575-06:00'
model: gpt-4-1106-preview
summary: Elixir rende la lettura di file semplice e diretta.
title: Lettura di un file di testo
weight: 22
---

## How to:
Elixir rende la lettura di file semplice e diretta. Ecco un esempio:

```elixir
# Lettura dell'intero contenuto del file
contenuto = File.read!("esempio.txt")
IO.inspect(contenuto)

# Lettura del file riga per riga
File.stream!("esempio.txt")
|> Enum.each(fn riga -> IO.puts(riga) end)
```

Output:

```
"Il contenuto del file di esempio."
"Il primo verso."
"Il secondo verso."
```

## Deep Dive
La lettura di file in Elixir si appoggia all'ERTS (Erlang Runtime System), che gestisce efficientemente I/O file. Storicamente, Erlang (e di conseguenza Elixir) è stato usato in sistemi che richiedono alta concorrenza e per questo leggere file non blocca il sistema.

Altre alternative per la lettura di file includono l'uso di librerie come `:file`, parte della Erlang Standard Library, o stream processing con GenStage per flussi di dati più grandi.

Leggere un file riga per riga con `File.stream!` legge il file in modo pigro (lazy), che è molto efficiente con file di grandi dimensioni poiché non vengono caricati interamente in memoria.

## See Also
- [Elixir Documentation for File](https://hexdocs.pm/elixir/File.html)
- [Erlang :file module documentation](http://erlang.org/doc/man/file.html)
