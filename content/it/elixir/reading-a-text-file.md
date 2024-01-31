---
title:                "Lettura di un file di testo"
date:                  2024-01-20T17:54:02.427029-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura di un file di testo"

category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Leggere un file di testo in Elixir significa accedere al contenuto di un file salvato sul disco per manipolarlo o analizzarlo. I programmatori lo fanno per elaborare dati, configurazioni o input esterni all'applicazione.

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
