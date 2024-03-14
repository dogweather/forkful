---
date: 2024-01-20 17:47:19.435496-07:00
description: "Calcolare la lunghezza di una stringa significa contare il numero di\
  \ caratteri che la compongono. \xC8 una operazione fondamentale per la manipolazione\
  \ di\u2026"
lastmod: '2024-03-13T22:44:43.075194-06:00'
model: gpt-4-1106-preview
summary: "Calcolare la lunghezza di una stringa significa contare il numero di caratteri\
  \ che la compongono. \xC8 una operazione fondamentale per la manipolazione di\u2026"
title: Trovare la lunghezza di una stringa
---

{{< edit_this_page >}}

## What & Why?
Calcolare la lunghezza di una stringa significa contare il numero di caratteri che la compongono. È una operazione fondamentale per la manipolazione di testi, come validare input, limitare lunghezza, o per semplice analisi dati.

## How to:
Elixir rende semplice trovare la lunghezza di una stringa mediante la funzione `String.length/1`:

```elixir
stringa = "Ciao, mondo!"
lunghezza = String.length(stringa)
IO.puts(lunghezza)
```

Output:
```
12
```

## Deep Dive
In Elixir, le stringhe sono codificate in UTF-8, il che significa che `String.length/1` restituisce il numero di caratteri Unicode, piuttosto che semplici byte. Questa è una distinzione importante, poiché alcuni caratteri potrebbero essere rappresentati da più byte.

Se vuoi il conteggio dei byte, potresti usare `byte_size/1`:

```elixir
byte_size("Ciao, mondo!")
```

Prima dell'adozione dell'UTF-8, si usavano altri schemi di codifica che non rappresentavano sempre correttamente i caratteri internazionali. Oggi con `String.length/1` otteniamo un modo affidabile e universale di lavorare con testi da tutto il mondo.

Un'alternativa vecchio stile era usare liste di caratteri, note come charlists in Elixir:

```elixir
charlist = 'Ciao, mondo!'
length = length(charlist)  # Attenzione: Questa è la funzione length/1 generica di Elixir, non String.length/1
IO.puts(length)
```

Questo restituirà lo stesso numero di elementi, ma lavorare con charlists è sconsigliato quando si gestiscono stringhe di testo in UTF-8 in Elixir moderno.

## See Also
- Documentazione ufficiale di `String.length/1`: https://hexdocs.pm/elixir/String.html#length/1
- Una guida sulle stringhe UTF-8 in Elixir: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html
- Elixir School, per un'introduzione più ampia al linguaggio: https://elixirschool.com/en/
