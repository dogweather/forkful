---
date: 2024-01-20 17:42:25.353191-07:00
description: "How to (Come fare) In Elixir, possiamo utilizzare la funzione `String.replace/4`\
  \ per rimuovere i caratteri che corrispondono a un dato pattern utilizzando\u2026"
lastmod: '2024-04-05T22:38:42.559445-06:00'
model: gpt-4-1106-preview
summary: How to (Come fare) In Elixir, possiamo utilizzare la funzione `String.replace/4`
  per rimuovere i caratteri che corrispondono a un dato pattern utilizzando le espressioni
  regolari.
title: Eliminazione di caratteri che corrispondono a un pattern
weight: 5
---

## How to (Come fare)
In Elixir, possiamo utilizzare la funzione `String.replace/4` per rimuovere i caratteri che corrispondono a un dato pattern utilizzando le espressioni regolari.

```Elixir
stringa_originale = "Ciao, Mondo! 1234"

# Rimuovi tutti i numeri
stringa_senza_numeri = String.replace(stringa_originale, ~r/\d/, "")
IO.puts stringa_senza_numeri
# Output: Ciao, Mondo! 

# Rimuovi la punteggiatura
stringa_senza_punteggiatura = String.replace(stringa_originale, ~r/[[:punct:]]/, "")
IO.puts stringa_senza_punteggiatura
# Output: Ciao Mondo 1234

# Rimuovi vocali
stringa_senza_vocali = String.replace(stringa_originale, ~r/[aeiou]/i, "")
IO.puts stringa_senza_vocali
# Output: C, Mnd! 1234
```

## Deep Dive (Approfondimento)
La manipolazione di stringhe e pattern matching sono concetti molto vecchi nella programmazione. Le origini risalgono ai primi linguaggi di scripting e agli ambienti UNIX dove le espressioni regolari erano uno strumento chiave per la manipolazione del testo. 

In Elixir, la manipolazione di stringhe è nativamente supportata e le espressioni regolari sono fornite attraverso la libreria `Regex`. Elixir sfrutta la potente engine di espressioni regolari di Erlang, chiamata `re`. 

Un'alternativa al metodo `String.replace/4` per eliminare caratteri è l'uso di funzioni come `String.slice/3` oppure combinazioni di funzioni come `String.split/2` seguite da `Enum.join/2` per partizionare e riassemblare le stringhe, escludendo le parti non desiderate.

Concettualmente, la rimozione di caratteri che corrispondono a un pattern si accentra sull'immutabilità delle stringhe in Elixir: ogni volta che "modifichiamo" una stringa, stiamo in realtà creando una nuova stringa con i cambiamenti applicati.

## See Also (Vedi Anche)
- [Elixir Regex Documentation](https://hexdocs.pm/elixir/Regex.html)
- [Programming Elixir ≥ 1.6 book by Dave Thomas](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
- [Learn You Some Erlang for Great Good! - Regex (per comprendere la base su cui Elixir costruisce)](https://learnyousomeerlang.com/regular-expressions)
