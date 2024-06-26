---
date: 2024-01-20 17:38:02.568509-07:00
description: "How to: In Elixir, downcase una stringa \xE8 un gioco da ragazzi con\
  \ `String.downcase/1`."
lastmod: '2024-03-13T22:44:43.071355-06:00'
model: gpt-4-1106-preview
summary: "In Elixir, downcase una stringa \xE8 un gioco da ragazzi con `String.downcase/1`."
title: Conversione di una stringa in minuscolo
weight: 4
---

## How to:
In Elixir, downcase una stringa è un gioco da ragazzi con `String.downcase/1`.

```elixir
original = "Salve, Mondo!"
lowercased = String.downcase(original)

IO.puts lowercased
# Output: salve, mondo!
```

## Deep Dive
Elixir usa Unicode, quindi `String.downcase/1` gestisce bene più lingue, non solo l'inglese. La normalizzazione Unicode è importante perché certi grafemi possono avere rappresentazioni multiple. Alternativamente, potresti usare `String.downcase/2` se hai bisogno di specificare la locale. Nota bene: prima di Elixir 1.3, la funzione `String.downcase/1` era fornita dalla libreria esterna `:unicode_util_compat`.

ESEMPI DI ALTERNATIVE:
- Utilizzo di una libreria esterna come `:downcase` per specifici casi d'uso.
- Scrittura manuale di una funzione che itera sui caratteri di una stringa e li trasforma singolarmente.

### Implementazione Interessante:
Internamente, `String.downcase/1` converte la stringa in una lista di punti di codice Unicode e poi itera su di essa, applicando la trasformazione Unicode per il lowercase secondo le specifiche di Unicode NFD (Normalization Form Decomposition).

## See Also
- Documentazione ufficiale di Elixir per `String.downcase/1`: https://hexdocs.pm/elixir/String.html#downcase/1
- Unicode standard per il case mapping: https://www.unicode.org/reports/tr21/tr21-5.html
- Blog post sulle sfide del lowercasing in Unicode: https://blog.usejournal.com/elixir-and-unicode-friendliness-elixir-s-strings-are-on-a-different-level-a66b0cd29f6d
