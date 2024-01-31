---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
simple_title:         "Maiuscolizzare una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa significa trasformare la prima lettera di ogni parola in maiuscolo. Lo facciamo per conformità a regole grammaticali, per migliorare la leggibilità o per fini estetici in titoli e intestazioni.

## How to:
Elixir facilita la capitalizzazione di stringhe con la funzione `String.capitalize/1`. Ecco un esempio d'uso:

```elixir
stringa_originale = "amici programmatori, benvenuti!"
stringa_capitalizzata = String.capitalize(stringa_originale)
IO.puts stringa_capitalizzata
```

Output:

```
Amici programmatori, benvenuti!
```

## Deep Dive
In Elixir, capitalizzare una stringa è un'operazione comune, ed è importante comprendere come funziona. Di solito, `String.capitalize/1` è sufficiente per risolvere i casi comuni, ma attenzione quando si tratta di caratteri unicode o di lingue con regole specifiche di capitalizzazione.

Prima di Elixir, in altri linguaggi come Ruby o Python, abbiamo funzioni simili come `capitalize` e `title`. Tuttavia, Elixir gestisce in modo nativo le peculiarità di Unicode grazie al supporto della libreria standard.

Elixir usa il modulo `String` che incorpora le funzioni per manipolare le stringhe, incluso il capitale. Questo modulo lavora direttamente con le stringhe in codifica UTF-8, quindi supporta un set di caratteri ampio, includendo molti alfabeti non latini.

Per situazioni diverse, come titoli o nomi, potresti trovare librerie esterne come `Casing` che possono offrire una capitalizzazione più sofisticata e configurabile.

## See Also
- Documentazione ufficiale di Elixir su `String.capitalize/1`: https://hexdocs.pm/elixir/String.html#capitalize/1
- Elixir School per un approfondimento sulle stringhe in Elixir: https://elixirschool.com/en/lessons/basics/strings/
- La libreria `Casing` per Elixir: https://hex.pm/packages/casing
