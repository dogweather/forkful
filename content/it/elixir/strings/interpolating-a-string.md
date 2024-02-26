---
date: 2024-01-20 17:50:31.768803-07:00
description: "L'interpolazione di stringhe permette di inserire valori variabili all'interno\
  \ di una stringa. I programmatori la utilizzano per rendere il codice pi\xF9\u2026"
lastmod: '2024-02-25T18:49:40.998536-07:00'
model: gpt-4-1106-preview
summary: "L'interpolazione di stringhe permette di inserire valori variabili all'interno\
  \ di una stringa. I programmatori la utilizzano per rendere il codice pi\xF9\u2026"
title: Interpolazione di una stringa
---

{{< edit_this_page >}}

## What & Why?
L'interpolazione di stringhe permette di inserire valori variabili all'interno di una stringa. I programmatori la utilizzano per rendere il codice più leggibile e per comporre messaggi dinamicamente.

## How to:
Elixir rende l'interpolazione di stringhe semplice. Usi il carattere `#` seguito da `{}` per includere valori. Ecco come si fa:

```elixir
name = "Luca"
age = 25

message = "Ciao, mi chiamo #{name} e ho #{age} anni."
IO.puts(message)
```

Output:
```
Ciao, mi chiamo Luca e ho 25 anni.
```

Possiamo interpolare espressioni, non solo variabili:

```elixir
IO.puts("5 + 7 = #{5 + 7}")
```

Output:
```
5 + 7 = 12
```

## Deep Dive
L'interpolazione di stringhe in Elixir affonda le radici in linguaggi più antichi come Perl e Ruby. È superiore alla concatenazione di stringhe per efficienza e leggibilità.

Oltre a `#{}`, in altri linguaggi si usano simboli diversi per l'interpolazione. Per esempio in Python si usa `.format()` o le f-strings.

I dettagli di implementazione mostrano che quando interpoliamo una stringa, Elixir trasforma l'intera espressione in un'unico binario. Questo significa prestazioni migliori rispetto alla concatenazione di stringhe multiple.

## See Also
- [Elixir Interpolation: Elixir School](https://elixirschool.com/en/lessons/basics/strings/#interpolation)
- [Elixir's String module documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir Forum for discussion and questions](https://elixirforum.com)
