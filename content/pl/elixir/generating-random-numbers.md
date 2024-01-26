---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:48:46.776951-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generowanie liczb losowych to proces wytwarzania nieliniowych, trudno przewidywalnych liczb. Programiści używają go do testowania, symulacji, gier i w zabezpieczeniach.

## How to:
W Elixirze losowe wartości generujemy wygodnie z modułu `:rand`.

```Elixir
# Początkowe ziarno
:rand.seed(:exsplus, {1234, 123456, 7890123})

# Losowa liczba
random_number = :rand.uniform()
IO.puts(random_number)
```

Wyjście:
```
0.44358461759416275
```

A także dla liczby całkowitej z zakresu:

```Elixir
# Losowa liczba z zakresu 1-10
random_integer = :rand.uniform(10)
IO.puts(random_integer)
```

Wyjście:
```
7
```

## Deep Dive
Moduł `:rand` z Elixira bazuje na algorytmach z Erlanga, jak `exsplus`. Historia pokazuje, że pierwotne metody (jak np. liniowe kongruencje) były mniej skuteczne w tworzeniu prawdziwie losowych sekwencji. 

Alternatywą dla `:rand` jest korzystanie z zewnętrznych bibliotek, jak `:exrop` (kryptograficznie bezpieczny generator liczb losowych). 

Kluczowym elementem w generowaniu liczb losowych jest ziarno (`seed`). Jego zastosowanie pozwala na odtworzenie tej samej sekwencji liczb losowych - przydatne np. w testach.

## See Also
- Oficjalna dokumentacja Elixira na temat modułu `:rand`: https://hexdocs.pm/elixir/1.13.4/Random.html
- Erlang `:rand` module documentation: http://erlang.org/doc/man/rand.html
- Repozytorium `:exrop` na GitHubie: https://github.com/antipax/exrop
