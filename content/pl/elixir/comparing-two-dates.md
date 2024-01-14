---
title:    "Elixir: Porównywanie dwóch dat"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Elixira, na pewno masz do czynienia z datami. Czasami zdarza się, że musisz porównać dwie daty lub upewnić się, czy jedna data jest przed lub po drugiej. W tym krótkim wpisie pokażę Ci, jak porównywać daty w Elixirze.

## Jak to zrobić

Aby porównywać daty w Elixirze, potrzebujemy dwóch ważnych funkcji: `Date.compare/2` i `Date.before?/2`. Przyjrzyjmy się przykładowemu kodowi, aby lepiej zrozumieć, jak działają te funkcje:

```elixir
date1 = ~D[2021-01-01]
date2 = ~D[2020-12-31]

Date.compare(date1, date2)
# Output: :gt

Date.before?(date1, date2)
# Output: false
```

W pierwszym przykładzie użyliśmy funkcji `Date.compare/2`, która zwróciła wynik `:gt` (greater than), ponieważ data `date1` jest po `date2`. W drugim przykładzie skorzystaliśmy z funkcji `Date.before?/2`, która zwróciła wartość logiczną `false`, ponieważ `date1` jest po `date2`.

Jeśli chcesz upewnić się, czy data jest po lub przed inna datą, możesz również skorzystać z funkcji `Date.after?/2` lub `Date.same?/2`.

## Deep Dive

Możesz się zastanawiać, jak dokładnie działa porównywanie dat w Elixirze. Otóż Elixir używa porównania atomów, więc daty są przekształcane na unikalne atomy i porównywane za pomocą porównania atomów. Dzięki temu porównanie jest bardzo szybkie i wydajne.

Warto również pamiętać, że daty w Elixirze są przechowywane jako trzyliczbowe krotki, reprezentujące rok, miesiąc i dzień. Dzięki temu można dokładnie określić, która data jest wcześniejsza lub późniejsza.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o operacjach na datach w Elixirze, możesz sprawdzić poniższe linki:

- [Dokumentacja Elixira na temat porównywania dat](https://hexdocs.pm/elixir/Date.html#compare/2)
- [Tutorial Elixira na temat operacji na datach](https://elixir-lang.org/getting-started/basic-operators.html#comparison-operators)
- [Przewodnik po Elixirze - data i czas](https://elixir-lang.org/getting-started/basic-operators.html#comparison-operators)