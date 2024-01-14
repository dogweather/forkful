---
title:    "Elixir: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

Szukanie i zamiana tekstu jest jednym z podstawowych zadań w programowaniu. Jest to niezbędne, aby uaktualnić lub poprawić błędy w plikach i zawsze będzie przydatne podczas pracy z tekstem.

## Jak to zrobić

W Elixirze istnieje wiele sposobów na szukanie i zamianę tekstu, ale najbardziej wydajną opcją jest użycie funkcji `String.replace/4`. Przyjmie ona cztery argumenty: oryginalny napis, poszukiwany wyrażenie, wyrażenie zamienne i opcje. Na przykład:

```elixir
original_string = "Witaj, świecie!"
result = String.replace(original_string, "świecie", "Polsko")
IO.puts(result)
```

Wyjściem z tego kodu będzie `"Witaj, Polsko!"`

## Głębszy zanurzenie

Funkcja `String.replace/4` jest wysoce konfigurowalna i może być używana w różnych przypadkach. Na przykład, można zastosować opcję `count`, aby określić, ile wystąpień wyrażenia zamienić. Można również przekazać funkcję jako argument, aby przeprowadzić bardziej zaawansowane operacje na dopasowanym tekście.

```elixir
original_string = "Elixir jest niesamowitym językiem programowania!"
result = String.replace(original_string, ~r/[a-z]+/, fn match -> String.upcase(match) end)
IO.puts(result)
```

Wyjściem z tego kodu będzie `"ELIXIR JEST NIESAMOWITYM JĘZYKIEM PROGRAMOWANIA!"`

## Zobacz również

- [Dokumentacja funkcji `String.replace/4`](https://hexdocs.pm/elixir/String.html#replace/4)
- [Elixir School - Praca z tekstem](https://elixirschool.com/pl/lessons/basics/text/)
- [Elixir Cookbook - Szukanie i zamiana tekstu](https://elixircookbook.medium.com/search-and-replace-in-elixir-c8342004a9c4)