---
date: 2024-01-20 17:32:37.199272-07:00
description: "How to: W Elixirze por\xF3wnujemy daty u\u017Cywaj\u0105c modu\u0142\
  u `DateTime`. Sp\xF3jrz."
lastmod: '2024-03-13T22:44:35.056683-06:00'
model: gpt-4-1106-preview
summary: "W Elixirze por\xF3wnujemy daty u\u017Cywaj\u0105c modu\u0142u `DateTime`."
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

## How to:
W Elixirze porównujemy daty używając modułu `DateTime`. Spójrz:

```elixir
date1 = ~N[2023-03-14 14:00:00]
date2 = ~N[2023-03-15 14:00:00]

# Sprawdzamy, czy date1 jest wcześniejsza
IO.puts(DateTime.compare(date1, date2) == :lt)  # Wypisze 'true'

# Sprawdzamy, czy daty są identyczne
IO.puts(DateTime.compare(date1, date1) == :eq)  # Wypisze 'true'

# Pobieramy różnicę w sekundach między date1 i date2
difference = DateTime.diff(date2, date1)
IO.puts(difference)  # Wypisze '86400', co odpowiada jednemu dniu
```

Proste? Proste.

## Deep Dive
Porównywanie dat to stary jak świat problem, ale moduły takie jak `DateTime` w Elixirze to rozwiązują elegancko. Kiedyś bazowano na bardziej prymitywnych funkcjach, ale postęp języków programowania wspiera twórców w dostarczaniu coraz bardziej wyrafinowanych rozwiązań.

Alternatywy? Możesz użyć `NaiveDateTime` dla prostszych przypadków, gdzie strefy czasowe nie grają roli. Jeśli potrzebujesz większej precyzji, kieruj się ku `Time` dla samej godziny, minuty itp.

Implementacja porównywania dat w Elixirze opiera się na liczbach całkowitych, liczonych jako milisekundy od pewnej epoki (Unix Epoch), co pozwala uniknąć pułapek związanych z precyzją liczb zmienno-przecinkowych.

## See Also
Sprawdź także:
- Dokumentacja `DateTime`: https://hexdocs.pm/elixir/DateTime.html
- Wprowadzenie do modułu `NaiveDateTime`: https://hexdocs.pm/elixir/NaiveDateTime.html
- Moduł `Time` i jego zastosowania: https://hexdocs.pm/elixir/Time.html

Szukaj inspiracji w dokumentacji i grzeb w kodzie – to najlepsza nauka.
