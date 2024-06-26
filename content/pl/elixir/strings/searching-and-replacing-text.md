---
date: 2024-01-20 17:57:59.636146-07:00
description: "How to: \"G\u0142\u0119bsze spojrzenie\" Historia funkcji stringowych\
  \ w Elixirze nawi\u0105zuje do systemu pattern matching w Erlangu. Wyszukiwanie\
  \ i zamiana opieraj\u0105 si\u0119\u2026"
lastmod: '2024-04-05T22:50:49.333439-06:00'
model: gpt-4-1106-preview
summary: "\"G\u0142\u0119bsze spojrzenie\" Historia funkcji stringowych w Elixirze\
  \ nawi\u0105zuje do systemu pattern matching w Erlangu."
title: Wyszukiwanie i zamiana tekstu
weight: 10
---

## How to:
"Jak to zrobić?"

```elixir
# Podstawowa zamiana w ciągu znaków
original_text = "Witaj świecie, Elixir jest niesamowity!"
new_text = String.replace(original_text, "świecie", "Elixira")

IO.puts new_text
# Wynik: "Witaj Elixira, Elixir jest niesamowity!"

# Wyszukiwanie za pomocą wyrażeń regularnych
regex = ~r/jest (\w+)/
replacement = "to \\1 i wiele więcej"

IO.puts String.replace(original_text, regex, replacement)
# Wynik: "Witaj świecie, Elixir to niesamowity i wiele więcej"

# Zamiana wielu wzorców na raz
patterns = [{"Elixir", "Phoenix"}, {"jest", "to"}, {"niesamowity", "fantastyczny"}]

IO.puts Enum.reduce(patterns, original_text, fn {from, to}, acc -> 
  String.replace(acc, from, to) 
end)
# Wynik: "Witaj świecie, Phoenix to fantastyczny!"
```

## Deep Dive
"Głębsze spojrzenie"

Historia funkcji stringowych w Elixirze nawiązuje do systemu pattern matching w Erlangu. Wyszukiwanie i zamiana opierają się na potężnej składni wyrażeń regularnych, zapożyczonej z Perl'a. Alternatywą może być wykorzystanie funkcji związanych ze strumieniami (Streams), do przetwarzania dużych plików bez potrzeby ich wczytywania do pamięci. Sprawność implementacji tych funkcji w Elixirze pozwala na wydajne zarządzanie łańcuchami znaków, co jest kluczowe przy obsłudze protokołów sieciowych czy przetwarzaniu dużych zbiorów danych.

## See Also
"Zobacz też"

- Elixir Documentation on Strings: https://hexdocs.pm/elixir/String.html
- RegEx in Elixir (using `Regex` module): https://hexdocs.pm/elixir/Regex.html
- Chris McCord's "Metaprogramming Elixir" for patterns and matching: https://pragprog.com/titles/cmelixir/metaprogramming-elixir/
- Elixir School on Strings and Pattern Matching: https://elixirschool.com/en/lessons/basics/strings/
