---
date: 2024-01-20 17:42:13.604867-07:00
description: "Jak zrobi\u0107: ."
lastmod: '2024-03-13T22:44:35.024488-06:00'
model: gpt-4-1106-preview
summary: .
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## Jak zrobić:
```elixir
# Załadowanie Elixir
iex> String.replace("He1l2o! E3lix4ir!", ~r/[0-9]/, "")
"Helo! Elixir!"

# Usuwanie znaków specjalnych oprócz spacji
iex> String.replace("Cześć! Jak się masz?", ~r/[[:punct:]]/, "")
"Cześć Jak się masz"

# Zamiana wielu spacji na jedną
iex> String.replace("Elixir   jest    super", ~r/\s+/, " ")
"Elixir jest super"
```

## Deep Dive
Elixir, mocno osadzony w paradygmacie funkcyjnym, traktuje tekst jako serię niezmienialnych danych. Usuwanie znaków opiera się na mechanizmach dostępnych w modułach takich jak `String`. Wzorce, czyli wyrażenia regularne (regex), pozwalają na precyzyjne określenie, które znaki mają zostać usunięte.

Historycznie, obsługa stringów i regexów wywodzi się z języków jak Perl, gdzie obróbka tekstu była niemalże formą sztuki. Elixir, używając BEAM (maszyna wirtualna Erlanga), oferuje wydajną i bezpieczną pracę z tekstami dzięki swoim immutable strings.

Alternatywną metodą jest użycie funkcji `String.graphemes/1` wraz z wyrażeśniami listowymi, które mogą sprostać niektórym zadaniom bez potrzeby sięgania po regexy.

W Elixirze istnieje też możliwość korzystania z modułów napotkanych w Erlangu, takich jak `:re`, co daje dodatkowe opcje manipulacji tekstami.

## Zobacz również
- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- [Elixir Regex Module](https://hexdocs.pm/elixir/Regex.html)
- [Wyrażenia regularne w Elixirze (Regex)](https://elixir-lang.org/getting-started/pattern-matching.html#regexes)
- [Programowanie funkcyjne w Elixir](https://elixir-lang.org/getting-started/introduction.html#functional-programming)
