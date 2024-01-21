---
title:                "Usuwanie znaków pasujących do wzorca"
date:                  2024-01-20T17:42:13.604867-07:00
model:                 gpt-4-1106-preview
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie znaków pasujących do wzorca to proces filtracji tekstu - wybierasz, które znaki "przetrwają", a które zostaną usunięte. Programiści robią to, by oczyścić dane wejściowe, ujednolicić format lub przygotować tekst do dalszej obróbki.

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