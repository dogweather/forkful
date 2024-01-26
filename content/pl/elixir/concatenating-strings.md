---
title:                "Łączenie łańcuchów znaków"
date:                  2024-01-20T17:34:34.616954-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Łączenie tekstów to klejenie ich razem. Robimy to, by tworzyć nowe zdania, wyświetlać dane czy budować wiadomości.

## How to: (Jak to zrobić:)
Elixir używa operatora `<>` do łączenia tekstów.

```elixir
name = "Ania"
greeting = "Cześć, " <> name <> "!"
IO.puts greeting
```

```
Cześć, Ania!
```

Połącz zmienne i stałe teksty:

```elixir
prefix = "Eliksir"
version = "1.12.3"
full_name = prefix <> " wersja " <> version
IO.puts full_name
```

```
Eliksir wersja 1.12.3
```

## Deep Dive (Głębsze zanurzenie)
Łączenie tekstów sięga czasów pierwszych komputerów. W Elixirze, który bazuje na Erlandzie, operacja ta jest wydajna dzięki immutable strings. Alternatywą jest używanie list i funkcji `Enum.join`, ale `<>` jest prostsze i szybsze.

Detale implementacyjne: Teksty (strings) w Elixirze są UTF-8 i operacja `<>` odpowiednio obsługuje różne kody znaków.

## See Also (Zobacz również):
- [String — Elixir v1.12.3](https://hexdocs.pm/elixir/String.html)
- [Programming Elixir ≥ 1.6](https://pragprog.com/titles/elixir16/programming-elixir-1-6/) - książka o programowaniu w Elixir.
- [Elixir School](https://elixirschool.com/pl/) - lekcje programowania w Elixir.
