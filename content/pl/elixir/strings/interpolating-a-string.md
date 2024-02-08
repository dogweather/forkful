---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:50:40.544914-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Interpolacja łańcuchów znaków to wstawianie wartości zmiennych wewnątrz stringa. Programiści używają jej do tworzenia elastycznych wiadomości i dynamicznego kodu bez konieczności ciągłego łączenia ciągów znaków.

## How to: (Jak to zrobić:)
```elixir
name = "Łukasz"
age = 28

# Standard interpolation
greeting = "Cześć, mam na imię #{name} i mam #{age} lata."
IO.puts greeting
# Output: Cześć, mam na imię Łukasz i mam 28 lata.

# With expressions
info = "Za rok będę miał #{age + 1} lat."
IO.puts info
# Output: Za rok będę miał 29 lat.
```

## Deep Dive (Dogłębna analiza)
W Elixirze, interpolacja stringów jest implementowana za pomocą znaku `#` i nawiasów klamrowych `{}`. Została zapożyczona z języka Ruby i jest wygodniejsza niż ciągłe łączenie łańcuchów za pomocą operatora `<>`. Pod maską, Elixir przekształca interpolację w konkatenację, dzięki czemu jest również wydajna.

Alternatywnie możesz użyć funkcji `String.concat/2` albo operatora `<>`, ale to zazwyczaj prowadzi do bardziej rozwlekłego i mniej czytelnego kodu, zwłaszcza gdy łączy się wiele wartości.

## See Also (Zobacz również)
- [Elixir - String Docs](https://hexdocs.pm/elixir/String.html)
- [Programming Elixir by Dave Thomas](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
- [Elixir School - Strings](https://elixirschool.com/en/lessons/basics/strings/)
