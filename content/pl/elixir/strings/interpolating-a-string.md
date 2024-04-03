---
date: 2024-01-20 17:50:40.544914-07:00
description: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w to wstawianie warto\u015B\
  ci zmiennych wewn\u0105trz stringa. Programi\u015Bci u\u017Cywaj\u0105 jej do tworzenia\
  \ elastycznych wiadomo\u015Bci i\u2026"
lastmod: '2024-03-13T22:44:35.026465-06:00'
model: gpt-4-1106-preview
summary: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w to wstawianie warto\u015B\
  ci zmiennych wewn\u0105trz stringa."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

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
