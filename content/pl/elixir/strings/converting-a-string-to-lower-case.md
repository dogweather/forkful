---
date: 2024-01-20 17:38:05.288209-07:00
description: "How to: (Jak to zrobi\u0107:) ."
lastmod: '2024-04-05T21:53:36.470352-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## How to: (Jak to zrobić:)
```elixir
# Używamy funkcji String.downcase/1
original = "Szczęśliwy Programista Elixir"
lowercased = String.downcase(original)

IO.puts lowercased
# Wynik: szczęśliwy programista elixir
```

## Deep Dive (Dogłębna analiza)
W Elixirze, tak jak i w wielu innych językach programowania, operacja konwersji na małe litery jest standardowym narzędziem. Funkcja `String.downcase/1` wykorzystuje Unicode do obsługi różnych alfabetów, co jest ważne w kontekście globalizacji. Alternatywą jest własna funkcja, która przez iterację zmienia każdy znak - ale dlaczego wymyślać koło na nowo?

Wersje Elixir przed 1.3 używały `String.downcase/2` z opcjonalnym argumentem definiującym lokalizację, ale aktualnie lokalizacja jest obsługiwana automatycznie.

## See Also (Zobacz również)
- [Elixir String Module Documentation](https://hexdocs.pm/elixir/String.html)
- [Unicode Standard](http://www.unicode.org/standard/standard.html)
- [Elixir Forum Discussions](https://elixirforum.com)
