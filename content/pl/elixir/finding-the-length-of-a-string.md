---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Znalezienie długości łańcucha oznacza określenie, ile znaków zawiera dany tekst. Programiści robią to, aby kontrolować, sprawdzać lub dostosowywać dane tekstowe w swoim kodzie.

## Jak to zrobić:
W Elixir, sprawa jest prosta. Użyj funkcji `String.length/1`.

```elixir
tekst = "Witaj, świecie!"
IO.puts String.length(tekst)
```

Wyjście będzie:
```elixir
15
```

To oznacza, że łańcuch "Witaj, świecie!" ma 15 znaków.

## Deep Dive
Elixir jako język potomny Erlanga wprowadził prosty sposób na obsługę stringów, oparty na Unicode skupiając się na wydajności i poprawnej obsłudze znaków.

Znajdowanie długości łańcucha w Elixir jest szybsze i bardziej efektywne, ponieważ korzysta z listy łączonej, a nie z tablicy bajtów.

Alternatywnie można użyć funkcji `byte_size/1`, ale pamiętaj, że ta funkcja zwraca liczbę bajtów, a nie znaków. To może prowadzić do nieprawidłowych wyników dla stringów zawierających znaki Unicode.

```elixir
tekst = "ź"
IO.puts byte_size(tekst)
IO.puts String.length(tekst)
```

Wyjście będzie:
```elixir
2
1
```
Tu widać, że `byte_size/1` zwraca 2 (dlatego, że "ź" potrzebuje dwóch bajtów), a `String.length/1` poprawnie zwraca 1.

## Zobacz też:
1. [Dokumentacja Elixir: String.length/1](https://hexdocs.pm/elixir/String.html#length/1)
2. [Elixir School: Strings](https://elixirschool.com/pl/lessons/basics/strings/)
3. [Elixir Forum: discussion about string length](https://elixirforum.com/t/get-a-string-length/1504).