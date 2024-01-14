---
title:                "Elixir: Wyszukiwanie i zastępowanie tekstu"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego?

Wielu programistów codziennie musi wykonywać operacje wyszukiwania i wymiany tekstu w swoim kodzie. Dzięki Elixirowi można to robić szybko i efektywnie, co zmniejsza ilość pracy i pozwala skupić się na ważniejszych zadaniach.

## Jak to zrobić?

Kodowanie w Elixirze jest bardzo intuicyjne i czytelne. Aby wykonać wyszukiwanie i wymianę tekstu, należy użyć funkcji `String.replace/3`. Przykładowy kod może wyglądać następująco:

```Elixir
text = "Witaj, świecie!"
text = String.replace(text, "świecie", "tu")
IO.puts(text)
```
**Output:** "Witaj, tu!"

Aby wykonać wymianę wszystkich wystąpień danego tekstu, należy dodać opcję `global: true`:

```Elixir
text = "25 grudnia to święto Bożego Narodzenia!"
text = String.replace(text, "święto", "wolne", global: true)
IO.puts(text)
```
**Output:** "25 grudnia to wolne Bożego Narodzenia!"

## Głębszy zanurzenie

Elixir oferuje również wiele innych funkcji do manipulacji tekstem, takich jak `String.split/2` czy `String.capitalize/1`. Ze względu na dynamiczną naturę języka, można śmiało eksperymentować z różnymi funkcjonalnościami i dostosować je do swoich potrzeb.

Należy również zauważyć, że Elixir jest językiem funkcyjnym, więc wszystkie operacje na tekście wykonują się na kopii oryginalnego tekstu, co zapobiega niepożądanym zmianom.

## Zobacz również

- Dokumentacja Elixir: https://elixir-lang.org/getting-started/introduction.html
- Oficjalny poradnik Elixir: https://elixir-lang.org/getting-started/introduction.html
- Przykładowe kody i projekty w Elixirze: https://github.com/elixir-lang/elixir