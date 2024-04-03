---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:35.417054-07:00
description: "Wyra\u017Cenia regularne (regex) w j\u0119zyku Elixir s\u0105 u\u017C\
  ywane do wyszukiwania, dopasowywania oraz manipulowania ci\u0105gami znak\xF3w na\
  \ podstawie okre\u015Blonych wzorc\xF3w.\u2026"
lastmod: '2024-03-13T22:44:35.030939-06:00'
model: gpt-4-0125-preview
summary: "Wyra\u017Cenia regularne (regex) w j\u0119zyku Elixir s\u0105 u\u017Cywane\
  \ do wyszukiwania, dopasowywania oraz manipulowania ci\u0105gami znak\xF3w na podstawie\
  \ okre\u015Blonych wzorc\xF3w."
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
weight: 11
---

## Jak to zrobić:
Elixir używa modułu `Regex`, wykorzystując bibliotekę regex Erlanga, do operacji z regex. Oto podstawowe zastosowania:

```elixir
# Dopasowywanie wzorca - Zwraca pierwsze dopasowanie
match_result = Regex.run(~r/hello/, "hello world")
IO.inspect(match_result) # Wynik: ["hello"]

# Znajdowanie wszystkich dopasowań
all_matches = Regex.scan(~r/\d/, "There are 2 apples and 5 oranges.")
IO.inspect(all_matches) # Wynik: [["2"], ["5"]]

# Zastępowanie części ciągu znaków
replaced_string = Regex.replace(~r/\s+/, "Elixir is fun", "_")
IO.inspect(replaced_string) # Wynik: "Elixir_is_fun"
```

Dla bardziej skomplikowanych wzorców i funkcjonalności, możesz rozważyć użycie bibliotek stron trzecich, chociaż dla większości podstawowych zadań związanych z ciągami znaków i dopasowywaniem wzorców, wbudowany moduł `Regex` w Elixirze jest dość potężny.

Aby wykonać dopasowanie niezależne od wielkości liter, użyj opcji `i`:

```elixir
case_insensitive_match = Regex.run(~r/hello/i, "Hello World")
IO.inspect(case_insensitive_match) # Wynik: ["Hello"]
```

Wyrażenia regularne mogą być prekompilowane dla efektywności podczas wielokrotnego używania:

```elixir
precompiled_regex = Regex.compile!("hello")
match_result_precompiled = Regex.run(precompiled_regex, "hello world")
IO.inspect(match_result_precompiled) # Wynik: ["hello"]
```

Elixir wspiera również nazwane grupy ujęć, które mogą być bardzo przydatne do ekstrakcji określonych części ciągu znaków, jednocześnie czyniąc kod bardziej zrozumiałym:

```elixir
date_string = "2023-04-15"
pattern = ~r/(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/
{:ok, captures} = Regex.run(pattern, date_string, capture: :all_names)
IO.inspect(captures) # Wynik: %{"year" => "2023", "month" => "04", "day" => "15"}
```

Ten krótki przegląd podkreśla łatwość, z jaką Elixir obsługuje wyrażenia regularne, umożliwiając potężne techniki manipulacji ciągami znaków i ekstrakcji danych.
