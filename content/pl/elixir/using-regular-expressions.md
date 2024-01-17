---
title:                "Korzystanie z wyrażeń regularnych"
html_title:           "Elixir: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Czym jest wyrażenie regularne (ang. Regular expression)?

Wyrażenie regularne jest ciągiem znaków, który działa jako szablon do dopasowania pewnych wzorców tekstu. Jest to często wykorzystywane narzędzie przez programistów do wykonywania operacji na tekście, takich jak wyszukiwanie, zastępowanie i filtrowanie.

## Jak używać wyrażeń regularnych w Elixirze?

Wykorzystując wbudowany moduł `Regex`, możesz używać wyrażeń regularnych w swoim kodzie Elixir. Poniżej znajdują się przykłady kodu, które pokazują, jak użyć wyrażeń regularnych w Elixirze:

```
# Wyszukuje wszystkie wystąpienia ciągu 'Elixir' w tekście
Regex.run(~r/Elixir/, "Uczymy się Elixir'a")

# Sprawdza, czy dany tekst zawiera liczbę
Regex.scan(~r/\d+/, "Elixir 1.10.0")

# Zastępuje wystąpienia 'Elixir' przez 'Ruby' w tekście
Regex.replace(~r/Elixir/, "Uczymy się Elixir'a", "Uczymy się Ruby'ego")

# Dopasowuje jedynie duże litery w tekście
Regex.scan(~r/[A-Z]+/, "Elixir")
```

## Głębszy wgląd

Wyrażenia regularne istnieją już od wielu lat i są szeroko wykorzystywane w programowaniu.  Są one również dostępne w innych językach programowania, takich jak Perl, Python czy JavaScript.

Alternatywą dla wyrażeń regularnych w Elixirze jest użycie funkcji z modułu `String`, takich jak `contains?` czy `replace`. Jednak dla bardziej złożonych operacji na tekście, użycie wyrażeń regularnych może być wygodniejsze i szybsze.

Implementacja wyrażeń regularnych w Elixirze opiera się na języku Erlang i wzorcu "bezpiecznej sekstrakcji" (ang. safe extraction). Dzięki temu, operacje te są bezpieczne i nie powinny powodować błędów w Twoim kodzie.

## Zobacz również

Więcej informacji na temat wyrażeń regularnych w Elixirze możesz znaleźć w [dokumentacji oficjalnej](https://hexdocs.pm/elixir/Regex.html) oraz [poradniku Elixir School](https://elixirschool.com/pl/lessons/advanced/pattern-matching/).