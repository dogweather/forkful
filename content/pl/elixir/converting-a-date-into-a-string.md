---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Elixir: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Chciałbyś wiedzieć, jak przekonwertować datę na ciąg znaków? Jest to ważna umiejętność, której potrzebujesz, aby w łatwy sposób wyświetlać daty w swoim programie napisanym w języku Elixir.

## Jak to zrobić

```Elixir
date = ~D[2021-01-01]
date |> to_string |> IO.puts
# Wynik: "2021-01-01"
```

Czasami możesz chcieć dostosować format daty. W takim przypadku możesz użyć funkcji `format/2` i podać własny format jako argument.

```Elixir
date = ~D[2021-01-01]
date |> format("{0}/{1}/{2}", "-", "-") |> IO.puts
# Wynik: "2021-01-01"
```

Możesz także manipulować datami przy użyciu funkcji takich jak `add/2`, `subtract/2` i `day_of_week/1`.

## Głębsza analiza

Przekonwertowanie daty na ciąg znaków może być proste, ale jednocześnie bardzo istotne. W języku Elixir można wykorzystać wiele funkcji z biblioteki standardowej, takich jak `Calendar`, `DateTime` i `Date`, aby przeprowadzić różne operacje na datach. Aby uzyskać więcej informacji na temat tych funkcji, polecam przeczytać dokumentację języka Elixir.

## Zobacz również

- [Dokumentacja języka Elixir] (https://hexdocs.pm/elixir/Calendar.html)
- [Kalendarz] (https://elixir-lang.org/getting-started/basic-types.html#calendar)
- [Biblioteka daty i czasu] (https://elixir-lang.org/getting-started/basic-types.html#date-time)