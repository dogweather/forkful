---
title:                "Elixir: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędnym elementem w wielu programach Elixir. Dzięki temu można wyświetlić datę w czytelny sposób lub przekazać ją jako argument do innych funkcji. W tym wpisie dowiesz się, jak w prosty sposób przekonwertować datę na ciąg znaków w Elixir.

## Jak to zrobić

W celu przekonwertowania daty na ciąg znaków w Elixir, możemy skorzystać z funkcji `Calendar.strftime/2`. Przyjmuje ona dwa argumenty - format daty i wartość daty. Na przykład:

```Elixir
date = {{2019, 10, 31}, {23, 59, 59}}
Calendar.strftime(date, "%d-%m-%Y %H:%M:%S")
```

W wyniku otrzymamy następujący ciąg znaków:

`31-10-2019 23:59:59`

Możemy również wykorzystać funkcję `DateTime.to_iso8601/2`, która przekonwertuje datę do formatu ISO 8601. Przykładowe użycie:

```Elixir
date = {{2020, 01, 01}, {12, 00, 00}}
DateTime.to_iso8601(date, extended: true)
```

Otrzymamy wówczas taki ciąg znaków:

`2020-01-01T12:00:00+00:00`

## Głębszy zanurzenie

W przypadku funkcji `Calendar.strftime/2` możemy wykorzystać różne symbole, aby dostosować format daty do swoich potrzeb. Na przykład `%Y` oznacza rok w formacie czterocyfrowym, zaś `%y` - dwucyfrowym. Pełną listę symboli oraz ich opis możesz znaleźć w dokumentacji Elixir. W przypadku funkcji `DateTime.to_iso8601/2` możemy określić, czy chcemy wyświetlić czas w formacie rozszerzonym czy skróconym, a także ustawić strefę czasową.

## Zobacz także

- Dokumentacja Elixir na temat konwersji daty na ciąg znaków: https://hexdocs.pm/elixir/Calendar.html#strftime/2
- Przykłady użycia funkcji `Calendar.strftime/2`: https://www.tutorialspoint.com/elixir/elixir_calendar_module.htm
- Dokumentacja Elixir na temat konwersji daty do formatu ISO 8601: https://hexdocs.pm/elixir/DateTime.html#to_iso8601/2