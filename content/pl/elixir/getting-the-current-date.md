---
title:                "Elixir: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Zapewne wielu programistów zastanawia się po co w ogóle potrzebujemy pobierać aktualną datę w naszej aplikacji. Cóż, istnieje wiele powodów, dla których może to być potrzebne. Być może chcemy umożliwić użytkownikom wyświetlanie aktualnej daty lub czasu w naszej aplikacji, a może potrzebujemy uaktualniać ją w celu generowania raportów lub analiz. Bez względu na powód, pobieranie aktualnej daty jest ważną i nieodłączną częścią wielu aplikacji.

## Jak to zrobić

Aby pobrać aktualną datę w języku Elixir, możemy skorzystać z wbudowanej funkcji `Date.utc_today/0`. Powróćmy do naszych podstaw języka i przetestujmy tę funkcję w interpreterze i zobaczmy jak działa:

```Elixir
Date.utc_today()
```

Po uruchomieniu tej komendy, powinniśmy otrzymać aktualną datę w formacie `{year, month, day}`. Na przykład ` {2020, 10, 15}` oznacza 15 października 2020 roku. Możemy także przekazać argument do tej funkcji, aby otrzymać datę w wybranej strefie czasowej. Na przykład jeśli chcemy otrzymać aktualną datę w Warszawie, możemy użyć:

```Elixir
Date.utc_today("Europe/Warsaw")
```

W ten sposób możemy uaktualnić naszą aplikację o bieżącą datę i wykorzystać ją w dowolny sposób.

## Głębsza analiza

Jeśli chcemy pobrać więcej informacji na temat daty, możemy wykorzystać moduł `Calendar` w Elixirze. Udostępnia on wiele funkcji, które umożliwiają nam manipulowanie i przetwarzanie dat. Na przykład, funkcja `Calendar.day_of_week/1` przyjmuje datę jako argument i zwraca jej dzień tygodnia jako liczbę z zakresu 1-7, gdzie 1 oznacza poniedziałek, a 7 niedzielę.

```Elixir
Calendar.day_of_week({2020, 10, 15}) #=> 4
```

Możemy także użyć funkcji `Calendar.ISO.week_number/1`, aby wyznaczyć numer tygodnia w roku dla danej daty.

```Elixir
Calendar.ISO.week_number({2020, 10, 15}) #=> {2020, 42}
```

Dzięki tym funkcjom oraz wielu innym dostępnym w module `Calendar`, możemy w pełni wykorzystać możliwości Elixira w manipulowaniu datami.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o tym, jak wykorzystać daty w języku Elixir, warto zapoznać się z następującymi artykułami:

- [Dokumentacja Elixir: Calendar](https://hexdocs.pm/elixir/Calendar.html)
- [Pobieranie daty w różnych strefach czasowych w Elixir](https://cultivatehq.com/posts/working-with-timezones-and-dates-in-elixir/)
- [Moduł `Date` w Elixirze](https://elixir-lang.org/getting-started/basic-types.html#dates-and-times)

Mam nadzieję, że ten wpis był pomocny w zrozumieniu jak pobierać aktualną datę w języku Elixir. Dziękuję za przeczytanie!