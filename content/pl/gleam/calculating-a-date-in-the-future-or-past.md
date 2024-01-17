---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Gleam: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obliczanie daty w przyszłości lub w przeszłości jest użyteczną umiejętnością, która może być wykorzystana przez programistów w różnych sytuacjach. Pozwala na obliczanie dat, które znajdują się na pewnej odległości od aktualnej daty, co jest przydatne w różnych aplikacjach, takich jak kalendarze lub przypomnienia.

## Jak to zrobić:

Obliczenie daty w przyszłości lub w przeszłości w języku Gleam jest proste i wymaga użycia modułu `Time`. Poniżej przedstawione są dwa przykłady, jeden wyliczający datę w przyszłości, a drugi w przeszłości.

```Gleam
import Gleam.Time

// Obliczenie daty 7 dni od aktualnej
let future = Time.add_days(7, Time.now())

// Obliczenie daty 3 miesiące przed aktualną
let past = Time.sub_months(3, Time.now())

// Wypisanie wyników
IO.println("W przyszłości: " ++ Time.format(future))
IO.println("W przeszłości: " ++ Time.format(past))
```

Kod wyżej wykorzystuje funkcję `add_days` i `sub_months` z modułu `Time` do obliczenia daty w przyszłości i przeszłości. Następnie wykorzystywana jest funkcja `format` do sformatowania wyników do czytelniejszej postaci.

## Deep Dive:

Funkcje `add_days` i `sub_months` w języku Gleam wykorzystują bibliotekę standardową Erlanga do obsługi dat. W tym języku, daty są reprezentowane jako liczba milisekund od 1 stycznia 1970 roku. Dzięki temu funkcje te są wydajne i nie muszą wykonywać skomplikowanych obliczeń.

Alternatywą dla użycia biblioteki standardowej w języku Gleam jest wykorzystanie biblioteki `date-time`, która oferuje więcej funkcji do obsługi dat i jest często wybierana przez programistów, którzy potrzebują zaawansowanych operacji na datach.

## Zobacz też:

- Dokumentacja języka Gleam: http://gleam.run/
- Dokumentacja biblioteki `date-time`: https://hexdocs.pm/date_time/
- Strona internetowa projektu Erlang: https://www.erlang.org/