---
title:                "Elixir: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest wielką częścią naszego życia. Często musimy tworzyć aplikacje, które wymagają obliczenia daty w przeszłości lub przyszłości. Dzięki Elixir możemy łatwo wykonać takie obliczenia, co pozwala nam na oszczędność czasu i skupienie się na innych aspektach naszego projektu.

## Jak to zrobić

Obliczenie daty w przeszłości lub przyszłości w Elixir jest bardzo proste. Wystarczy użyć funkcji `DateTime.add/2` lub `DateTime.subtract/2`, które obliczą nowy datę na podstawie podanej daty oraz liczby dni, tygodni, miesięcy czy lat. W poniższym przykładzie dodamy 5 dni do dzisiejszej daty:

```
Elixir
iex> dzis = DateTime.utc_now()
~U[2020-11-15 10:00:00.000000Z]
iex> DateTime.add(dzis, 5, :days)
~U[2020-11-20 10:00:00.000000Z]
```

Możemy również użyć funkcji `DateTime.to_string/2` w celu sformatowania daty do czytelniejszej postaci. W poniższym przykładzie zmienimy format daty na "DD/MM/YYYY":

```
Elixir
iex> DateTime.to_string(dzis, "{0:%d}/{0:%m}/{0:%Y}")
"15/11/2020"
```


## Głębszy zanurzenie

Funkcje odpowiedzialne za obliczanie daty w przeszłości lub przyszłości zostały zaimplementowane w module `DateTime`, który jest częścią standardowej biblioteki Elixir. Warto zauważyć, że funkcje te zwracają nową datę, a nie modyfikują oryginalnej, co jest zgodne z ogólnym założeniem języka Elixir o niemutowalności danych.

Drugim ważnym elementem jest fakt, że Elixir wykorzystuje czas uniwersalny UTC (Coordinated Universal Time), co oznacza, że nie musimy się martwić o strefy czasowe czy zmianę czasu letniego i zimowego.

## Zobacz również

- [Dokumentacja Elixir - DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Książka "Programming Elixir" przez Davida Thomasa i José Valima](https://pragprog.com/titles/elixir/programming-elixir/)