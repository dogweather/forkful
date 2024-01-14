---
title:    "Elixir: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości jest niezbędnym elementem wielu aplikacji w dzisiejszych czasach. Mogą to być aplikacje do rezerwacji spotkań, wycieczek, lub nawet prostych powiadomień o ważnych wydarzeniach. Dlatego warto poznać podstawy programowania w Elixir, aby móc wygodnie i precyzyjnie obliczać daty.

## Jak to zrobić

Pierwszym krokiem jest zaimportowanie modułu `Date` i wykorzystanie funkcji `add` lub `sub`. Na przykład, jeśli chcemy dodać 2 dni do dzisiejszej daty:

```Elixir
iex> Date.add(Date.utc_today(), 2)
{:ok, ~U[2021-04-03]}
```

Możemy również podać datę źródłową i określić jednostkę czasu (np. dni, miesiące, lata):

```Elixir
iex> Date.add(~U[2021-04-01], 1, :month)
{:ok, ~U[2021-05-01]}
```

Jeśli chcemy odjąć pewną jednostkę czasu od daty, możemy wykorzystać funkcję `sub`, na przykład aby odjąć 5 lat od naszej daty źródłowej:

```Elixir
iex> Date.sub(~U[2021-01-01], 5, :year)
{:ok, ~U[2016-01-01]}
```

## Deep Dive

Podczas obliczania daty w przyszłości lub przeszłości warto pamiętać o różnicach w kalendarzu gregoriańskim i kalendarzu juliańskim. Kalendarz juliański używa starszego systemu obliczania czasu, więc daty mogą się różnić o jeden dzień. W Elixirze, możemy wybrać kalendarz, w którym obliczanie będzie przeprowadzone poprzez użycie opcji `calendar`:

```Elixir
iex> Date.add(~U[2021-01-01], 10, :year, calendar: Calendar.ISO)
{:ok, ~U[2031-01-01]}
```

### Zobacz także

- [Dokumentacja modułu Date](https://hexdocs.pm/elixir/Date.html)
- [Porównanie kalendarzy gregoriańskiego i juliańskiego](https://www.timeanddate.com/calendar/gregorian-julian-switch.html)
- [Inne funkcje pomocne przy obliczaniu dat w Elixirze](https://elixirschool.com/en/lessons/basics/ides-and-comments/)