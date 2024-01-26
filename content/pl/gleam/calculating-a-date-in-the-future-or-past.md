---
title:                "Obliczanie daty w przyszłości lub przeszłości"
date:                  2024-01-20T17:30:52.001978-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Co i dlaczego? Obliczanie daty w przyszłości lub przeszłości to ustalenie nowego momentu w czasie, dodając lub odejmując wartości czasu. Programiści robią to, aby obsługiwać terminy, harmonogramy, przypomnienia i ważność danych.

## How to:
Jak to zrobić:

```gleam
import gleam/calendar.{Date, Duration}
import gleam/int
import gleam/result
import gleam/io

fn add_days_to_date(days: Int, date: Date) -> result.Result(Date, String) {
  let duration = Duration(days: days)
  calendar.Date.add(date, duration)
}

pub fn main() {
  let today = Date(day: 10, month: 4, year: 2023)
  try new_date = add_days_to_date(5, today)

  io.println(new_date)
  // Output: Ok(Date(day: 15, month: 4, year: 2023))
}
```

## Deep Dive
Zagłębienie tematu: Przesuwanie daty w przeszłość czy przyszłość to standardowa funkcjonalność w wielu językach programowania. Historia pokazuje, że obsługa czasu była zawsze ważna w komputerach - zaczynając od pracy z systemami UNIX w latach 70. W Gleam, korzystamy z modułu `calendar`, który oferuje funkcje jak `Date.add`, aby sprawnie radzić sobie z operacjami na datach. Alternatywą jest ręczne obliczanie dat, jednak jest to podatne na błędy i nie uwzględnia szczegółów jak lata przestępne.

## See Also
Zobacz również:

- [Online date calculator](https://www.timeanddate.com/date/dateadd.html)
