---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:15:32.445186-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Pobranie aktualnej daty oznacza uzyskanie informacji o dokładnym momencie, w którym znajdujemy się w czasie. Programiści używają tej funkcji do logowania, ustalania terminów, i wielu innych zadań wymagających śledzenia czasu.

## How to: (Jak to zrobić:)
```Lua
-- Wczytanie modułu odpowiedzialnego za czas
local os_date = os.date

-- Pobranie aktualnej daty i czasu jako string
print(os_date("%Y-%m-%d %H:%M:%S")) -- output: 2023-04-01 12:30:45

-- Pobranie aktualnej daty jako tabeli
local date_table = os_date("*t")
print(date_table.year, date_table.month, date_table.day) -- output: 2023 4 1
```

## Deep Dive (Wgłębiając się)
Lua oferuje funkcje języka os.date, które służą do uzyskiwania bieżących informacji o czasie. Po raz pierwszy zostały wprowadzone w Lua 5.1. Oto alternatywne sposoby:

- **os.time()**: Zwraca czas jako timestamp (sekundy od epoch, czyli od 1 stycznia 1970).
- **os.clock()**: Mierzy czas CPU używany przez program.

Datę można formatować w wielu stylach, korzystając z różnych ciągów formatujących zgodnych z funkcją strftime w C. Na przykład, "%A" wyświetli pełną nazwę dnia tygodnia, a "%B" pełną nazwę miesiąca. Implementacja czasu w Lua wynika bezpośrednio z funkcji ANSI C, co sprawia, że jest zarówno wydajna, jak i znana dla programistów w innych językach.

## See Also (Zobacz również)
- Dokumentacja Lua `os` library: [https://www.lua.org/manual/5.4/manual.html#6.9](https://www.lua.org/manual/5.4/manual.html#6.9)
- Strftime format specifiers reference: [http://man7.org/linux/man-pages/man3/strftime.3.html](http://man7.org/linux/man-pages/man3/strftime.3.html)
