---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Lua: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości to proces określania dokładnego dnia, który występuje po lub przed określoną datą. Programiści to robią, aby zarządzać danymi czasu, np. do tworzenia harmonogramów czy przypomnień.

## Jak to zrobić:
Obliczanie daty w przyszłości lub przeszłości w Lua jest proste. Użyjemy wbudowanej funkcji `os.time()`, która zwraca aktualny czas w sekundach, a `os.date()` do formatowania daty:

```Lua
-- Obliczanie daty za 7 dni
local czas_teraz = os.time()
local sekundy_na_dobe = 60 * 60 * 24 -- 60 sekund, 60 minut, 24 godziny
local dni_w_przyszlosci = 7

local czas_w_przyszlosci = czas_teraz + sekundy_na_dobe * dni_w_przyszlosci
local data_w_przyszlosci = os.date('%d-%m-%Y', czas_w_przyszlosci)
print(data_w_przyszlosci) -- Wydrukuj datę.
```

Podobnie, możemy obliczyć datę w przeszłości odejmując dni:

```Lua
-- Obliczanie daty 7 dni temu
local dni_w_przeszlosci = 7
local czas_w_przeszlosci = czas_teraz - sekundy_na_dobe * dni_w_przeszlosci
local data_w_przeszlosci = os.date('%d-%m-%Y', czas_w_przeszlosci)
print(data_w_przeszlosci) -- Wydrukuj datę.
```

## Deep Dive
Obliczanie daty w przyszłości lub przeszłości to powszechna praktyka od czasów maszyn liczących, kiedy to stosowano algorytmy takie jak mechanizm Zeller'a. Alternatywą dla naszego podejścia w Lua są bibloteki do zarządzania czasem, takie jak date.lua, które oferują więcej funkcji i elastyczności. W Lua, `os.time()` zwraca czas w sekundach od pewnej epoki, zwykle od 00:00:00 UTC, 1 stycznia 1970, co jest typowym zachowaniem dla systemów Unix.

## Zobacz również
- Dokumentacja Lua: https://www.lua.org/manual/5.4/manual.html#6.9
- Pakiet date dla Lua: https://olivinelabs.com/date/