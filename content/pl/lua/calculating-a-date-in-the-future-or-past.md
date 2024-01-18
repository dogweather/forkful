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

## Co & Dlaczego?
Obliczanie daty w przeszłości lub przyszłości to proces, w którym programista wykorzystuje kod, aby ustalić, jaka będzie data, gdy zostanie dodana lub odebrana pewna ilość dni do bieżącej daty. Jest to przydatne w wielu różnych aplikacjach, takich jak kalendarze, systemy rezerwacji lub programy finansowe.

## Jak to zrobić:
```Lua
-- Przykład kodu w Lua do dodawania dni do bieżącej daty
local data = os.date("*t") -- pobranie bieżącej daty w formacie tabeli
data.day = data.day + 5 -- dodanie 5 dni do daty
local nowaData = os.time(data) -- konwersja do formatu czasu
print(os.date("%Y-%m-%d", nowaData)) -- wyświetlenie nowej daty w formacie rok-miesiąc-dzień
```

Przykładowy wynik: 2021-12-07

## Głębszy zanurzenie:
Obliczanie daty w przeszłości i przyszłości jest możliwe dzięki bibliotece czasu dostępnej w języku Lua. Istnieją także inne sposoby na manipulowanie datami, takie jak wykorzystanie bibliotek zewnętrznych lub funkcji dostępnych w systemie operacyjnym. W niektórych aplikacjach konieczne jest korzystanie z specjalnych formatów dat, w takim przypadku należy dostosować kod do odpowiednich wymagań.

## Zobacz także:
- [Dokumentacja Lua o bibliotece czasu](https://www.lua.org/manual/5.1/manual.html#5.8)
- [Inne sposoby manipulowania datami w Lua](https://lua-users.org/wiki/DayProg)
- [Porównanie różnych metod obliczania dat w Lua](https://stackoverflow.com/questions/18641379/how-to-increase-date-by-few-days-in-lua)