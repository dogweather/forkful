---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:37:46.911160-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie daty polega na konwersji tekstowej reprezentacji daty do formatu, który można łatwo przetworzyć w programie. Programiści parsują daty, aby umożliwić manipulację i porównywanie dat, a także do weryfikacji i formatowania danych wejściowych użytkownika.

## Jak to zrobić:
Poniżej znajdziesz kod w Lua do parsowania daty ze stringa:

```Lua
-- Przykładowy string z datą
local dateString = "2023-03-15"

-- Funkcja do parsowania
function parseDate(str)
  local year, month, day = str:match("(%d+)-(%d+)-(%d+)")
  return { year = tonumber(year), month = tonumber(month), day = tonumber(day) }
end

-- Użycie funkcji
local dateTable = parseDate(dateString)

-- Wydrukowanie wyniku
print(string.format("Rok: %d, Miesiąc: %d, Dzień: %d", dateTable.year, dateTable.month, dateTable.day))
```

Wyjście:
```
Rok: 2023, Miesiąc: 3, Dzień: 15
```

## Glebokie Zanurzenie
Parsowanie daty z stringa ma swoje korzenie w pierwszych dniach programowania, a potrzeba ta wzrosła z rozwojem aplikacji webowych i systemów zarządzania bazami danych. Lua, nie posiadając wbudowanego wsparcia dla dat i czasu w sposobie jaki mają inne języki, jak Python czy JavaScript, wymaga od programistów pisania własnych funkcji lub korzystania z zewnętrznych bibliotek jak `lua-date` dla bardziej złożonych zadań. Powyższy przykład wykorzystuje podstawowe wyrażenia regularne do ekstrakcji elementów daty, co jest proste i skuteczne, ale nie uwzględnia walidacji danych — coś, co biblioteki zewnętrzne robią lepiej.

## Zobacz Również
Aby poszerzyć swoją wiedzę o parsowaniu dat w Lua:

- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/) – Dokumentacja Lua z przykładami.
- [lua-date](https://github.com/Tieske/date) – Potężna biblioteka do zarządzania datami w Lua.
- [Wikipedia: ISO 8601](https://pl.wikipedia.org/wiki/ISO_8601) – Standard formatowania daty i czasu, którego użyto w przykładzie.
