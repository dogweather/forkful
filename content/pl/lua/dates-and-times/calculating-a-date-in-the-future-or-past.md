---
title:                "Obliczanie daty w przyszłości lub przeszłości"
aliases:
- /pl/lua/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:58.714171-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Calculating a date in the future or past to znalezienie konkretnej daty przed lub po wyznaczonej liczbie dni. Programiści robią to, aby obsługiwać rezerwacje, terminy płatności, przypomnienia i inne funkcje związane z czasem.

## Jak to zrobić:
```Lua
os = require('os')
time = os.time() -- bieżący czas w formacie epoki Unix

-- Dodaj 7 dni do bieżącej daty
future_time = time + (7 * 24 * 60 * 60) -- 7 dni, 24 godziny, 60 minut, 60 sekund

-- Odejmij 7 dni od bieżącej daty
past_time = time - (7 * 24 * 60 * 60) -- to samo, ale odejmujemy

-- Formatuj daty by wyświetlić w przyjaznym formacie
future_date = os.date("%Y-%m-%d", future_time)
past_date = os.date("%Y-%m-%d", past_time)

print("Data w przyszłości: " .. future_date)
print("Data w przeszłości: " .. past_date)
```
Sample output:
```
Data w przyszłości: 2023-04-14
Data w przeszłości: 2023-03-31
```

## Deep Dive
Kariera daty w przyszłości lub przeszłości sięga jeszcze czasów przed informatyką, ale wraz z nadejściem komputerów i potrzebą zarządzania czasem, szybko stała się jedną z podstawowych funkcji programistycznych. W Lua, taka operacja jest relatywnie prosta dzięki wbudowanemu modułowi `os` i funkcji `os.time`, która zwraca czas w formacie epoki Unix — liczby sekund, które upłynęły od północy 1 stycznia 1970 GMT. Alternatywą jest biblioteka `luadate`, która oferuje bogatsze możliwości manipulacji datami. Funkcja `os.date` pozwala konwertować czas epoki na bardziej zrozumiały format daty.

Rozważając implementację, pamiętaj, że różne strefy czasowe i zmiana czasu z letniego na zimowy może wpłynąć na obliczanie dokładnej daty. W komputach Lua opartych na systemach POSIX (takich jak Linux), czas epoki Unix nie ulega zmianie z powodu zmian strefy czasowej, co jest znaczącą zaletą dla programistów.

## See Also
- Dokumentacja Lua 5.4 [`os` library](https://www.lua.org/manual/5.4/manual.html#6.9)
- Projekt `luadate` na GitHub: [luadate](https://github.com/Tieske/date)
- Dokładne omówienie czasu w Unix na stronie [Wikipedia: Unix time](https://en.wikipedia.org/wiki/Unix_time)
