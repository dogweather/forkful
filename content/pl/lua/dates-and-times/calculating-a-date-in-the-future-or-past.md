---
date: 2024-01-20 17:31:58.714171-07:00
description: "Jak to zrobi\u0107: Kariera daty w przysz\u0142o\u015Bci lub przesz\u0142\
  o\u015Bci si\u0119ga jeszcze czas\xF3w przed informatyk\u0105, ale wraz z nadej\u015B\
  ciem komputer\xF3w i potrzeb\u0105 zarz\u0105dzania\u2026"
lastmod: '2024-04-05T22:50:49.876445-06:00'
model: gpt-4-1106-preview
summary: "Kariera daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci si\u0119\
  ga jeszcze czas\xF3w przed informatyk\u0105, ale wraz z nadej\u015Bciem komputer\xF3\
  w i potrzeb\u0105 zarz\u0105dzania czasem, szybko sta\u0142a si\u0119 jedn\u0105\
  \ z podstawowych funkcji programistycznych."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

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
