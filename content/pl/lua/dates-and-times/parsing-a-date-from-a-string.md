---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:54.155607-07:00
description: "Przetwarzanie daty z ci\u0105gu znak\xF3w polega na konwertowaniu tekstowych\
  \ reprezentacji dat i czasu na format, kt\xF3ry mo\u017Cna \u0142atwo manipulowa\u0107\
  , przechowywa\u0107 lub\u2026"
lastmod: '2024-03-13T22:44:35.549468-06:00'
model: gpt-4-0125-preview
summary: "Przetwarzanie daty z ci\u0105gu znak\xF3w polega na konwertowaniu tekstowych\
  \ reprezentacji dat i czasu na format, kt\xF3ry mo\u017Cna \u0142atwo manipulowa\u0107\
  , przechowywa\u0107 lub por\xF3wnywa\u0107 w programie Lua."
title: "Analiza sk\u0142adniowa daty z \u0142a\u0144cucha znak\xF3w"
weight: 30
---

## Jak to zrobić:
Lua nie ma wbudowanego wsparcia dla manipulacji datą i czasem poza ograniczoną funkcjonalnością, jaką zapewniają funkcje `os.date` i `os.time`. Jednakże mogą one być wykorzystane do podstawowego parsowania, a dla bardziej złożonych wymagań można użyć biblioteki zewnętrznej `luadate`.

**Korzystanie z `os.date` i `os.time`:**
```lua
-- Konwersja czytelnej dla człowieka daty na znacznik czasu i z powrotem
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local year, month, day, hour, minute, second = dateString:match(pattern)

local timestamp = os.time({
  year = year,
  month = month,
  day = day,
  hour = hour,
  min = minute,
  sec = second
})

-- Konwersja znacznika czasu z powrotem na format czytelny dla człowieka
local formattedDate = os.date("%Y-%m-%d %H:%M:%S", timestamp)
print(formattedDate)  -- Wynik: 2023-09-21 15:00:00
```

**Korzystanie z `luadate` (biblioteka stronna):**
Aby użyć `luadate`, upewnij się, że jest zainstalowana za pomocą LuaRocks lub menedżera pakietów według wyboru. `luadate` dodaje obszerne możliwości parsowania i manipulowania datą i czasem.

```lua
local date = require('date')

-- Bezpośrednie parsowanie ciągu daty
local parsedDate = date.parse("2023-09-21 15:00:00")
print(parsedDate:fmt("%Y-%m-%d %H:%M:%S"))  -- Wynik: 2023-09-21 15:00:00

-- Dodawanie okresu czasu
local oneWeekLater = parsedDate:adddays(7)
print(oneWeekLater:fmt("%Y-%m-%d %H:%M:%S"))  -- Wynik: 2023-09-28 15:00:00
```

Biblioteka `luadate` oferuje bardziej intuicyjny i potężny sposób pracy z datami, w tym parsowanie z ciągów znaków, formatowanie i operacje arytmetyczne na datach, co znacząco upraszcza pracę z danymi czasowymi w Lua.
