---
date: 2024-01-20 17:37:24.127698-07:00
description: "How to: (Jak to zrobi\u0107:) Konwersja daty na ci\u0105g znak\xF3w\
  \ w Lua jest prosta dzi\u0119ki wbudowanemu modu\u0142owi `os`. Historia tej funkcjonalno\u015B\
  ci si\u0119ga pocz\u0105tk\xF3w\u2026"
lastmod: '2024-04-05T22:50:49.874121-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Konwersja daty na ci\u0105g znak\xF3w w Lua jest prosta\
  \ dzi\u0119ki wbudowanemu modu\u0142owi `os`."
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

## How to: (Jak to zrobić:)
```Lua
-- Załaduj moduł do obsługi czasu
local date = os.date

-- Aktualna data i czas jako ciąg znaków
local currentDate = date("%Y-%m-%d %H:%M:%S")
print(currentDate) -- Output: 2023-04-12 15:45:30 (Przykładowa data i czas)

-- Tylko data, bez czasu
local justDate = date("%Y-%m-%d")
print(justDate) -- Output: 2023-04-12

-- Tylko czas, bez daty
local justTime = date("%H:%M:%S")
print(justTime) -- Output: 15:45:30
```

## Deep Dive (Głębszy wgląd):
Konwersja daty na ciąg znaków w Lua jest prosta dzięki wbudowanemu modułowi `os`. Historia tej funkcjonalności sięga początków języka w latach 90. Istnieją alternatywy jak `os.date("*t")`, który zwraca tabelę z poszczególnymi elementami daty. Wymaga to większej ilości kodowania do ponownego sformatowania danych, ale daje więcej elastyczności. 

Lua nie posiada zbyt bogatej standardowej biblioteki, dlatego os.date to jedna z nielicznych wbudowanych opcji do manipulacji datami. Warto jednak wiedzieć, że string powstały przez `os.date` może mieć różny format w zależności od systemu operacyjnego. Dlatego też, w projektach wymagających portowalności międzyplatformowej, dobrze jest korzystać z bibliotek zewnętrznych, np. `luadate`.

## See Also (Zobacz też):
- Lua Manual dla `os.date`: https://www.lua.org/manual/5.4/manual.html#6.9
- Projekt `luadate` dla zaawansowanej manipulacji datami: https://github.com/Tieske/date
- Formatowanie dat w różnych językach programowania w porównaniu z Lua: https://rosettacode.org/wiki/Date_format
