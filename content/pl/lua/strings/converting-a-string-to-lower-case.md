---
date: 2024-01-20 17:38:43.646081-07:00
description: "How to: (Jak to zrobi\u0107:) Zamiana tekstu na ma\u0142e litery jest\
  \ prost\u0105 ale wa\u017Cn\u0105 operacj\u0105 w programowaniu. Historia tej funkcjonalno\u015B\
  ci si\u0119ga wczesnych lat\u2026"
lastmod: '2024-04-05T22:50:49.849258-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Zamiana tekstu na ma\u0142e litery jest prost\u0105\
  \ ale wa\u017Cn\u0105 operacj\u0105 w programowaniu."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## How to: (Jak to zrobić:)
```Lua
local tekst = "Witaj Świecie!"
local tekstMaleLitery = tekst:lower()

print(tekstMaleLitery)  -- wyświetli "witaj świecie!"
```

## Deep Dive (Dogłębna analiza)
Zamiana tekstu na małe litery jest prostą ale ważną operacją w programowaniu. Historia tej funkcjonalności sięga wczesnych lat programowania, gdzie porównywano kody ASCII dużych i małych liter dla unifikacji danych. W Lua, funkcja `lower()` jest częścią standardowej biblioteki string i wykorzystuje lokalizację systemu do wykonywania operacji, co może mieć wpływ na teksty zawierające np. polskie znaki. Alternatywą jest manualne mapowanie znaków czy korzystanie z zewnętrznych bibliotek, które mogą oferować zaawansowane możliwości przy manipulacji tekstami. Implementacja w Lua jest prosta i wydajna, ale ważne jest świadome stosowanie jej z uwagi na lokalizację.

## See Also (Zobacz również)
- Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/manual.html#6.4
- Lua Users Wiki – Strings Tutorial: http://lua-users.org/wiki/StringsTutorial
- Unicode in Lua: https://www.unicode.org/notes/tn14/LuaMappings.html

*Uwaga: Linki mogą prowadzić do stron w języku angielskim.
