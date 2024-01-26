---
title:                "Konwersja ciągu znaków na małe litery"
date:                  2024-01-20T17:38:43.646081-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Zamiana łańcucha na małe litery oznacza, że każda duża litera w tekście zostaje przekształcona na swoją małą wersję. Programiści robią to, aby ułatwić porównywanie tekstu, gdzie wielkość liter nie ma znaczenia, jak podczas logowania czy wyszukiwania.

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
