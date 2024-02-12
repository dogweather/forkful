---
title:                "Wyszukiwanie i zamiana tekstu"
aliases:
- /pl/lua/searching-and-replacing-text.md
date:                  2024-01-20T17:58:14.774707-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wyszukiwanie i zamiana tekstu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Wyszukiwanie i zamiana tekstu to podstawy manipulacji ciągami. Programiści używają tych technik do edycji danych, automatyzacji poprawek i przetwarzania wprowadzania.

## How to: (Jak to zrobić:)
```Lua
local text = "Hello, world!"
local toFind = "world"
local toReplace = "Lua"

-- Zwykłe wyszukiwanie tekstu.
local found = string.find(text, toFind)
print(found)  -- 8

-- Prosta zamiana tekstu.
local replaced = string.gsub(text, toFind, toReplace)
print(replaced)  -- Hello, Lua!

-- Wyszukiwanie wzorców (pattern matching).
local pattern = "(%w+), (%w+)"
local new_text = string.gsub(text, pattern, "%2, %1")
print(new_text)  -- world, Hello!
```

## Deep Dive (Pogłębione spojrzenie):
Wyszukiwanie i zamiana tekstu w Lua jest niezwykle wygodne dzięki funkcjom wbudowanym w standardową bibliotekę `string`. Historia języka Lua sięga 1993 roku, gdzie manipulacja tekstem już od początku była kluczowym aspektem. W Lua można używać patternów wzorowanych na regexach (choć nieco ograniczone), które pozwalają na bardziej złożone operacje. Alternatywy takie jak biblioteki zewnętrzne oferują dodatkowe możliwości, ale często standardowe narzędzia są więcej niż wystarczające. Implementacja tych funcji jest wydajna i dobrze zoptymalizowana, ale warto pamiętać o potencjalnym zagrożeniu, jakim jest nadmierne używanie wzorców w dużych ciągach tekstu, co może prowadzić do spadku wydajności.

## See Also (Zobacz również):
- [String Patterns in Lua](https://www.lua.org/pil/20.2.html) – informacje o wzorcach w Lua.
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/) – oficjalny podręcznik referencyjny Lua.
