---
title:                "Hitta längden på en sträng"
aliases:
- sv/lua/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:00.845798-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng innebär att räkna antalet tecken den innehåller. Programmerare gör det för att validera indata, navigera genom text, eller manipulera strängar effektivt.

## Hur gör man:
För att fånga längden på en sträng i Lua, använd `#` operatorn. Kolla här:

```Lua
local str = "Hej, Sverige!"
print(#str)  -- Output: 13
```

Enkelt, inte sant? Notera att `#` ger rätt resultat även med Unicode-tecken:

```Lua
local unicodeStr = "Åäö"
print(#unicodeStr)  -- Output kan variera beroende på din Lua-version och system
```

Får du konstiga resultat? Oroa dig inte, vi dyker in i detaljerna i nästa del.

## Deep Dive
I historien har Lua-användare använt `string.len()` funktionen, men `#` operatören är den moderna, smidiga vägen. 

```Lua
local str = "Äventyr"
print(string.len(str))  -- Samma som #str
```

Men, en varning: `#` hanterar Unicode-tecken olika beroende på Lua-version och systemets strängimplementering. Lua 5.3 och senare hanterar det bättre genom UTF-8-aware funktioner som `utf8.len()`:

```Lua
local str = "Räksmörgås"
print(utf8.len(str))   -- Output: 11, om din Lua-version stödjer det
```

Detta är viktigt när du arbetar med flerspråkiga eller internationella applikationer – t.ex. när en användare skriver på svenska och du vill räkna antalet bokstäver korrekt.

## Se även:
- Lua 5.4 referensmanual (på engelska): [www.lua.org/manual/5.4/](http://www.lua.org/manual/5.4/)
- 'Programming in Lua' (på engelska): [www.lua.org/pil/](http://www.lua.org/pil/) – En bra bok för Djupdykning i Lua.
- Utf8 i Lua (på engelska): [www.lua.org/manual/5.3/manual.html#6.5](http://www.lua.org/manual/5.3/manual.html#6.5) – Läs mer om UTF-8 funktioner.
