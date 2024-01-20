---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng innebär att bestämma antalet tecken i den. Programmerare gör detta för att navigera eller manipulera information inom strängar och för att kontrollera dataintegritet.

## Så här gör du:
För att hitta längden på en sträng i Lua, använder du funktionen `string.len()`. Eller än kortare kan du använda operatorn `#`.

```Lua
local str = "tecken"
print(string.len(str))  -- Output: 6
print(#str)  -- Output: 6
```
Deep Dive
Längdfunktionen är en grundläggande del av strängmanipulation, där sedan starten av programmeringsspråket i 1993. I Lua kan du använda två metoder att få stränglängden, `string.len()` metoden och `#` operatorn.

Det finns inga direkta alternativ till att hitta längden på en sträng bortsett från ovanstående två metoder. Dock kan man manuellt utföra operationen genom att iterera över strängen, men det är ineffektivt och onödigt i Lua.

Lua lagrar intern den stränglängden för omedelbar åtkomst, vilket gör ovanstående metoder snabba och effektiva. Obs! `#` operatorn kan ge inkorrekt resultat om strängen innehåller nolla-byte.

## Se även:
För mer information om strängar i Lua, utforska följande länkar:

1. [Programming in Lua: Strings](https://www.lua.org/pil/2.4.html)
2. [Lua 5.3 Reference Manual: string.len](https://www.lua.org/manual/5.3/manual.html#pdf-string.len)
3. [Lua-Users Wiki: String Library Tutorial](http://lua-users.org/wiki/StringLibraryTutorial)