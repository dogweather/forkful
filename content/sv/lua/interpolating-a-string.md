---
title:                "Att interpolera en sträng"
html_title:           "Lua: Att interpolera en sträng"
simple_title:         "Att interpolera en sträng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Interpolering av strängar är när man sätter samman flera delar av en sträng för att skapa en ny sträng. Programerare använder sig av detta för att göra textbehandling mer effektivt och för att skapa dynamiska textsträngar som kan ändras beroende på olika variabler.

## Hur man gör:

Det finns flera sätt att interpolera strängar i Lua, men det enklaste sättet är att använda operatorn "%". Här är ett exempel:

```Lua
local name = "Sofia"
local age = 25

local greeting = "Hej, mitt namn är %s och jag är %d år gammal."
print(greeting:format(name, age))
```
Output: "Hej, mitt namn är Sofia och jag är 25 år gammal."

I exemplet ovan används operatorn "%s" för att byta ut variabeln "name" och "%d" för att byta ut variabeln "age". Det är viktigt att strängens positionering av variablerna matchar med ordningen de skrivs in i funktionen "format()".

## Djupdykning:

Interpolerade strängar har funnits i programmeringsspråk sedan 1960-talet. Ett vanligt alternativ till att använda operatorn "%" är att använda funktionen "gsub()" som låter dig byta ut specifika delar av en sträng. Men interpolering med operatorn "%" anses vara en mer läsbar och effektiv metod.

Implementeringen av operatorn "%" för att interpolera strängar är baserad på språket C. Detta gör det enkelt att använda för programmerare som är bekanta med C-språket.

## Se också:

[Lua operators](https://www.lua.org/pil/3.3.html)

[Lua string library](https://www.lua.org/manual/5.3/manual.html#6.4.1)