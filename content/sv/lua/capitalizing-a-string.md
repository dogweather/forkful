---
title:                "Gör om en sträng till versaler"
html_title:           "Lua: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Kapitalisering av en sträng i programmering innebär att förändra en bokstav från små till stora. Programmerare gör det för att förbättra läsbarheten och göra viktig text mer framträdande.

## Så här gör du:

Här är ett praktiskt exempel på att tydliggöra en sträng i Lua:

```lua
-- Lua fungerar inte inbyggt med string.capitalize, så vi skapar vår egen funktion.
function string.capitalize(s)
  return (s:gsub("^%l", string.upper))
end

print(string.capitalize("hello world"))   -- "Hello world"
```
Notera att vi skapade en funktion för att hantera kapitaliseringen eftersom Lua på egen hand inte erbjuder denna funktionalitet.

## Djupdykning:

Historiskt sett har många programmeringsspråk inbyggda funktioner för strängmanipulation, inklusive kapitalisering, men Lua är inte ett av dem. Alternativen är att skapa en funktion själv, vilket vi visade tidigare, eller att använda en extern bibliotek som Penlight.

Implementeringen av strängkapitalisering i Lua är inte svår. Funktionen vi skapade använder regex (gmatch) för att matcha det första lilla bokstaven i strängen och gör om det till stor bokstav.

## Se Mer:

För mer djupgående resurser för strängkapitalisering och strängmanipulation i Lua, kolla in följande länkar:

1. Lua String Library: https://www.lua.org/manual/5.3/manual.html#6.4
2. Penlight Lua Libraries: https://github.com/stevedonovan/Penlight
3. Lua Patterns (Regex): https://www.lua.org/pil/20.2.html