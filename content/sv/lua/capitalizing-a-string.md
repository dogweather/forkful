---
title:                "Att göra en sträng versal"
date:                  2024-01-19
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att omvandla den första bokstaven i varje ord till versal, medan resten av bokstäverna blir gemener. Programmerare gör detta för att standardisera textdata, som namn eller titlar, vilket förbättrar läsbarheten och konsistensen.

## Hur gör man:
```Lua
function capitalize(str)
    return (str:gsub("(%a)([%w_']*)", function(first, rest) return first:upper()..rest:lower() end))
end

print(capitalize("hej världen")) -- Output: Hej Världen
print(capitalize("lua är kul"))  -- Output: Lua Är Kul
```

## Djupdykning
Kapitalisering av strängar är inte ett nytt koncept utan har använts i textbehandling länge. I Lua, som inte har inbyggda funktioner för kapitalisering, får man skapa egna lösningar. Alternativen varierar från enkla första-bokstavsomvandlingar till komplexa funktioner som hanterar undantag och lokaliseringsregler. Ovanstående implementering använder Lua:s mönstermatchningsfunktioner för att hitta ord och omvandla tecken, men kan behöva anpassas beroende på språkspecifika regler, som svenska bokstäver.

## Se även
- Lua Users Wiki om mönstermatchning: http://lua-users.org/wiki/PatternsTutorial
- Lua 5.4 referensmanual: https://www.lua.org/manual/5.4/manual.html
- Onlineresurser om textbehandling och algoritmer: https://rosettacode.org/wiki/String_case#Lua
