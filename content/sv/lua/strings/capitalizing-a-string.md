---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:58.912153-07:00
description: "Hur man g\xF6r: Lua har ingen inbyggd funktion f\xF6r att g\xF6ra bokst\xE4\
  ver till versaler, men du kan enkelt genomf\xF6ra denna uppgift med hj\xE4lp av\
  \ grundl\xE4ggande\u2026"
lastmod: '2024-03-13T22:44:38.021715-06:00'
model: gpt-4-0125-preview
summary: "Lua har ingen inbyggd funktion f\xF6r att g\xF6ra bokst\xE4ver till versaler,\
  \ men du kan enkelt genomf\xF6ra denna uppgift med hj\xE4lp av grundl\xE4ggande\
  \ str\xE4ngmanipuleringsfunktioner."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Hur man gör:
Lua har ingen inbyggd funktion för att göra bokstäver till versaler, men du kan enkelt genomföra denna uppgift med hjälp av grundläggande strängmanipuleringsfunktioner. Här är en enkel funktion för att göra första bokstaven i ett enskilt ord till versal:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- Utdatat: Hello
```

För att göra varje ord i en mening med versal, kan du dela upp meningen i ord, göra varje ord med versal, och sedan sammanfoga dem igen:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- Utdatat: Hello World From Lua
```

Om du arbetar med ett projekt där prestanda är avgörande och du finner dig själv i behov av mer avancerade strängmanipuleringsmöjligheter, överväg att använda ett tredjepartsbibliotek som `Penlight`. Penlight förbättrar Lua med mer mångsidiga funktioner för stränghantering, bland andra verktyg:

```lua
-- Antag att Penlight är installerat:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- Utdatat: Hello lua users

-- Notera: Penlights capitalized-funktion gör bara den första bokstaven till versal.
-- För att göra varje ord med versal skulle du fortfarande behöva implementera en egen lösning eller utforska andra bibliotek.
```
