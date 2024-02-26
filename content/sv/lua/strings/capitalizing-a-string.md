---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:58.912153-07:00
description: "Att skriva med stor bokstav inneb\xE4r att \xE4ndra det f\xF6rsta tecknet\
  \ i varje ord i en mening till versal, samtidigt som man ser till att resten \xE4\
  r gemener.\u2026"
lastmod: '2024-02-25T18:49:36.325115-07:00'
model: gpt-4-0125-preview
summary: "Att skriva med stor bokstav inneb\xE4r att \xE4ndra det f\xF6rsta tecknet\
  \ i varje ord i en mening till versal, samtidigt som man ser till att resten \xE4\
  r gemener.\u2026"
title: "G\xF6r om en str\xE4ng till versaler"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva med stor bokstav innebär att ändra det första tecknet i varje ord i en mening till versal, samtidigt som man ser till att resten är gemener. Denna teknik används ofta för att formatera text för ett mer professionellt eller läsbart utdata, såsom att förbereda titlar eller användarinmatning för visning.

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
