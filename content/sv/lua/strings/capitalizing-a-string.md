---
title:                "Gör om en sträng till versaler"
aliases: - /sv/lua/capitalizing-a-string.md
date:                  2024-02-03T19:05:58.912153-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
