---
title:                "Sammanslagning av strängar"
html_title:           "Lua: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att sammanfoga strängar (concatenation) är en process där man kombinerar flera strängar till en enda, längre sträng. Det är ett vanligt verktyg för programmerare att använda när de behöver manipulera text eller skapa nya strängar baserat på befintliga. Det är ett snabbt och enkelt sätt att manipulera text på ett flexibelt sätt.

## Hur man gör det:
För att sammanfoga strängar i Lua använder man operatorn ".." och anger de strängar man vill kombinera. Se nedan för exempel på hur man gör och vilket resultat det ger:

```Lua
local str1 = "Hej "
local str2 = "på dig!"
local combined = str1 .. str2
print(combined) -- Ger "Hej på dig!"
```

Man kan även sammanfoga flera strängar samtidigt, vilket visas i exemplet nedan:

```Lua
local str1 = "Mitt "
local str2 = "namn "
local str3 = "är "
local str4 = "Lua."
local combined = str1 .. str2 .. str3 .. str4
print(combined) -- Ger "Mitt namn är Lua."
```

## Djupdykning:
Sammanfogning av strängar är en vanlig teknik som har funnits med sedan de tidiga dagarna av programmering. Innan "string interpolation" (en teknik som låter programmerare skriva variabler direkt i en sträng) fanns, var sammanfogning av strängar det enda sättet att kombinera text. Det är fortfarande ett vanligt sätt att hantera textmanipulering och är särskilt användbart när man kombinerar text med variabler och annan data.

Det finns några alternativ till strängsammanfogning, till exempel "string.format" funktionen som låter programmerare skapa en sträng från ett "format string" och sedan lägga till variabler i denna sträng. Detta kan vara mer läsbart och enklare att hantera för vissa programmerare.

För de som är intresserade av implementationen av strängsammanfogning i Lua, använder det inbyggda "*" funktionen för att skapa en ny sträng med det sammanslagna innehållet. Detta kan dock förändras mellan versioner av Lua så det är bäst att använda den inbyggda ".." operatören för att garantera portabilitet.

## Se även:
För mer information om Lua och dess funktioner och användningsområden, besök officiella dokumentationen på https://www.lua.org/docs.html. För att lära dig mer om strängmanipulation i Lua, besök https://www.lua.org/pil/20.1.html.