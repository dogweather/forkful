---
title:                "Generera slumpmässiga nummer"
html_title:           "Lua: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Generering av slumpmässiga nummer är en viktig del av programmering. Det är processen med att skapa nummer som verkar slumpmässiga och används ofta för spel, simuleringar och kryptering. Programmerare använder slumpmässiga nummer för att skapa variation och osäkerhet i sina program och för att utföra uppgifter som kräver slumpmässighet.

## Hur man:
Lua har inbyggda funktioner som gör det enkelt att generera slumpmässiga nummer. Här är två exempel på hur du kan använda dessa funktioner:

```
-- Generera ett slumpmässigt heltal mellan 1 och 10
local nummer = math.random(1,10)
print(nummer)
-- Output: Slumpmässigt tal mellan 1 och 10, t.ex. 7

-- Generera ett slumpmässigt decimaltal mellan 0 och 1
local decimaltal = math.random()
print(decimaltal)
-- Output: Slumpmässigt decimaltal mellan 0 och 1, t.ex. 0.492624338

```

## Djupdykning:
Generering av slumpmässiga nummer har funnits i många år och har utvecklats mycket. Tidigare använde programmerare fysiska enheter, som tärningar eller kort, för att skapa slumpmässiga nummer. Idag använder vi datorer och algoritmer för att skapa dem, vilket är mycket snabbare och mer pålitligt.

Det finns också olika alternativ för att generera slumpmässiga nummer i Lua. En mer avancerad metod är att använda Pseudoslumpgeneratorer, som använder en algoritm för att skapa nummer som verkar slumpmässiga. Du kan också ställa in en "frö"-värde för dessa algoritmer för att skapa olika sekvenser av slumpmässiga nummer.

Identitetsmatriser är också ett alternativ och används ofta inom datavetenskap för att skapa slumpmässiga matriser. I Lua kan du använda funktionen ```math.randomseed()``` för att generera identitetsmatriser.

## Se även:
- [Lua dokumentation om slumpmässiga nummer](https://www.lua.org/pil/20.2.html)
- [Wikipedia artikel om RNG](https://sv.wikipedia.org/wiki/Slumpgenerator)