---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Radering av karaktärer som matchar ett mönster i programmering innebär att man tar bort specifika element i en sträng som följer ett visst mönster. Programmerare gör detta för att rensa oönskad data eller omformat text i ett mer läsbart eller användbart format.

## Hur gör man
Exempel på hur man tar bort karaktärer som matchar ett mönster i Lua:

```lua
str = "Hej, hvordan går det? 123"
-- Tar bort alla siffror
str = str:gsub("%d", "")
print(str) --> Hej, hvordan går det?
```
`gsub` är en inbyggd funktion i Lua som används för att byta ut delar av en sträng. `%d` matchar alla siffror. Resultatet är en sträng utan några siffror.

```lua
str = "Hej, hvordan går det? 123"
-- Tar bort alla icke-alfabetiska tecken
str = str:gsub("%W", "")
print(str) --> Hejhvordangrdet
```
`%W` matchar alla icke-alfabetiska tecken. Resultatet är en sträng endast med bokstäver.

## Fördjupning
Raderingen av karaktärer som matchar ett mönster är ett vanligt mönster inom programmering och har sina rötter långt tillbaka i programmeringshistorien. Alternativt kan du använda `string.match` för att hitta mönstret och sedan `string.remove` för att ta bort det, men `gsub` är både mer snyggt och effektivt.

Viktigt att förstå här är att Lua använder "mönstermatchning" snarare än "reguljära uttryck" som i vissa andra programmeringsspråk. Lua nöjer sig med en enklare, mindre kraftig variant men som är lättare att lära och använda.

## Se även
-För mer detaljerade exempel och förklaringar på gsub-funktionen, se [Lua's String Library Tutorial](https://www.lua.org/pil/20.html)
-För ytterligare information om hantering av strängar i Lua, se [Lua-String Library](https://www.lua.org/manual/5.3/manual.html#6.4)