---
title:                "Utskrift av felaviseringar"
html_title:           "Lua: Utskrift av felaviseringar"
simple_title:         "Utskrift av felaviseringar"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva ut debug output är en vanlig teknik som används av programmerare för att få en bättre förståelse för hur deras kod fungerar. Genom att skriva ut information om variabler, värden och steg i koden kan programmerare hitta fel och problem snabbare och enklare.

## Så här gör du:
Här är ett exempel på hur man kan skriva ut en variabel i Lua:
```lua
local num = 42
print(num)
```
Outputen av detta kodblock kommer att vara:
```
42
```
Det är enkelt och snabbt att skriva ut variabler på detta sätt, vilket gör det till ett användbart verktyg för att felsöka och förstå kodens funktion.

## Djupdykning:
Att skriva ut debug output är en teknik som har funnits länge inom programmering, och det förekommer i många olika språk. I Lua är det vanligt att använda funktionen `print()` för att skriva ut information, men det finns också alternativ som t.ex. `fprintf()` och `printf()`. Det är viktigt att komma ihåg att debug output bör tas bort eller kommenteras bort när koden är klar, eftersom det annars kan påverka prestandan i en färdig produkt.

En annan aspekt av att skriva ut debug output är formateringen av outputen. Genom att använda speciella formatteringssträngar, som t.ex. `%d` för att skriva ut heltal och `%s` för att skriva ut strängar, kan man skapa en mer läsbar och organiserad output.

## Se även:
- Officiell Lua dokumentation (på engelska): https://www.lua.org/manual/5.3/manual.html#6.1
- En guide för att använda debug output i Lua: https://www.tutorialspoint.com/lua/lua_debugging.htm
- En video som förklarar hur man använder `print()` i Lua: https://www.youtube.com/watch?v=YKw5kBAkZRE