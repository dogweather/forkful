---
title:                "Läsning av kommandoradsargument"
html_title:           "Lua: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa in kommandoradsargument är ett sätt för en Lua-programmerare att interagera med sitt program från kommandoraden. Det kan användas för att ge input till programmet eller för att lägga till extra funktionalitet baserat på det som skrivs in av användaren.

## Hur gör man:

I Lua finns det många inbyggda metoder för att hantera kommandoradsargument. Det första steget är att importera biblioteket `arg` i ditt program. Sedan kan du använda följande metoder:

* `arg[0]` - returnerar namnet på det nuvarande programmet som körs
* `arg[1]` - returnerar första argumentet som skrivs in från kommandoraden
* `arg.n` - returnerar antalet argument som skrivs in från kommandoraden
* `arg[<index>]` - returnerar ett specifikt argument baserat på dess position i kommandoraden

Ett exempel på hur du kan använda dessa metoder är:

```Lua
print("Hej " .. arg[1] .. "!")
```

Om du kör detta program från kommandoraden med argumentet "Svensk" kommer det att skriva ut "Hej Svensk!". 

## Djupdykning:
I äldre versioner av Lua var `arg` ett bord som innehöll alla argument som gavs till programmet. Men detta ansågs vara ineffektivt och därför ändrades det till en global variabel som kan nås från vilken funktion som helst. Det finns också alternativa sätt att hantera kommandoradsargument, som att använda biblioteket `optparse` som ger mer avancerade möjligheter för hantering av argument.

Det är också möjligt att skicka med kommandoradsargument till din Lua-kod från andra program eller skript. Till exempel kan du skicka med argument till ditt Lua-program när du kör det från ett shell-script eller från en annan programvara.

## Se även:
* [Lua 5.4 - Kommandoradsargument](https://www.lua.org/manual/5.4/manual.html#6.9)
* [Optparse biblioteket för Lua](https://github.com/luaposix/luaposix/blob/master/src/optparse.lua)