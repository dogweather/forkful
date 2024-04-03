---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:10.456921-07:00
description: "Lua komt niet met een ingebouwde debugger, maar je kunt externe debuggers\
  \ gebruiken, zoals ZeroBrane Studio. Hier is een voorproefje van hoe je ermee zou\u2026"
lastmod: '2024-03-13T22:44:50.941948-06:00'
model: gpt-4-0125-preview
summary: Lua komt niet met een ingebouwde debugger, maar je kunt externe debuggers
  gebruiken, zoals ZeroBrane Studio.
title: Een debugger gebruiken
weight: 35
---

## Hoe:
Lua komt niet met een ingebouwde debugger, maar je kunt externe debuggers gebruiken, zoals ZeroBrane Studio. Hier is een voorproefje van hoe je ermee zou werken:

```Lua
-- Dit is een eenvoudig Lua-script met een opzettelijke fout
local function add(a, b)
    local result = a+ b -- Oeps, laten we doen alsof we 'b' vergeten te definiëren
    return result
end

print(add(10))
```

Wanneer je dit in een debugger uitvoert, zal het de uitvoering stoppen waar dingen misgaan. Je zult zoiets zien:

```
lua: example.lua:3: poging tot uitvoeren van rekenkunde op een nil-waarde (lokale 'b')
stack traceback:
	example.lua:3: in functie 'add'
	example.lua:7: in hoofdblok
	[C]: in ?
```

Je kunt breakpoints instellen, stap voor stap door je code gaan en variabelen bekijken om de bug op te sporen zonder je verstand te verliezen.

## Dieper duiken
Helaas, de eenvoud van Lua strekt zich niet uit tot debuggen. Geen zorgen, de Lua-gemeenschap staat voor je klaar. Hulpmiddelen zoals ZeroBrane Studio, LuaDec en anderen bieden debugmogelijkheden. Historisch gezien bestonden debuggers niet lang nadat de eerste programma's problemen begonnen te vertonen, waardoor ontwikkelaars de middelen kregen om hun code te repareren zonder blindelings te hoeven rommelen.

Met Lua vertrouw je vaak op externe debuggers of bouw je ze in je ontwikkelomgeving in. ZeroBrane Studio is bijvoorbeeld een IDE die volledig is geïntegreerd met een Lua-debugger. Het stelt je in staat om door code te stappen, breakpoints in te stellen en variabelen in de gaten te houden. Aan de implementatiekant gebruiken debuggers typisch hooks om breakpoints en andere debugmogelijkheden in te voegen.

Alternatieven? Zeker. Goede oude `print`-statements, liefkozend "printf-debugging" genoemd, kunnen soms het kunstje doen zonder fancy tools.

## Zie ook
Om je reis in het debuggen voort te zetten, bekijk:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Lua-gebruikers wiki over Lua Code Debuggen: http://lua-users.org/wiki/DebuggingLuaCode
- De `debug`-bibliotheekreferentie in Lua's handleiding: https://www.lua.org/manual/5.4/manual.html#6.10
