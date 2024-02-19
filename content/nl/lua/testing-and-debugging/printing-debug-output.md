---
aliases:
- /nl/lua/printing-debug-output/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:33.558533-07:00
description: "Debugoutput afdrukken is informatie op je scherm gooien om te zien wat\
  \ er aan de hand is met je code. Programmeurs doen dit om gremlins in de machine\
  \ op\u2026"
lastmod: 2024-02-18 23:09:01.994197
model: gpt-4-0125-preview
summary: "Debugoutput afdrukken is informatie op je scherm gooien om te zien wat er\
  \ aan de hand is met je code. Programmeurs doen dit om gremlins in de machine op\u2026"
title: Debug-output afdrukken
---

{{< edit_this_page >}}

## Wat & Waarom?
Debugoutput afdrukken is informatie op je scherm gooien om te zien wat er aan de hand is met je code. Programmeurs doen dit om gremlins in de machine op te sporen - bugs.

## Hoe te:
Hier is de essentie van spul afdrukken in Lua:

```Lua
print("Hallo, Debug Wereld!")  -- Zet een string op de console

local number = 42
print("Het nummer is:", number)  -- Combineert strings en nummers

local table = {name = "Lua", year = 1993}
print(table)  -- Drukt de tabelreferentie af, niet super handig
```

Voorbeelduitvoer:
```
Hallo, Debug Wereld!
Het nummer is: 42
table: 0x194a330
```

Om in de tabel te duiken en zijn ingewanden te tonen, doe dit:
    
```Lua
for key, value in pairs(table) do
    print(key, "=", value)
end
```

Voorbeelduitvoer:
```
name = Lua
year = 1993
```

## Diep Duiken
Debugoutput afdrukken is niet nieuw of fancy. Het is betrouwbaar als een oude hamer. Zie je, in de oude dagen, toen fancy debuggers er nog niet waren, drukten coders af om te zien waar dingen misgingen. Lua's `print`-functie is ongecompliceerd. Het gooit dingen naar stdout - dat is meestal je terminal.

Alternatieven? Lua heeft er een hoop. Er is de zwaardere `io.write()` als je meer controle nodig hebt, zoals nieuwe regels overslaan. Modules zoals `inspect` laten de ingewanden van je tabellen beter zien dan print kan.

Implementatie van `print` is basis in Lua's C-broncode. Het gebruikt `tostring` op elk argument en gooit het naar `stdout` met een nieuwe regel. LuaJIT, een just-in-time compiler versie van Lua, gebruikt dezelfde `print` benadering, maar met riem en bretels.

## Zie Ook
Krijg het grotere plaatje:

- Lua's officiële `print` documentatie: https://www.lua.org/manual/5.4/manual.html#pdf-print
- Een introductie tot LuaJIT: http://luajit.org/intro.html
- `io` bibliotheek uiteenzetting voor het vuile werk over `io.write`: https://www.lua.org/manual/5.4/manual.html#6.8
- De `inspect.lua` module, voor als je moe bent van je tabellen die verlegen spelen: https://github.com/kikito/inspect.lua
