---
date: 2024-01-20 17:51:34.331300-07:00
description: "Hur g\xF6r man: F\xF6re `string.format`, skedde konkatenering av str\xE4\
  ngar och variabler manuellt, exempelvis: `local h\xE4lsning = \"Hej, \" .. anv\xE4\
  ndarnamn .. \"! Du\u2026"
lastmod: '2024-04-05T22:50:52.327632-06:00'
model: gpt-4-1106-preview
summary: "F\xF6re `string.format`, skedde konkatenering av str\xE4ngar och variabler\
  \ manuellt, exempelvis."
title: "Interpolera en str\xE4ng"
weight: 8
---

## Hur gör man:
```Lua
local användarnamn = "Erik"
local ålder = 29
-- Använd string.format för interpolering
local hälsning = string.format("Hej, %s! Du är %d år gammal.", användarnamn, ålder)
print(hälsning)
```
Output:
```
Hej, Erik! Du är 29 år gammal.
```
Alternativt:
```Lua
-- Med tabeller och uppslagning
local användare = {namn = "Erik", ålder = 29}
local hälsning = ("Hej, %s! Du är %d år gammal."):format(användare.namn, användare.ålder)
print(hälsning)
```
Output är densamma som ovan.

## Deep Dive
Före `string.format`, skedde konkatenering av strängar och variabler manuellt, exempelvis: `local hälsning = "Hej, " .. användarnamn .. "! Du är " .. ålder .. " år gammal."` Vilket var både bökigt och svårläst. `string.format` erbjuder en ren och lättläst syntax som är särskilt användbar för komplexa eller långa strängar.

Lua använder format specifierare (såsom `%s` för strängar, `%d` för decimaltal) inuti strängen som ersätts av tillhörande värden. Detta koncept är inte unikt för Lua – det är inspirerat av C:s `printf` funktion.

Som alternativ kan man använda tabeller. I Lua kan tabellfält ha nästan vilken nyckeltyp som helst, men string keys är vanligast för detta syfte. Med en vältänkt struktur kan interpolering och datahantering förenklas, vilket kan vara kraftfullt i större program.

## Se även:
- Lua's officiella dokumentation om strängar: [https://www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4)
- En guide till `string.format`: [https://www.lua.org/pil/20.2.html](https://www.lua.org/pil/20.2.html)
