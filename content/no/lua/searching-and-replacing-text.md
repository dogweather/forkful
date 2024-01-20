---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å søke og erstatte tekst er en måte å finne spesifikt innhold i en string, og deretter endre det for noe annet. Programmerere gjør dette for å manipulere, organisere og oppdatere data effektivt.

## Hvordan:
Her er noen enkle eksempler på bruk av `gsub` funksjonen i Lua for å søke og erstatte tekst. 

```Lua
local str = "Hei, verden!"
local newstr, n = string.gsub(str, "verden", "Lua")
print(newstr)  -- Output: Hei, Lua!
```
Denne koden søker etter ordet "verden" i strengstr og erstatter det med "Lua". Output vil være "Hei, Lua!"


## Dypdykk
Lua's `gsub` funksjon stammer fra tidligere programmeringsspråk som Perl og C, hvor ideen om å søke og erstatte tekst først ble implementert. Til tross for sin enkelhet, er det en rekke alternativer for å oppnå lignende funksjoner i Lua, som `match` og `find`.

Selv om `gsub` er rett frem, er dens interne implementasjon avansert. Den bruker en teknikk kalt "backtracking" for å finne alle forekomster av søkestrengen, noe som gjør den svært effektiv, selv for store strenger.

## Se også
For mer informasjon, se følgende ressurser:

1. Lua 5.3 referansehåndbok: String manipulasjon (https://www.lua.org/manual/5.3/manual.html#6.4)
2. Lua-Guide: Å jobbe med strenger i Lua (https://www.tutorialspoint.com/lua/lua_strings.htm)
3. StackOverflow: Vanlige spørsmål om Lua strengmanipulasjon (https://stackoverflow.com/questions/tagged/lua+string)