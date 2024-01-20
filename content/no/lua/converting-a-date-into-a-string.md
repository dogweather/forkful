---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en dato til en streng er prosessen for å endre et datofelt til en leserbar tekst. Programmerere gjør dette for å gjøre dataene mer forståelige og anvendelige for andre operasjoner, som utskrift eller lagring.

## Hvordan gjør man det:
I Lua, vi bruker `os.date` funksjonen for å konvertere en dato til en streng. Her er en kodeeksempel:

```Lua
local tid = os.date("*t") -- henter lokal tid
local strengTid = os.date("%Y-%m-%d %H:%M:%S", os.time(tid)) 
print(strengTid) -- utskrift vil være i formen: '2022-02-22 14:30:00'
```
Strengen kan formateres til varierte formater ved å endre argumentet gitt til `os.date`.

## Dypdykk
Lua, først utgitt i 1993, er en kraftig, effektiv og lett vekt-scripting språk. `os.date` er en innebygd funksjon i Lua og ble introdusert for å gi et enklere grensesnitt for tid og dato operasjoner.

Du kan også bruke andre biblioteker som `LuaDate`, som gir mer komplekse dato og tid operasjoner. Men, for enkel konvertering av datoer til strenger, `os.date` er mer enn passende.

Implementeringsdetaljer å merke seg inkluderer at `os.date` funksjonen faktisk returnerer forskjellige typer avhengig av argumentet det gis. Med `*t` gir det en tabell med tid / datoinformasjon, mens med en strengformater gir det en streng.

## Se også
For å gå dypere inn i emnet, se følgende lenker:

- Lua's innebygde funksjoner: https://www.lua.org/manual/5.4/manual.html#6.9
- LuaDate bibliotek: https://github.com/Tieske/date
- Lua programmering/Lua standardbiblioteker: https://en.wikibooks.org/wiki/Lua_Programming

Husk, øvelse gjør mester. Prøv deg på kodeeksemplene og gå gjennom kildelinkene for bedre forståelse.