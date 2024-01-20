---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:15:40.270733-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Å hente dagens dato er å finne ut eksakt dato akkurat nå. Programmerere bruker dette for funksjoner som tidsstempler, dato-logging og tidsavhengige events.

## How to:
Lua-scriptet nedenfor viser hvordan du henter og viser dagens dato:

```lua
-- Henter dagens dato
local dagens_dato = os.date("*t") -- lagrer dato og tid som en tabell

-- Skriver ut dagens dato formatert som DD/MM/YYYY
print(string.format("%02d/%02d/%04d", dagens_dato.day, dagens_dato.month, dagens_dato.year))
```

Sample output:
```
31/03/2023
```

## Deep Dive
I Lua, bruker `os.date` funksjonen for å hente systemets dato og tid. Dette er en del av standardbiblioteket og krever ingen eksterne avhengigheter. Historisk sett har Lua-miljøet vektlagt en enkel, lettvekts opplevelse, og `os.date` tilbyr en grei måte å hente tid data uten å måtte installere tunge tidspakker.

Alternativer til `os.date` inkluderer biblioteker som `luadate`, men for de fleste brukstilfeller er `os.date` tilstrekkelig. Lua gir deg også `os.time` for å få tiden som et Unix-tidsstempel, noe som kan være praktisk for å beregne tidsforskjeller eller lagre tidspunkter på en standardisert format.

Detaljene i implementasjonen er ganske rett frem: `os.date` formaterer tid basert på systemklokken og kan returnere en tabell (som over), eller en formatert streng hvis du spesifiserer formatet selv. Lua justerer seg også etter tidszonen til systemet det kjører på.

## See Also
- Lua 5.4 referansemanual: https://www.lua.org/manual/5.4/
- lua-users Wiki om dato og tid: http://lua-users.org/wiki/DateTime
- GitHub-repositorium for luadate: https://github.com/Tieske/date