---
title:                "Hente den nåværende datoen"
html_title:           "Lua: Hente den nåværende datoen"
simple_title:         "Hente den nåværende datoen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å få den nåværende datoen er en metode for å få informasjon om dagens dato, inkludert år, måned og dag. Dette er nyttig for programmerere som trenger å holde styr på tidsbaserte hendelser eller for å generere dynamisk innhold basert på datoen.

## Slik gjør du det:
```Lua
-- Bruk funksjonen os.date() for å få den nåværende datoen
local dato = os.date()

-- Du kan også spesifisere et eget format for datoen med hjelp av strenger og tegn
local formatert_dato = os.date("%d.%m.%Y")

-- Skriver ut den nåværende datoen og det spesifiserte formatet
print(dato) --> 01/03/2021
print(formatert_dato) --> 01.03.2021
```

## Dypdykk:
Funksjonen os.date() er tilgjengelig i mange programmeringsspråk og er basert på standarden ANSI C. Dette gjør den enkel å forstå og implementere, selv om det finnes alternativer som Moment.js eller Luxon for mer avansert dato-behandling. Det er også mulig å spesifisere lokale tidssoner og formater i os.date() for å tilpasse datoen til dine behov.

## Se også:
- [Lua dokumentasjon for os.date()](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Moment.js](https://momentjs.com/)
- [Luxon](https://moment.github.io/luxon/)