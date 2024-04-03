---
date: 2024-01-20 17:37:13.808884-07:00
description: "Konvertering av en dato til en streng betyr \xE5 forvandle datoomr\xE5\
  det til tekstformat. Programmere gj\xF8r dette for \xE5 gj\xF8re datoene lesbare\
  \ for mennesker\u2026"
lastmod: '2024-03-13T22:44:40.941668-06:00'
model: gpt-4-1106-preview
summary: "Konvertering av en dato til en streng betyr \xE5 forvandle datoomr\xE5det\
  \ til tekstformat."
title: Konvertere en dato til en streng
weight: 28
---

## Hva & Hvorfor?
Konvertering av en dato til en streng betyr å forvandle datoområdet til tekstformat. Programmere gjør dette for å gjøre datoene lesbare for mennesker eller for å formatere dem for lagring og sammenligning.

## Hvordan:
```Lua
os.setlocale('nb_NO') -- Setter norsk lokalisering
local nå = os.date("*t") -- Henter nåværende dato og tid som en tabell

-- Enkel dato til streng konvertering
local datoStreng = os.date("%d.%m.%Y") -- dd.mm.yyyy format
print(datoStreng) -- Output: 'dd.mm.yyyy'

-- Tid og dato til streng
local tidDatoStreng = os.date("%X %x") -- Standard tid og dato format
print(tidDatoStreng) -- Output: 'HH:MM:SS dd/mm/yyyy'

-- Tilpasset format
local tilpassetStreng = os.date("%B %d, %Y %H:%M:%S")
print(tilpassetStreng) -- Output: 'Måned dd, yyyy HH:MM:SS'
```

## Dypdykk
Historisk sett var håndtering av datoer og tider utfordrende på grunn av forskjellige tidssoner og formater. Lua bruker 'os.date'-funksjonen som bygger på C standardbibliotekets 'strftime'-funksjon for å formatere datoer og tider. Det finnes alternative biblioteker som 'LuaDate' for mer avansert dato-håndtering. Når du konverterer datoer til strenger, er en av hovedhensynene hvordan du formaterer dem for spesifikke brukstilfeller, for eksempel sammenligning eller visning.

## Se Også
- Lua Users Wiki: http://lua-users.org/wiki/OsLibraryTutorial
- 'strftime' C funksjon manual: https://www.cplusplus.com/reference/ctime/strftime/
- LuaDate bibliotek på GitHub: https://github.com/Tieske/date
