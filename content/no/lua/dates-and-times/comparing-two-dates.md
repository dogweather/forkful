---
date: 2024-01-20 17:33:34.028017-07:00
description: "Hvordan: Lua behandler datoer og tider gjennom `os` biblioteket, hvor\
  \ `os.time()` funksjonen er sentral. Historisk har tidsregning og sammenligning\u2026"
lastmod: '2024-04-05T22:50:54.945055-06:00'
model: gpt-4-1106-preview
summary: Lua behandler datoer og tider gjennom `os` biblioteket, hvor `os.time()`
  funksjonen er sentral.
title: Sammenlikning av to datoer
weight: 27
---

## Hvordan:
```Lua
os.date("*t") -- Henter dagens dato som en tabell
os.time(tabell) -- Konverterer en dato-tabell til et tidsstempel

-- Eksempel: Sammenlign to datoer
local dato1 = os.time({year=2023, month=3, day=25})
local dato2 = os.time({year=2023, month=4, day=5})

if dato1 < dato2 then
    print("Dato1 kommer før Dato2.")
elseif dato1 > dato2 then
    print("Dato1 kommer etter Dato2.")
else
    print("Datoene er like.")
end
```

Sample output:
```
Dato1 kommer før Dato2.
```

## Dypdykk
Lua behandler datoer og tider gjennom `os` biblioteket, hvor `os.time()` funksjonen er sentral. Historisk har tidsregning og sammenligning utfordret programmerere, spesielt med håndtering av tidssoner og skuddår. Lua forenkler denne prosessen ved å tilby en standard UTC tid. Til alternativer, kan man bruke tredjepartsbiblioteker som `luadate` som gir mer fleksibilitet. Implementasjonsdetaljer inkluderer håndtering av `time_t` verdien som representerer sekunder siden Unix-tiden (1. januar 1970). Denne verdi brukes over hele verden for å sikre konsistens.

## Se Også
- [Lua 5.4 Reference Manual (os library)](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Programming in Lua (date and time)](https://www.lua.org/pil/22.1.html)
- [LuaDate: Date and Time library for Lua](https://github.com/Tieske/date)
