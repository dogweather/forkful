---
date: 2024-01-20 17:35:19.945849-07:00
description: "Hvordan: Historisk sett har sammensl\xE5ing av strenger v\xE6rt en grunnleggende\
  \ funksjon i de fleste programmeringsspr\xE5k. Lua bruker '..' (to prikker) for\
  \ \xE5\u2026"
lastmod: '2024-04-05T21:53:41.886507-06:00'
model: gpt-4-1106-preview
summary: "Historisk sett har sammensl\xE5ing av strenger v\xE6rt en grunnleggende\
  \ funksjon i de fleste programmeringsspr\xE5k."
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## Hvordan:
```Lua
-- Enkel eksempel på sammenslåing av strenger
local hilsen = "Hei, " .. "verden!"
print(hilsen)  -- Output: Hei, verden!

-- Bruk av variabler
local fornavn = "Ola"
local etternavn = "Nordmann"
local fulltNavn = fornavn .. " " .. etternavn
print(fulltNavn)  -- Output: Ola Nordmann

-- Kombinere strenger med tall (typekonvertering er nødvendig)
local alder = 30
local beskrivelse = "Alder: " .. tostring(alder)
print(beskrivelse)  -- Output: Alder: 30
```

## Dypdykk
Historisk sett har sammenslåing av strenger vært en grunnleggende funksjon i de fleste programmeringsspråk. Lua bruker '..' (to prikker) for å binde sammen strenger, noe som er litt annerledes sammenlignet med andre språk som kanskje bruker '+' eller andre operatører.

Et alternativ til '..' er `string.format`, som gir mer kontroll over formatet:
```Lua
local velkomst = string.format("Hei, %s!", "Ola")
print(velkomst)  -- Output: Hei, Ola!
```

Implementeringsdetaljer inkluderer at Lua behandler strengsammenslåing med en rekke optimaliseringer under panseret. For eksempel, når du slår sammen en lang kjede av strenger, prøver Lua å være smart om det for å redusere minnebruk og CPU-sykluser.

## Se Også
- Lua 5.4 referansemanual: https://www.lua.org/manual/5.4/
- Online Lua demo for å eksperimentere med kode: https://www.lua.org/demo.html
- Diskusjonsforum for Lua-programmerere: https://www.lua.org/lua-l.html
