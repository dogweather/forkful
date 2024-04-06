---
date: 2024-01-26 03:45:57.137329-07:00
description: "Hvordan: Lua inkluderer ikke en avrundingsfunksjon rett ut av boksen,\
  \ ulikt noen andre spr\xE5k. Historisk sett trenger du \xE5 skrive din egen eller\
  \ bruke et\u2026"
lastmod: '2024-04-05T21:53:41.889597-06:00'
model: gpt-4-0125-preview
summary: "Lua inkluderer ikke en avrundingsfunksjon rett ut av boksen, ulikt noen\
  \ andre spr\xE5k."
title: Avrunding av tall
weight: 13
---

## Hvordan:
```lua
-- Grunnleggende avrunding i Lua kommer ikke innebygd, men du kan definere en funksjon:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- For å avrunde til et spesifikt desimalplass:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Dypdykk
Lua inkluderer ikke en avrundingsfunksjon rett ut av boksen, ulikt noen andre språk. Historisk sett trenger du å skrive din egen eller bruke et tredjepartsbibliotek. Vanlige løsninger avhenger av `math.floor()` for avrunding nedover og `math.ceil()` for avrunding oppover, parret med å legge til eller trekke fra 0.5 før man gjør dette, avhengig av tallets fortegn.

Alternativer til å lage din egen funksjon inkluderer biblioteker som "lua-users wiki" eller "Penlight". Hvert har sine fordeler og ulemper, som ekstra funksjonaliteter eller mer overhead.

Internt fungerer disse funksjonene vanligvis ved å utnytte måten datamaskiner lagrer flyttall på. Å legge til 0.5 på et positivt flyttall du vil avrunde vil dytte det over terskelen til den neste heltallsverdien, så når du bruker `math.floor()`, vil det runde ned til det nærmeste heltallet.

## Se Også
- [Lua 5.4 Referansehåndbok: De Matematiske Funksjonene](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Lua Biblioteker: Matematikk](https://github.com/lunarmodules/Penlight)
