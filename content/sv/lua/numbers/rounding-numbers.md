---
date: 2024-01-26 03:45:46.299743-07:00
description: "Hur man g\xF6r: Lua inkluderar inte en avrundningsfunktion direkt ur\
  \ l\xE5dan till skillnad fr\xE5n vissa andra spr\xE5k. Historiskt sett beh\xF6ver\
  \ du skriva din egen\u2026"
lastmod: '2024-04-05T21:53:39.373757-06:00'
model: gpt-4-0125-preview
summary: "Lua inkluderar inte en avrundningsfunktion direkt ur l\xE5dan till skillnad\
  \ fr\xE5n vissa andra spr\xE5k."
title: Avrundning av tal
weight: 13
---

## Hur man gör:
```lua
-- Grundläggande avrundning i Lua kommer inte inbyggd, men du kan definiera en funktion:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- För att avrunda till en specifik decimalplats:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Djupdykning
Lua inkluderar inte en avrundningsfunktion direkt ur lådan till skillnad från vissa andra språk. Historiskt sett behöver du skriva din egen eller använda ett tredjepartsbibliotek. Vanliga lösningar är beroende av `math.floor()` för nedåtrundning och `math.ceil()` för uppåtrundning, ihop med att lägga till eller dra ifrån 0.5 innan, beroende på talets tecken.

Alternativ till att rulla din egen funktion inkluderar bibliotek såsom "lua-users wiki" eller "Penlight". Var och en har sina fördelar och nackdelar, som ytterligare funktioner eller mer overhead.

Internt fungerar dessa funktioner normalt genom att utnyttja hur datorer lagrar flyttalsnummer. Att lägga till 0.5 till ett positivt flyttal som du vill avrunda kommer att skjuta det över tröskeln för det nästa heltalsvärdet, så när du tillämpar `math.floor()` avrundas det nedåt till det närmaste heltalet.

## Se även
- [Lua 5.4 Referensmanual: De matematiska funktionerna](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Lua Bibliotek: Math](https://github.com/lunarmodules/Penlight)
