---
title:                "Beräkning av ett datum i framtiden eller förflutna"
html_title:           "Lua: Beräkning av ett datum i framtiden eller förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Beräkning av ett datum i framtiden eller förflutet är konceptet att hitta ett exakt datum baserat på en viss tidsperiod från ett känt datum. Programmerare gör det för att hantera tidskänsliga uppgifter som schema beräkningar, påminnelser, avståndsberäkningar och liknande applikationer.

## Så här gör du:

I Lua, vi kan beräkna förflutna eller framtida datum med hjälp av `os.date` och `os.time` funktioner. Här är ett exempel:

```Lua
dagar_fram = 7
futur_datum = os.date("*t", os.time() + (dagar_fram * 24 * 60 * 60))
print(string.format("Framtida datum är: %s/%s/%s", futur_datum.day, futur_datum.month, futur_datum.year))
```

Provutmatning:
```Lua
Framtida datum är: 27/7/2022
```

## Djupdykning:

Historiskt sett, har beräkningarna av datum i framtiden eller förflutet inte alltid varit en lätt uppgift, särskilt med hänsyn till olika kalendersystem och tidszoner. Därför är programmeringsspråk utrustade med inbyggda funktioner eller bibliotek för att hantera dessa utmaningar.

Ett alternativ til `os.date` och `os.time` i Lua är att använda tredjepartsbibliotek som Penlight eller Date, där mer avancerade och komplexa datumoperationer är möjliga.

När det kommer till implementeringsdetaljer, fungerar `os.time` funktionen genom att returnera det aktuella datumet och tiden, uttryckt i sekunder sen Unix epoch. Genom att lägga till eller dra uttryck i sekunder till detta värde, kan vi få ett nytt datum.

## Se även

För mer information om datum och tidshantering i Lua, följande länkar kan vara till hjälp:

- [Lua 5.1 Referensmanual - os.date](https://www.lua.org/manual/5.1/manual.html#pdf-os.date)
- [Lua 5.1 Referensmanual - os.time](https://www.lua.org/manual/5.1/manual.html#pdf-os.time)
- [Penlight Documentation](https://stevedonovan.github.io/Penlight/api/index.html)
- [Date (Lua library)](https://github.com/Tieske/date)