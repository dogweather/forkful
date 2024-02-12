---
title:                "Een string omzetten naar kleine letters"
aliases:
- /nl/lua/converting-a-string-to-lower-case/
date:                  2024-01-28T21:57:51.672263-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string omzetten naar kleine letters"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/lua/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een string omzetten naar kleine letters betekent het wisselen van alle hoofdletters in de string naar hun kleine letter tegenhangers. Programmeurs doen dit voor consistentie, vooral bij het vergelijken of verwerken van tekstgegevens waarbij hoofdletters er niet toe zouden moeten doen, zoals gebruikersinvoer of zoekopdrachten.

## Hoe te:
In Lua verlicht je je last met `string.lower()`. Voer het een string in, en er komt een kleine letter versie uit. Kijk maar:

```lua
local originalString = "Hello, World!"
local lowerCaseString = string.lower(originalString)
print(lowerCaseString)  -- Uitvoer: hello, world!
```

Voer dit fragment uit. De schreeuwende hoofdletters zijn nu fluister-niveau kleine letters.

## Diepgaand
Sinds de dageraad van de informatica, hebben mensen om allerlei redenen tekst in een uniforme vorm moeten drukken, zoals sorteren of hoofdletterongevoelige logins. In Lua is `string.lower()` sinds het begin de go-to geweest. Het is netjes, het is ingebouwd, en het doet zijn werk zonder gedoe.

Maar wat zit er onder de motorkap? `string.lower()` gaat door elk karakter heen, en als het een hoofdletter is (A t/m Z), dan converteert het die. Lua vertrouwt op de ASCII-waarden: 'A' (65) tot 'Z' (90) worden verhoogd naar 'a' (97) tot 'z' (122). Het verschil? 32. Dus, `kleineLetter = hoofdLetter + 32`.

Wat als `string.lower()` te mainstream aanvoelt? Je zou handmatig door karakters kunnen ploegen met een lus, gebruikmakend van ASCII-waarden, of patroonmatching met `string.gsub()`:

```lua
local s = "Make Me Lowercase, Please"
s = s:gsub("%u", function (upper) return string.char(upper:byte() + 32) end)
print(s)  -- Uitvoer: make me lowercase, please
```

Maar echt, waarom zou je roeien als je een buitenboordmotor hebt (lees: `string.lower()`)?

## Zie Ook
Verdiep je verder in de stringmanipulatie van Lua met deze lekkernijen:
- [Programming in Lua (4e editie)](https://www.lua.org/pil/contents.html) voor de ins, outs en tussenin van strings.
- [Lua 5.4 Referentiehandleiding](https://www.lua.org/manual/5.4/manual.html#6.4) voor alle stringfuncties wanneer je klaar bent om verder te gaan dan kleinere letters.
