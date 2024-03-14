---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:00.308660-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta merkitsee\
  \ tekstiesitysten muuntamista p\xE4iv\xE4m\xE4\xE4rist\xE4 ja ajoista sellaiseen\
  \ formaattiin, jota on helppo k\xE4sitell\xE4,\u2026"
lastmod: '2024-03-13T22:44:56.706232-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sent\xE4minen merkkijonosta merkitsee tekstiesitysten\
  \ muuntamista p\xE4iv\xE4m\xE4\xE4rist\xE4 ja ajoista sellaiseen formaattiin, jota\
  \ on helppo k\xE4sitell\xE4,\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Päivämäärän jäsentäminen merkkijonosta merkitsee tekstiesitysten muuntamista päivämääristä ja ajoista sellaiseen formaattiin, jota on helppo käsitellä, tallentaa tai verrata Lua-ohjelmassa. Ohjelmoijat suorittavat tämän tehtävän helpottaakseen toimintoja, kuten aikataulutusta, lokitusta tai mitä tahansa aikaa koskevia laskelmia, ja siltana ihmisen luettavien päivämääräformaatien ja tietokoneen tehokkaasti käsittelemien rakenteellisten datatyyppien välillä.

## Kuinka:
Lua ei tarjoa sisäänrakennettua tukea päivämäärän ja ajan käsittelyyn sen rajatun toiminnallisuuden yli, jota `os.date` ja `os.time` funktiot tarjoavat. Kuitenkin näitä voidaan hyödyntää perusjäsennykseen, ja monimutkaisempiin vaatimuksiin voidaan käyttää `luadate`-kirjastoa, joka on ulkoinen kirjasto.

**Käyttäen `os.date` ja `os.time`:**
```lua
-- Muunnetaan ihmisen luettava päivämäärä aikaleimaksi ja takaisin
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local vuosi, kuukausi, päivä, tunti, minuutti, sekunti = dateString:match(pattern)

local aikaleima = os.time({
  year = vuosi,
  month = kuukausi,
  day = päivä,
  hour = tunti,
  min = minuutti,
  sec = sekunti
})

-- Muunnetaan aikaleima takaisin ihmisen luettavaan formaattiin
local muotoiltuPäivämäärä = os.date("%Y-%m-%d %H:%M:%S", aikaleima)
print(muotoiltuPäivämäärä)  -- Tuloste: 2023-09-21 15:00:00
```

**Käyttäen `luadate` (kolmannen osapuolen kirjasto):**
Kun käytät `luadate`, varmista että se on asennettu LuaRocksilla tai valitsemallasi paketinhallintajärjestelmällä. `luadate` lisää kattavat päivämäärän ja ajan jäsentämisen ja käsittelyn kyvyt.

```lua
local date = require('date')

-- Jäsennetään päivämäärämerkkijono suoraan
local parsedDate = date.parse("2023-09-21 15:00:00")
print(parsedDate:fmt("%Y-%m-%d %H:%M:%S"))  -- Tuloste: 2023-09-21 15:00:00

-- Lisätään kestoja
local viikkoMyöhemmin = parsedDate:adddays(7)
print(viikkoMyöhemmin:fmt("%Y-%m-%d %H:%M:%S"))  -- Tuloste: 2023-09-28 15:00:00
```

`luadate`-kirjasto tarjoaa intuitiivisemman ja tehokkaamman tavan työskennellä päivämäärien kanssa, mukaan lukien jäsentäminen merkkijonoista, muotoilu ja aritmeettiset operaatiot päivämäärien yli, mikä huomattavasti yksinkertaistaa työskentelyä aikatietojen kanssa Luassa.
