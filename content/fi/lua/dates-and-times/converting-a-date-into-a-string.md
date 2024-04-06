---
date: 2024-01-20 17:37:02.532785-07:00
description: "How to: Lua-kielell\xE4 p\xE4iv\xE4m\xE4\xE4r\xE4n muunto merkkijonoksi\
  \ on yleist\xE4, kun tallennetaan logitiedostoja tai k\xE4sitell\xE4\xE4n aikaleimoja.\
  \ Os.date()-funktio on ollut\u2026"
lastmod: '2024-04-05T21:53:58.280602-06:00'
model: gpt-4-1106-preview
summary: "Lua-kielell\xE4 p\xE4iv\xE4m\xE4\xE4r\xE4n muunto merkkijonoksi on yleist\xE4\
  , kun tallennetaan logitiedostoja tai k\xE4sitell\xE4\xE4n aikaleimoja."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## How to:
```Lua
-- Oletusmuoto
local date = os.date("*t")      -- hanki tämänhetkinen paikallinen aika
local dateString = os.date()    -- muuntaa sen merkkijonoksi
print(dateString)               -- "Tue Mar 01 14:10:02 2023"

-- Määritelty muoto
dateString = os.date("%Y-%m-%d %H:%M:%S", os.time(date))
print(dateString)               -- "2023-03-01 14:10:02"
```

## Deep Dive
Lua-kielellä päivämäärän muunto merkkijonoksi on yleistä, kun tallennetaan logitiedostoja tai käsitellään aikaleimoja. Os.date()-funktio on ollut osana standardikirjastoa jo pitkään, esitellen joustavuutta muotoiluun. Vaihtoehtoina, voit käyttää muita kirjastoja kuten luadate, jos tarvitsen enemmän ominaisuuksia. Tarkat muotoilukoodit löydät Lua-dokumentaatiosta – ne vastaavat usein C-kielen strftime-funktion koodeja.

## See Also
- Lua 5.4 referenssidokumentaatio: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
- LuaDate-kirjaston kotisivu: [https://github.com/Tieske/date](https://github.com/Tieske/date)
