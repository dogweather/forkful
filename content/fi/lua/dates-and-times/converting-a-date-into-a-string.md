---
date: 2024-01-20 17:37:02.532785-07:00
description: "Muuntaminen p\xE4iv\xE4m\xE4\xE4r\xE4st\xE4 merkkijonoksi tarkoittaa\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4-arvon esitt\xE4mist\xE4 tekstimuodossa. Ohjelmoijat\
  \ tekev\xE4t t\xE4m\xE4n esitt\xE4\xE4kseen p\xE4iv\xE4m\xE4\xE4r\xE4n\u2026"
lastmod: '2024-03-13T22:44:56.708388-06:00'
model: gpt-4-1106-preview
summary: "Muuntaminen p\xE4iv\xE4m\xE4\xE4r\xE4st\xE4 merkkijonoksi tarkoittaa p\xE4\
  iv\xE4m\xE4\xE4r\xE4-arvon esitt\xE4mist\xE4 tekstimuodossa. Ohjelmoijat tekev\xE4\
  t t\xE4m\xE4n esitt\xE4\xE4kseen p\xE4iv\xE4m\xE4\xE4r\xE4n\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi"
weight: 28
---

## What & Why?
Muuntaminen päivämäärästä merkkijonoksi tarkoittaa päivämäärä-arvon esittämistä tekstimuodossa. Ohjelmoijat tekevät tämän esittääkseen päivämäärän ymmärrettävästi ihmisille tai siirtääkseen dataa formaateissa, jotka vaativat tekstiesitystä.

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
