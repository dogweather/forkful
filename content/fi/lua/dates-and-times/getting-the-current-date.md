---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:15.049822-07:00
description: "Kuinka: Lua tarjoaa `os.date` funktion nykyisen p\xE4iv\xE4m\xE4\xE4\
  r\xE4n ja ajan hakemiseen. Funktion voi k\xE4ytt\xE4\xE4 ilman argumentteja saadakseen\
  \ muotoillun merkkijonon\u2026"
lastmod: '2024-03-13T22:44:56.707341-06:00'
model: gpt-4-0125-preview
summary: "Lua tarjoaa `os.date` funktion nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n ja ajan\
  \ hakemiseen."
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
weight: 29
---

## Kuinka:
Lua tarjoaa `os.date` funktion nykyisen päivämäärän ja ajan hakemiseen. Funktion voi käyttää ilman argumentteja saadakseen muotoillun merkkijonon tai muotoiluspesifikaattoreiden kanssa tuloksen mukauttamiseksi. Näin sitä käytetään:

```lua
-- Nykyisen päivämäärän ja ajan hakeminen muotoiltuna merkkijonona
print(os.date())  -- esim., Thu Mar  3 14:02:03 2022

-- Tulosteen muotoilun mukauttaminen
-- %Y vuodelle, %m kuukaudelle, %d päivälle, %H tunnille, %M minuuteille
print(os.date("%Y-%m-%d %H:%M"))  -- esim., 2022-03-03 14:02
```

Monimutkaisempia päivämäärä- ja aikamanipulointeja varten Lualla ei ole yhtä kattavia sisäänrakennettuja kirjastoja kuin joissakin muissa ohjelmointikielissä. Voit kuitenkin käyttää kolmansien osapuolien kirjastoja, kuten `lua-date` (https://github.com/Tieske/date). Tämä kirjasto tarjoaa kattavampia toiminnallisuuksia päivämäärien ja aikojen käsittelyyn. Näin saatat käyttää sitä:

Ensiksi, varmista että olet asentanut `lua-date` kirjaston. Sen voi yleensä asentaa LuaRocksilla seuraavalla komennolla:

```bash
luarocks install lua-date
```

Sitten, voit käyttää sitä Lua-skriptissäsi seuraavasti:

```lua
local date = require("date")

-- Luodaan päivämääräobjekti nykyiselle päivämäärälle ja ajalle
local now = date()

print(now:fmt("%Y-%m-%d %H:%M:%S"))  -- esim., 2022-03-03 14:02:03
```

Tämä esimerkki osoittaa `date` objektin luomisen, joka edustaa nykyistä hetkeä, ja jonka voit sitten muotoilla samankaltaisesti kuin `os.date` funktion, mutta lisäjoustavuudella ja vaihtoehdoilla, joita `lua-date` kirjasto tarjoaa.
