---
title:                "Nykyisen päivämäärän hankkiminen"
aliases: - /fi/lua/getting-the-current-date.md
date:                  2024-02-03T19:10:15.049822-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Nykyisen päivämäärän hakeminen ohjelmoinnissa on olennainen tehtävä monille sovelluksille, mukaan lukien lokit, tapahtumien aikaleimat tai tehtävien ajoittaminen. Luassa tämä toiminnallisuus mahdollistaa ohjelmoijien käsittellä päivämäärä- ja aikaoperaatioita saumattomasti sovelluksissaan, varmistaen että heidän ohjelmistonsa voi toimia tehokkaasti reaaliaikaisen datan kanssa.

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
