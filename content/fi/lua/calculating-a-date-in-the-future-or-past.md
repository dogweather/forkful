---
title:                "Tulevan tai menneen päivämäärän laskeminen"
html_title:           "Lua: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Mitä ja miksi?
Kun ohjelmoijat haluavat laskea päivämäärän tulevaisuudessa tai menneisyydessä, he käyttävät laskentaohjelmaa. Tämä voi olla hyödyllistä esimerkiksi aikaleimojen luomiseen tai tapahtumien ajoittamiseen.

Kuinka:
```Lua
-- Laske 10 päivää nykyisestä päivästä
local nykyinen_paiva = os.date("*t")
local tuleva_paiva = os.time(nykyinen_paiva) + 10*24*60*60 -- lisää 10 päivää (24 tuntia * 60 minuuttia * 60 sekuntia)
local lasketut_paivamaara = os.date("%x", tuleva_paiva) -- muunna tuleva päivä stringiksi
print(lasketut_paivamaara) --> 07/16/20
```

Syväsukellus:
Historiallinen konteksti:
Päivämäärän laskenta on ollut tärkeää jo antiikin ajoista lähtien esimerkiksi kalenterien luomisessa. Nykyään tämä toiminto on yksinkertaistunut ja saatavilla helposti ohjelmointikielillä, kuten Lua.

Vaihtoehtoja:
On olemassa muitakin tapoja laskea päivämäärä tulevaisuudessa tai menneisyydessä, kuten käyttämällä aikajanoja tai erityisiä kirjastoja.

Toteutuksen yksityiskohdat:
Lua:ssa päivämäärän laskentaan käytetään os.date() ja os.time() -funktioita. Os.time() muuntaa päivän, kuukauden ja vuoden arvot sekunneiksi ja os.date() muuntaa sekunnit takaisin päivämääräksi. Tarkempien päivämäärien laskentaan voidaan myös käyttää date-luokkaa.

Katso myös:
- Lua documentation (https://www.lua.org/pil/22.1.html)
- Date and Time Library (https://github.com/Tieske/date)