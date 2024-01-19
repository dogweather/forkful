---
title:                "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
html_title:           "Lua: Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneisyyden päivämäärän laskeminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tulevaisuudessa tai menneisyydessä olevan päivämäärän laskeminen on kykyä siirtää päivämäärä eteenpäin tai taaksepäin, määritellyn ajanjakson mukaan. Ohjelmoijat tekevät sen esim. projektinhallintaan ja ajanhallintaan liittyvissä sovelluksissa.

## Kuinka:

Seuraavassa esimerkissä näytämme, kuinka voit laskea kahden päivän päässä olevan päivämäärän nykyhetkestä käyttäen `os.date` ja `os.time` funktioita.
```
Lua
local nykyinen_paivamaara = os.time()
local kaksi_paivaa_secs = 2 * 24 * 60 * 60 -- 2 päivää sekunneissa

local tulevaisuuden_paivamaara = os.date("*t", nykyinen_paivamaara + kaksi_paivaa_secs)

print(os.date("%x", os.time(tulevaisuuden_paivamaara))) 
```
Näyte ulostulo:

`10/14/2022`

## Syvempi Sisältö:

(1) **Historiallinen Konteksti:** Tulevaisuuden tai menneisyyden päivämäärän laskeminen on ollut keskeinen osa ohjelmistoja jo vuosikymmenien ajan. Se voi tarjota tärkeitä tietoja ja ymmärrystä ajasta, joka on kulunut tai joka on edessä.

(2) **Vaihtoehdot:** `os.date` ja `os.time` soveltuvat perus päivämäärän laskentaan. Kuitenkin, jos tarvitset enemmän ominaisuuksia, kuten viikonpäivän selvittämisen, harkitse 'luadate' kirjastoa.

(3) **Toteutuksen Yksityiskohdat:** `os.time` palauttaa sekunteja, jotka ovat kuluneet tietystä kiinteästä pisteestä, yleensä 1. tammikuuta 1970. `os.date` muuttaa tämän sekuntien määrän ymmärrettävämpään muotoon.

## Lisätietoja:

- Lua-dokumentaatio päivämäärän ja ajan käsittelyyn: https://www.lua.org/pil/22.1.html 
- 'luadate' kirjasto laajaspektrisempään päivämäärä- ja aikakäsittelyyn: https://luarocks.org/modules/Tieske/date 
- Artikkeli päivämäärän manipulointi tekniikoista: https://www.computerhope.com/jargon/d/date-calculation.htm 

Huomaa, että voi olla myös muita kirjastoja ja tapoja hallita aikaa ja päivämääriä Luassa. Käytä sitä, mikä parhaiten sopii ohjelmointitarpeisiisi.