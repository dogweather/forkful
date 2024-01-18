---
title:                "Verkkosivun lataaminen"
html_title:           "Lua: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Verkkosivujen lataaminen tarkoittaa tiedon hakemista internetistä ja sen tallentamista paikalliseen tietokoneeseen. Ohjelmoijat tekevät tätä esimerkiksi uuden sisällön hakuun tai tiedon analysointiin.

## Miten:

Esimerkiksi alla oleva koodi näyttää, kuinka voimme ladata verkkosivun käyttäen Lua-ohjelmointikieltä ja kirjastoa nimeltä "luasocket". Tämä koodi lataa google.com -sivun ja tulostaa sen sisällön.

```
Lua näyte 
luaSokea = vaativa = require "lua-socket"
local url = "http://www.google.com"
local body, error = socket.http.request(url)
if not body then
    print(error)
else
    print(body)
end
```

Tulostaessa näemme, että sivun HTML-koodi on ladattu ja tulostettu komentokehotteeseen. Tätä tekniikkaa voidaan käyttää moniin eri tarkoituksiin, kuten tiedon kaivamiseen tai tiedon visualisointiin.

## Syventävä tarkastelu:

Tämä web-sivujen lataustekniikka oli vallankumouksellinen aikanaan, koska se mahdollisti verkkosivujen sisällön käsittelyn ohjelmallisesti. Nykyään on olemassa myös muita tapoja ladata verkkosivuja, kuten käyttäen REST API:a tai HTML-kasana, mutta lua-socket pysyy silti suosittuna vaihtoehtona sen yksinkertaisuuden ja nopeuden vuoksi. Tämän toteuttamiseksi lua-socket käyttää "socket" kirjastoa, joka käytännössä muodostaa yhteyden annettuun verkkosivuun ja hakee sen HTML-koodin.

## Katso myös:

- Lua-kielen opas: https://www.lua.org/docs.html
- Lua-socket kirjaston dokumentaatio: https://luasocket.github.io/luasocket/
- Lua-kielen foorumi: https://www.lua.org/lua-l.html