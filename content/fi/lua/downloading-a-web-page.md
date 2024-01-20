---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# "Mitä & Miksi?"

Lataat verkkosivun kun tuot sen sisältöä tietokoneellesi nähtäväksi ilman jatkuvaa internet-yhteyttä. Ohjelmoijat tekevät tämän mm. tietojen keräämisen, verkkosivujen testaamisen tai offline-lukemiseen.

# "Näin se tehdään:"

```Lua
-- tarvitsemme socketin http-moduuli
http = require("socket.http")

-- Haetaan sivu
sivun_tiedot = http.request("http://www.example.com")

-- Tulostetaan sivun tiedot
print(sivun_tiedot)
```
Tämä koodipätkä lataisi sivun www.example.com sisällön ja tulostaisi sen merkkijonona. 

# "Syvempi sukellus":

Lua tarjoaa luonnollisen syntaksin verkkoyhteyksien käsittelyyn socket-kirjaston avustuksella, jonka http-moduuli auttaa erityisesti HTTP-pyyntöjen tekemisessä. Tämän tilalle voit harkita esim. wget-ohjelmaa tai curl-kirjastoa, jos tarvitset enemmän mukautettavuutta tai suoritusnopeutta.

Verkkosivujen lataus Lua:lla on yksinkertaista: http.request funktio ottaa URL:n argumenttina, lähettää GET-pyynnön ja palauttaa vastauksen sisällön merkkijonona. Kuitenkin tässä voi olla haitallista se, että koko sivun sisältö ladataan kerralla muistiin. Suuret sivut voivat näin syödä muistia nopeasti, joten muista aina olla varovainen, kun luet tuntemattomia URL:ja.

# "Lue myös":

Lua socket-kirjaston dokumentaatio (http://w3.impa.br/~diego/software/luasocket/http.html)

Tutorial Lua:n http-moduulin käyttöön (https://www.tutorialspoint.com/lua/lua_http_requests.htm)

Curl-kirjaston dokumentaatio (https://curl.haxx.se/libcurl/c/)