---
title:                "Perusautentikoinnin kanssa http-pyynnön lähettäminen."
html_title:           "Lua: Perusautentikoinnin kanssa http-pyynnön lähettäminen."
simple_title:         "Perusautentikoinnin kanssa http-pyynnön lähettäminen."
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lähettäminen HTTP-pyyntö basic authenticationin kanssa tarkoittaa, että pyyntöön lisätään käyttäjän tunnistetiedot, kuten käyttäjätunnus ja salasana. Tämä auttaa varmistamaan, että vain oikeat käyttäjät pääsevät käsiksi tiettyyn resurssiin, kuten verkkosivulle tai sovellukseen. Ohjelmoijat käyttävät tätä tekniikkaa parantamaan tietoturvaa ja estämään luvattomien käyttäjien pääsyn.

## Kuinka:
```
-- Lähetä HTTP-pyyntö basic authenticationilla
-- Ota käyttöön http-kirjasto
local http = require("socket.http")

-- Muodosta pyyntö
local username = "käyttäjätunnus"
local password = "salasana"
local authtoken = username..":"..password
local response_body = {}
local request_body = "Tervetuloa!"

-- Lähetä pyyntö
local code, headers, status = http.request{
  url = "https://example.com",
  method = "POST",
  source = ltn12.source.string(request_body),
  headers = {
    ["Content-Type"] = "text/plain",
    ["Authorization"] = "Basic "..mime.b64(authToken)
  },
  sink = ltn12.sink.table(response_body)
}

-- Tulosta vastauskoodi ja vastauksen sisältö
print(code)
print(table.concat(response_body))
```

Tulostettu vastauskoodi näyttää pyynnön onnistumisen tai mahdollisen virheen. Vastauksen sisältö sisältää palvelimesta saadun vastauksen.

## Syvempi sukellus:
HTTP-perusautentikointi ei ole täysin turvallinen menetelmä, koska käyttäjätunnus ja salasana välitetään perusrakenteessa (base64) ja ne voidaan helposti salata. Tästä syystä kehitettiin muita keinoja, kuten Digest authentication, joka käyttää haaste-vastaus-menetelmää ja SSL / TLS-salausta.

Jos käytät Lua-versiota, joka ei tue socket.http-kirjastoa, voit käyttää muita kirjastoja, kuten LuaSocket tai luasec.

## Katso myös:
- [HTTP-pyyntöjen lähettäminen Lua-kielellä](https://wiki.garrysmod.com/page/HTTP_Library)
- [LuaSocket-kirjasto](https://luarocks.org/modules/luarocks/luasocket)
- [Sec-kirjasto](https://github.com/openresty/lua-resty-core/blob/master/lib/ngx/ssl.md)