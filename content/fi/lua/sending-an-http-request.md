---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

# HTTP-Pyyntöjen lähettäminen Lualla

## Mikä & Miksi?
HTTP-pyyntöjen lähettäminen on tapa kommunikoida verkon välityksellä palvelimen kanssa. Ohjelmoijat tekevät sen tiedon hankkimiseksi tai välittämiseksi palvelimelle.

## Kuinka:
Lua tarjoaa useita työkaluja HTTP-pyyntöjen tekemiseen. Tässä esimerkki `lua-http` -kirjaston käytöstä:

```Lua
local http_request = require "http.request"
local req = http_request.new_from_uri("http://example.com")
local headers, stream = req:go()
local body = assert(stream:get_body_as_string())
if headers:get ":status" == "200" then
  print(body)
end
```

Tämä scripti lähettää HTTP GET -pyynnön osoitteeseen "http://example.com" ja tulostaa palvelimelta saadun vastauksen, jos se on onnistunut.

## Syvempi sukellus:
HTTP-pyyntöjen lähettäminen on ollut olennainen osa web-ohjelmointia sen alkuajoista lähtien. Lua, vaikka se ei olekaan pääasiallinen verkkokieli, tarjoaa täyden tuen siihen.

Vaihtoehtoisesti, voit käyttää `luasocket` ja `luasec` -kirjastoja saman tavoitteen saavuttamiseksi, vaikkakin hieman monimutkaisemmalla tavalla.

Toteutuksen yksityiskohtien osalta, `lua-http` lähettää pyynnön, odottaa vastausta, tulkitsee sen ja palauttaa tuloksen ohjelmalle. Tämä kaikki tapahtuu TCP-yhteyden yli, joka on HTTP-protokollan alla toimiva perusta.

## Katso myös:
Lisätietoja ja apua löydät seuraavista lähteistä:

1. [Lua:n virallinen dokumentaatio](https://www.lua.org/manual/5.4/)
2. [lua-http](https://github.com/daurnimator/lua-http)
3. [luasocket ja luasec](http://w3.impa.br/~diego/software/luasocket/)