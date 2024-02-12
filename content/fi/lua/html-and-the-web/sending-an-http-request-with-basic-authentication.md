---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
date:                  2024-01-20T18:01:57.135003-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lähettää HTTP-pyyntö perusautentikoinnilla tarkoittaa käyttäjätunnuksen ja salasanan lähettämistä palvelimelle. Tämä tehdään yleensä turvallisen resurssin saamiseksi tai API:n käyttämiseksi.

## Näin teet:
```Lua
http = require("socket.http")
ltn12 = require("ltn12")

local username = "kayttaja"
local password = "salasana"
local auth = "Basic " .. (username .. ":" .. password):gsub("\n", ""):gsub("\r\n", ""):encode("base64")

local response = {}
local result, statuscode, response_headers = http.request{
  url = "http://esimerkki.fi/api/data",
  method = "GET",
  headers = {
    ["Authorization"] = auth
  },
  sink = ltn12.sink.table(response)
}

if statuscode == 200 then
  print(table.concat(response))
else
  print("Virhe: " .. tostring(statuscode))
end
```
Näyte tulostus:
```
{"vastaus":"Tämä on API-vastauksen sisältö."}
```

## Syväsukellus
Perusautentikointi on yksi vanhimmista keinoista suojata HTTP-verkkopalveluja. Se on yksinkertainen, mutta ei kovin turvallinen – tieto välittyy tekstimuodossa, mikä tekee siitä haavoittuvan. Parempia vaihtoehtoja ovat OAuth ja API-avaimet, joiden käyttö on lisääntynyt viime aikoina. Tietoturvan lisäämiseksi HTTPS:n käyttö on ehdoton, erityisesti, kun lähetetään tunnistetietoja. Lua:n "socket.http"-kirjasto ei automaattisesti tue perusautentikointia, joten yllä käytetään manuaalista lähestymistapaa, jossa "Authorization"-header luodaan itse.

## Katso myös
- LuaSocket dokumentaatio: http://w3.impa.br/~diego/software/luasocket/http.html
- Base64-koodaus Luassa: https://luarocks.org/modules/luarocks/lua-cjson
- Turvalliset HTTP-autentikointimenetelmät: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
