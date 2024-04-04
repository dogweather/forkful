---
changelog:
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 18:00:16.334061-07:00
description: "Kuinka: Lua ei sis\xE4ll\xE4 natiivia HTTP-tukea, joten k\xE4yt\xE4\
  mme kirjastoja. Yleinen valinta on `lua-requests`. T\xE4ss\xE4 nopea esimerkki."
lastmod: '2024-04-04T00:26:50.639947-06:00'
model: gpt-4-0125-preview
summary: "Lua ei sis\xE4ll\xE4 natiivia HTTP-tukea, joten k\xE4yt\xE4mme kirjastoja."
title: "L\xE4hett\xE4m\xE4ss\xE4 HTTP-pyynt\xF6\xE4"
weight: 44
---

## Kuinka:
Lua ei sisällä natiivia HTTP-tukea, joten käytämme kirjastoja. Yleinen valinta on `lua-requests`. Tässä nopea esimerkki:

```lua
local requests = require('requests')

-- GET-pyyntö
local response = requests.get('https://api.example.com/data')
print(response.status_code)
print(response.text)

-- POST-pyyntö jonkin datan kanssa
local post_response = requests.post('https://api.example.com/post', {data = {key1 = 'value1', key2 = 'value2'}})
print(post_response.status_code)
print(post_response.text)
```

Esimerkkivastaus voi näyttää tältä:

```lua
200
"{\"data\":\"Tässä pyytämäsi tiedot!\"}"

201
"{\"success\":true,\"message\":\"Tiedot vastaanotettu!\"}"
```

## Syväsukellus
Luassa sen yksinkertaisuus ei natiivisti käsitä HTTP:tä, minkä vuoksi kirjastot astuvat kuvaan. `lua-requests` heijastelee Python Requests -kirjaston toiminnallisuutta, tehden siitä helpon niille, jotka ovat tuttuja Pythonin kanssa.

Muita vaihtoehtoja sisältävät `LuaSocket` alemman tason HTTP-työhön ja `luasocket.http` suurempaan kontrolliin. Lualla on myös sidonnaisuuksia `libcurl`iin (via `Lua-cURL`) monimutkaisiin HTTP-toimintoihin.

Historiallisesti sisäänrakennetun HTTP-tuen puuttuminen heijastaa Luan juuria upotetuissa järjestelmissä, joissa verkkoprogrammointi ei ollut prioriteetti. Sen evoluutio ulkoisten kirjastojen kautta esimerkillistää yhteisön sopeutumiskykyä ja kielen laajennettavuutta.

Toteutuksen kannalta, kun lähetät HTTP-pyynnön, se matkaa verkon yli määriteltyyn palvelimeen. Palvelin käsittelee sen ja vastaa. Luan kirjastot abstrahoivat tarvittavan socket-ohjelmoinnin, käsitellen kaiken verkkoviestinnän pikkutarkan, jotta voit keskittyä itse pyyntöön ja vastaukseen.

## Katso Myös
- [lua-requests GitHub-repositorio](https://github.com/JakobGreen/lua-requests)
- [LuaSocket Käsikirja](http://w3.impa.br/~diego/software/luasocket/http.html)
