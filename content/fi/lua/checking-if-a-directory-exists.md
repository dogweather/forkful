---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Lua: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi? 

Hakemiston olemassaolon tarkistaminen tarkoittaa, että ohjelmoijat varmistavat, onko tiettyyn polkuun viittaava hakemisto olemassa ennen kuin jatkavat koodin suorittamista. Tämä on tärkeää välttääkseen virheitä, joita saattaisi ilmetä, jos koodi yrittäisi päästä käsiksi olemattomaan hakemistoon.

## Kuinka tehdä se: 
Suorita pätkä koodia, joka tarkistaa, onko hakemisto olemassa.

```Lua
local f = io.open("/tähän/oma/polku", "r")
if f == nil then
    print('Hakemistoa ei ole olemassa.')
else
    f:close()
    print('Hakemisto on olemassa.')
end
```
Jos hakemisto ei ole olemassa, saat vastaukseksi 'Hakemistoa ei ole olemassa.' Jos se on olemassa, saat vastaukseksi 'Hakemisto on olemassa.'

## Syvempi tutkiskelu

Vaikka hakemistojen olemassaolon tarkistaminen ei ole alkuperäinen osa Lua-kieltä, se lisättiin myöhemmin 'io'-moduuliin, joka sisältää I/O-toiminnot. Vaihtoehtona on myös `lfs` (LuaFileSystem) -kirjasto, joka tarjoaa monipuolisempia tiedosto- ja hakemistotoimintoja. Hakemistojen olemassaolon tarkistaminen toteutetaan avamalla tiedosto lukutilassa. Jos tiedostoa ei voida avata, siitä palautuu `nil`, muutoin se suljetaan heti avauksen jälkeen. 

## Katso myös

[Lua-ohjelmointikielen kotisivu](http://www.lua.org)
[LuaFileSystem-dokumentaatio](https://keplerproject.github.io/luafilesystem/)