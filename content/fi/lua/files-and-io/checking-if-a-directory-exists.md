---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:55.183791-07:00
description: "Hakemiston olemassaolon tarkistaminen on perusoperaatio, kun kirjoitetaan\
  \ skriptej\xE4, jotka vuorovaikuttavat tiedostoj\xE4rjestelm\xE4n kanssa. Se varmistaa,\u2026"
lastmod: '2024-03-13T22:44:56.711141-06:00'
model: gpt-4-0125-preview
summary: "Hakemiston olemassaolon tarkistaminen on perusoperaatio, kun kirjoitetaan\
  \ skriptej\xE4, jotka vuorovaikuttavat tiedostoj\xE4rjestelm\xE4n kanssa."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Miten:
Luassa ei ole sisäänrakennettua funktiota suoraan tarkistaa, onko hakemisto olemassa, joten usein nojaudutaan Lua File System (lfs) -kirjastoon, joka on suosittu kolmannen osapuolen kirjasto tiedosto-operaatioihin.

Varmista ensin, että sinulla on Lua File System asennettu. Jos ei, sen voi yleensä asentaa käyttämällä LuaRocksia:

```sh
luarocks install luafilesystem
```

Sen jälkeen voit käyttää seuraavaa esimerkkiä tarkistaaksesi hakemiston olemassaolon:

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr and attr.mode == "directory"
end

-- Tarkista, onko tietty hakemisto olemassa
if directoryExists("/path/to/your/directory") then
    print("Hakemisto on olemassa.")
else
    print("Hakemistoa ei ole olemassa.")
end
```

Tämä tulostaa:

```
Hakemisto on olemassa.
```

Tai, jos hakemistoa ei ole olemassa:

```
Hakemistoa ei ole olemassa.
```

Tämä lähestymistapa käyttää `lfs.attributes`-funktiota saadakseen polun attribuutit. Jos polku on olemassa ja sen `mode`-attribuutti on `directory`, se vahvistaa hakemiston olemassaolon.
