---
title:                "Tarkistetaan, onko hakemistoa olemassa"
html_title:           "Lua: Tarkistetaan, onko hakemistoa olemassa"
simple_title:         "Tarkistetaan, onko hakemistoa olemassa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Tarkastaa, onko hakemisto olemassa, on tärkeä osa ohjelmointia. Se tarkoittaa, että ohjelma tarkistaa, onko tietyllä polulla oleva hakemisto olemassa vai ei. Tämä on tärkeää esimerkiksi varmistaessaan, että ohjelma käyttää olemassa olevia tiedostoja tai luodessa uusia kansioita.

## How to:

```lua
local lfs = require("lfs")

-- Tarkastaa, onko hakemisto olemassa
local dir_exists = lfs.attributes("polku/hakemisto", "mode") == "directory"
print(dir_exists) -- true tai false, riippuen onko hakemisto olemassa vai ei
```

## Deep Dive:

Hakemistojen olemassaolon tarkistamisen tarve juontaa juurensa Unix-järjestelmistä, joissa kaikki on hierarkkisesti järjestetty tiedostohakemistoihin. Tästä on peräisin myös "ls" -komennon nimi, joka tarkoittaa "listaa tiedostot". Tämä komento on yleisimmin käytetty tapa tarkistaa hakemiston olemassaolo komentoriviltä.

Toinen tapa tarkistaa hakemiston olemassaolo Lua-ohjelmassa on käyttää "io.popen" -funktiota, joka ajaa komentoja komentoriviltä ja palauttaa tuloksen. Käyttämällä tätä funktiota, hakemiston olemassaolo voidaan tarkistaa seuraavalla komennolla:

```lua
local command = io.popen("ls polku/hakemisto")
local result = command:read("*l")
print(result) -- nil, jos hakemistoa ei ole olemassa, tai listaa hakemiston sisällön, jos se on olemassa
```

Tämä ei kuitenkaan ole yhtä tehokasta kuin käyttämällä "lfs" -kirjastoa, sillä "io.popen" -funktio vaatii jokaisen komennon suorittamisen ja palauttaa sitten tuloksen.

## See Also:

"Lfs: Lua File System" -kirjasto mahdollistaa monien tiedostojärjestelmien ominaisuuksien käytön Lua-ohjelmissa, ja sisältää myös funktion hakemistojen olemassaolon tarkistamiseen. Voit lukea lisää tästä kirjastosta täältä: https://keplerproject.github.io/luafilesystem/manual.html#attrib