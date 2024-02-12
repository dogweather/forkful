---
title:                "Tekstitiedoston lukeminen"
aliases: - /fi/lua/reading-a-text-file.md
date:                  2024-01-20T17:54:37.360305-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstitiedoston lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Lukemalla tekstitiedostoa koodissasi käsittelet tiedon virtoja. Ohjelmoijat lukevat tiedostoja automatisoidakseen datan käsittelyä – hae, muokkaa, tallenna.

## How to: (Miten tehdä:)
```Lua
-- Tiedoston avaaminen lukutilassa
local tiedosto = io.open("esimerkki.txt", "r")

-- Tiedoston sisällön lukeminen
local sisalto = tiedosto:read("*a")
print(sisalto)

-- Muista sulkea tiedosto!
tiedosto:close()
```
Output:
```
Tämä on esimerkkitiedoston sisältö.
Toinen rivi tekstiä.
```

## Deep Dive (Sukellus syvyyksiin)
Lua, kevyt skriptikieli, on ollut olemassa 1990-luvun alkupuolelta. Tiedostonlukutoiminnot ovat perusominaisuuksia, ja `io`-kirjaston käyttö on yleinen tapa käsitellä tiedostoja. Vaihtoehtona Lua tarjoaa myös `file:lines()` funktion iteraattorina, jolla voi käydä läpi tiedoston rivi riviltä – hyvä muistin hallintaan. Tiedostonlukemisen suorituskyky ja turvallisuus syntyvät oikeasta toteutuksesta. Ota huomioon virhetilanteet, kuten olemattomat tiedostot tai käyttöoikeusongelmat.

## See Also (Katso lisäksi)
- Lua 5.4 referenssi (suositeltava versio): https://www.lua.org/manual/5.4/manual.html#6.8
- Programming in Lua (kirja ohjelmoinnista Luassa): https://www.lua.org/pil/ 
- Lua-users wiki (käyttäjien ylläpitämä tietopankki): http://lua-users.org/wiki/IoLibraryTutorial
