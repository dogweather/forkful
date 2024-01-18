---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Lua: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Tekstitiedoston kirjoittaminen on yksinkertainen tapa tallentaa tietoa ohjelman suorittamisen aikana. Ohjelmoijat käyttävät sitä usein tallentaakseen tilapäistä tietoa, kuten ohjelman käyttäjän antamia syötteitä tai ohjelman laskemia tietoja.

## Ohjeet:

```Lua 
file = io.open("tiedosto.txt", "w") -- Luo tai avaa tiedoston kirjoitettavaksi

file:write("Tervetuloa, ohjelmoija!") -- Kirjoita teksti tiedostoon

file:close() -- Sulje tiedosto 
```

Tämä koodiesimerkki luo tekstitiedoston nimeltä "tiedosto.txt" ja kirjoittaa siihen tekstirivin "Tervetuloa, ohjelmoija!". Lopuksi tiedosto suljetaan jotta muut ohjelmat voivat käyttää sitä.

## Syventävä tieto:

Texkitiedoston kirjoittamisessa käytetään yleisesti io-kirjastoa, mutta on myös muita vaihtoehtoja kuten file-kirjasto. Tiedoston avaamisessa käytetään "w" -parametria, joka tarkoittaa, että tiedosto avataan kirjoitettavaksi. Voit myös käyttää "a" -parametria tiedoston lisäämiseen (append) tai "r+" -parametria tiedoston lukemiseen ja kirjoittamiseen. Tiedoston sulkeminen on tärkeää jotta muut ohjelmat voivat käyttää sitä.

## Katso myös:

[W3Schools: Lua - Writing Files](https://www.w3schools.com/lua/lua_file_io.asp)