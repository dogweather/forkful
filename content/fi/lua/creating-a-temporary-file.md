---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tilapäinen tiedosto on ohjelmointimaailmassa yleisesti käytetty tapa tallentaa tietoja tilapäisesti. Näitä tiedostoja käytetään pääasiallisesti säilyttämään tilapäistä tietoa, joka on tarpeen suoritettavan prosessin tai tehtävän aikana.

## Näin tehdään:

Luomme tilapäisen tiedoston Lua-ohjelmointikielellä seuraavalla tavalla:

```Lua
os = require('os')

-- Luo tilapäinen tiedosto
tmpname = os.tmpname()

-- Avaa tiedosto kirjoittamista varten
file = io.open(tmpname, "w")

-- Kirjoita jotain tiedostoon
file:write("Hello, World!")

-- Sulje tiedosto
file:close()
```

Kun suoritat tämän koodin, se luo tilapäisen tiedoston ja kirjoittaa siihen tekstin "Hello, world!". 

## Syvempi sukellus:

Historiallisesti ohjelmoijat ovat luoneet tilapäisiä tiedostoja prosessien ja tehtävien välisten tietojen jakamiseen. Lua yksinkertaistaa tätä prosessia os-moduulillaan, joka tarjoaa `os.tmpname()` -funktion.

Lua ja monet muut ohjelmointikielet tarjoavat myös vaihtoehtoisen tavan säilyttää väliaikaista tietoa muistissa, ns. *prosessin sisäisen muistin*, mutta tämä lähestymistapa ei ole yhtä skaalautuva tai joustava kuin tilapäisten tiedostojen käyttö.

Tilapäisten tiedostojen luomisessa on huomattava, että tiedostot luodaan käyttöjärjestelmän `<TMP>` tai `<TEMP>` -hakemistoon, kaikki riippuu siitä, mitä käyttöjärjestelmä käytät.

## Tutustu myös:

1. Luas os-module official documentation: (https://www.lua.org/manual/5.4/manual.html#6.9)
2. Stack Overflow discussion on Lua temporary files: (https://stackoverflow.com/questions/15197761/lua-create-temporary-file-in-specified-directory)
3. Lua-users wiki / File Input Output: (http://lua-users.org/wiki/FileInputOutput)