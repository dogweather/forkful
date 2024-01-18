---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Lua: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonon pituuden löytäminen tarkoittaa sitä, että selvität, kuinka monta merkkiä tai merkintää siinä on. Ohjelmoijat tekevät tätä esimerkiksi tekstin muotoilun, tietokannan käsittelyn tai järjestelyjen tekemistä varten.

## Miten:
Lua-koodiblokkien sisällä näet esimerkkejä siitä, miten voit hakea merkkijonon pituuden ja millainen tulos siitä saat.

```Lua
local merkkijono = "Tämä on merkkijono."
print(string.len(merkkijono))
-- Output: 21
```

```Lua
local merkkijono = "Tässä on 123 merkkiä."
print(string.len(merkkijono))
-- Output: 21
```

```Lua
local merkkijono = ""
print(string.len(merkkijono))
-- Output: 0
```

## Syväsukellus:
Merkkijonon pituuden määrittäminen on ollut oleellinen osa ohjelmointia jo kauan aikaa. Monet kielet, kuten C ja Java, käyttävät nollapäätteisiä merkkijonoja, joissa käytetään erillistä merkkiä lopettamaan merkkijono. Tämä voi olla hankalaa käsitellä, sillä merkkiä täytyy varoa oikean merkkijonon pituuden laskemisessa. Lua käyttää sen sijaan pituutta tallentavaa arvoa, jolloin merkkijonon pituuden hakeminen on huomattavasti helpompaa.

## Katso myös:
Voit löytää lisätietoa Lua:n string-kirjastosta Lua:n viralliselta verkkosivustolta.
https://www.lua.org/manual/5.3/manual.html#6.4