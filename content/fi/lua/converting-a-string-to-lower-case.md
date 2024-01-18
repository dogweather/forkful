---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Lua: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Merkkijonon muuntaminen pieniksi kirjaimiksi (tunnetaan myös nimellä lowercasing) tarkoittaa, että kaikki merkkijonon kirjaimet muutetaan pieniksi kirjaimiksi. Tämä on hyödyllistä esimerkiksi silloin, kun merkkijonoa käsitellään ja vertaillaan muihin merkkijonoihin, sillä pieniksi muutetut kirjaimet eivät ole CaSe sEnSiTivE (isompien ja pienempien kirjainten huomioon ottaminen). Ohjelmoijat käyttävät tätä toimintoa helpottamaan ja tarkentamaan merkkijonojen vertailuja ja käsittelyjä.

## Kuinka tehdä:
``` Lua
-- Esimerkkimerkkijono
local merkkijono = "Hei MAAILMA!"

-- Pienennetään kirjaimet
merkkijono = string.lower(merkkijono)

-- Tulostaa: hei maailma!
print(merkkijono)
```

``` Lua
-- Toinen vaihtoehtoinen tapa
local merkkijono = "HELLO world!"

-- Käytetään pienaakkosfunktiota
merkkijono = merkkijono:lower()

-- Tulostaa: hello world!
print(merkkijono)
```

## Syväsukellus:
Historiallisesti, merkkijonojen muuntaminen pieniksi kirjaimiksi liittyi tietokoneiden rajoitettuun muistiin. Isojen ja pienten kirjainten ero oli hankala huomioida, joten pieniksi muuntaminen helpotti tekstin käsittelyä. Nykyään tämä toiminto on vieläkin relevantti, sillä se auttaa kirjainten erojen huomioon ottamisessa, kuten vertailuissa. Vaihtoehtoisesti, on olemassa myös iso- ja pienikirjaimen huomioonottavia funktioita, kuten `string.upper()` ja `string.islower()`.

## Katso myös:
- [Lua string library](https://www.lua.org/manual/5.3/manual.html#6.4)
- [The Power of Lowercasing in Programming](https://medium.com/@zacharyefisher/the-power-of-lowercasing-in-programming-357f062299f7)