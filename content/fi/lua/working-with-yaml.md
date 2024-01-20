---
title:                "Työskentely yaml:n kanssa"
html_title:           "Lua: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Lua on skriptauskieli, jota käytetään usein ohjelmoinnissa. Yksi sen ominaisuuksista on kyky käsitellä erilaisia tiedostomuotoja, kuten YAML. YAML on tekstipohjainen formaatti, jonka avulla voidaan esittää tietoa hierarkkisessa muodossa, mikä tekee siitä erittäin hyödyllisen ohjelmoijille.

## Kuinka?

YAML-tiedostojen käsittely Lua-kielellä on hyvin yksinkertaista. Se vaatii vain muutaman rivin koodia, ja sen jälkeen voit helposti lukea tai kirjoittaa YAML-tiedostoja.

**Lue YAML-tiedosto:**

```Lua
local yaml = require 'yaml'
-- lataa YAML-kirjasto
local myData = yaml.load_file('tiedosto.yaml')
-- lue tiedostosta data ja tallenna muuttujaan
```

**Kirjoita YAML-tiedosto:**

```Lua
local yaml = require 'yaml'
-- lataa YAML-kirjasto
local data = {name = "Matti", age = 25, city = "Helsinki"}
-- määritä data taulukkoon
local myData = yaml.dump(data)
-- tallenna tiedosto myData-muuttujaan
yaml.save("tiedosto.yaml", myData)
-- tallenna tiedosto levylle
```

## Syvenny asiaan

YAML on lyhenne sanoista "YAML Ain't Markup Language" ja se on kehitetty vuonna 2001. Se on helppolukuinen ja ihmisystävällinen formaatti, jota käytetään usein konfiguraatio-tiedostoissa ja tietojen tallentamisessa. Vaihtoehtoina YAML:lle ovat esimerkiksi XML ja JSON, mutta YAML on yleensä helpompi lukea ja kirjoittaa.

Lua tarjoaa kirjaston nimeltä luayaml, joka mahdollistaa YAML-tiedostojen käsittelyn. Tämä kirjasto on kehitetty C-kielellä, ja se on nopea ja tehokas. Se sisältää myös monia hyödyllisiä apufunktioita, kuten tiedostojen lukemisen ja tallentamisen, sekä mahdollisuuden muuntaa JSON-formaattiin.

## Katso myös

- [YAML-dokumentaatio](https://yaml.org/)
- [Lua-opas: Perusteet](https://lua.org/docs.html)