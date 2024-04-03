---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:44.566675-07:00
description: "Ohjelmoinnissa s\xE4\xE4nn\xF6lliset lausekkeet mahdollistavat merkkijonojen\
  \ hakemisen ja manipuloinnin tiettyjen mallien perusteella. Ohjelmoijat k\xE4ytt\xE4\
  v\xE4t niit\xE4\u2026"
lastmod: '2024-03-13T22:44:56.687965-06:00'
model: gpt-4-0125-preview
summary: "Ohjelmoinnissa s\xE4\xE4nn\xF6lliset lausekkeet mahdollistavat merkkijonojen\
  \ hakemisen ja manipuloinnin tiettyjen mallien perusteella."
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

## Mikä & Miksi?

Ohjelmoinnissa säännölliset lausekkeet mahdollistavat merkkijonojen hakemisen ja manipuloinnin tiettyjen mallien perusteella. Ohjelmoijat käyttävät niitä tehtäviin, kuten validointiin, hakuun ja tekstimanipulaatioon, niiden monipuolisuuden ja tehokkuuden ansiosta monimutkaisten merkkijono-operaatioiden käsittelyssä.

## Miten:

Lua ei tue säännöllisiä lausekkeita natiivisti samalla tavalla kuin kielet kuten Perl tai Python. Sen sijaan se tarjoaa mallihakutoiminnallisuutta, joka kattaa monia säännöllisten lausekkeiden yleisiä käyttötarkoituksia. Kuitenkin täyden säännöllisten lausekkeiden tuen saamiseksi voidaan käyttää kolmannen osapuolen kirjastoa, kuten `lrexlib`.

### Perusmallihaku Luassa:

Lua tarjoaa voimakkaan mallihakujärjestelmän, jota voit käyttää yksinkertaisiin korvaamisiin ja hakuun:

```lua
-- Yksinkertainen haku
local str = "Hello, World!"
if string.find(str, "World") then
  print("Osuma löydetty!")
end
-- Tuloste: Osuma löydetty!

-- Yksinkertainen korvaaminen
local s = string.gsub("Lua on mahtavaa!", "mahtavaa", "upeaa")
print(s)
-- Tuloste: Lua on upeaa!
```

### Merkkijono-osien ottaminen talteen:

Voit ottaa talteen merkkijonojen osia, jotka vastaavat malleja:

```lua
local date = "Tänään on 17/05/2023."
local d, m, y = string.match(date, "(%d+)/(%d+)/(%d+)")
print("Päivä:", d, "Kuukausi:", m, "Vuosi:", y)
-- Tuloste: Päivä: 17 Kuukausi: 05 Vuosi: 2023
```

### `lrexlibin` käyttäminen säännöllisiin lausekkeisiin:

Käyttääksesi varsinaisia säännöllisiä lausekkeita, voit asentaa ja käyttää `lrexlibiä`. Olettaen, että sinulla on se asennettuna (`luarocks install lrexlib-pcre`), voit suorittaa monimutkaisempia mallihakuja:

```lua
local rex = require 'rex_pcre'

local text = "Sade Espanjassa pysyy pääasiassa tasangolla."
local regex = "\\bS\\w+"
local count, err = rex.gsub(text, regex, function(w)
  return w:upper()
end)
if err then
  print("Virhe:", err)
else
  print("Muokattu teksti:", text)
  print("Korvauksia tehty:", count)
end
-- Esimerkkituloste: Muokattu teksti: Sade ESPANJASSA pysyy PÄÄASIASSA tasangolla.
-- Korvauksia tehty: 3
```

Yllä olevat esimerkit kuvaavat peruskäyttöä Luassa oman mallihakujärjestelmänsä kanssa ja kuinka hyödyntää säännöllisten lausekkeiden voimaa `lrexlibin` kautta. Olitpa sitten suorittamassa yksinkertaisia merkkijonojen manipulaatioita tai tarvitset säännöllisten lausekkeiden täyttä monipuolisuutta, Lua yhdessä voimakkaiden kirjastojen kanssa voi vastata tarpeisiisi.
