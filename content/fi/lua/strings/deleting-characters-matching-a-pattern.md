---
date: 2024-01-20 17:42:45.307494-07:00
description: "Mit\xE4 & Miksi? Lua:ssa merkkien poistaminen kuvion mukaan on tapa\
  \ siivota merkkijonoja \u2013 otetaan pois mit\xE4 ei tarvita. Koodarit k\xE4ytt\xE4\
  v\xE4t t\xE4t\xE4 esimerkiksi\u2026"
lastmod: '2024-03-13T22:44:56.682296-06:00'
model: gpt-4-1106-preview
summary: "Mit\xE4 & Miksi."
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## How to:
Miten:
```Lua
local str = "Käyttäjä123Syöte!"
local pattern = "%D"
-- Poistetaan kaikki ei-numeeriset merkit
local cleanStr = str:gsub(pattern, "")
print(cleanStr)  -- Tulostaa: 123
```
```Lua
local finnish_text = "Hänellä on 2 kissaa ja 3 koiraa."
local pattern = "%d"
-- Poistetaan kaikki numerot
finnish_text = finnish_text:gsub(pattern, "")
print(finnish_text)  -- Tulostaa: Hänellä on  kissaa ja  koiraa.
```

## Deep Dive
Syväsukellus:
Lua:n patterit ovat regexin (säännölliset lausekkeet) kaltainen konsepti, mutta yksinkertaisempi. Ei ole aina ollut näin. Aikaisemmissa Lua-versioissa oli rajoitetumpi sana- ja merkkikäsittely, mutta versiosta 5.1 lähtien `string`-kirjasto on tarjonnut paremmat työkalut kuvioon perustuvalle tekstinkäsittelylle.

Vaihtoehtoisesti, kehittäjät voivat käyttää `string.find`- tai `string.match`-funktioita etsimiseen, mutta `string.gsub` on tehokas työkalu suoraan monen merkin poistoon.

Suorituskyvyn kannalta, vaikka Lua ei tarjoa regexin täyttä tehoa, sen kuvioratkaisut ovat optimoituja eivätkä yleensä muodosta suorituskyvyn pullonkaulaa.

## See Also
Lisätietoja:
- [Lua 5.4 Reference Manual: Patterns](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [Programming in Lua (first edition): Strings](https://www.lua.org/pil/20.2.html)
