---
date: 2024-01-20 17:42:45.307494-07:00
description: "How to: Syv\xE4sukellus: Lua:n patterit ovat regexin (s\xE4\xE4nn\xF6\
  lliset lausekkeet) kaltainen konsepti, mutta yksinkertaisempi. Ei ole aina ollut\
  \ n\xE4in.\u2026"
lastmod: '2024-04-05T22:38:57.291489-06:00'
model: gpt-4-1106-preview
summary: "Syv\xE4sukellus: Lua:n patterit ovat regexin (s\xE4\xE4nn\xF6lliset lausekkeet)\
  \ kaltainen konsepti, mutta yksinkertaisempi. Ei ole aina ollut n\xE4in. Aikaisemmissa\
  \ Lua-versioissa oli rajoitetumpi sana- ja merkkik\xE4sittely, mutta versiosta 5.1\
  \ l\xE4htien `string`-kirjasto on tarjonnut paremmat ty\xF6kalut kuvioon perustuvalle\
  \ tekstink\xE4sittelylle. Vaihtoehtoisesti, kehitt\xE4j\xE4t voivat k\xE4ytt\xE4\
  \xE4 `string.find`- tai `string.match`-funktioita etsimiseen, mutta `string.gsub`\
  \ on tehokas ty\xF6kalu suoraan monen merkin poistoon. Suorituskyvyn kannalta, vaikka\
  \ Lua ei tarjoa regexin t\xE4ytt\xE4 tehoa, sen kuvioratkaisut ovat optimoituja\
  \ eiv\xE4tk\xE4 yleens\xE4 muodosta suorituskyvyn pullonkaulaa."
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
