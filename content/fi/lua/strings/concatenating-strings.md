---
date: 2024-01-20 17:35:17.277629-07:00
description: "How to: (Kuinka tehd\xE4:) T\xE4ss\xE4 pari esimerkki\xE4 yhdist\xE4\
  misest\xE4."
lastmod: '2024-04-05T21:53:58.260837-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) T\xE4ss\xE4 pari esimerkki\xE4 yhdist\xE4misest\xE4."
title: "Merkkijonojen yhdist\xE4minen"
weight: 3
---

## How to: (Kuinka tehdä:)
Tässä pari esimerkkiä yhdistämisestä:

```Lua
-- Yksinkertainen yhdistäminen
local tervehdys = "Hei, " .. "maailma!"
print(tervehdys)  -- Output: Hei, maailma!

-- Muuttujien ja merkkijonojen yhdistäminen
local etunimi = "Maija"
local sukunimi = "Mehiläinen"
local koko_nimi = etunimi .. " " .. sukunimi
print(koko_nimi)  -- Output: Maija Mehiläinen

-- Numberin muuntaminen merkkijonoksi yhdistämisessä
local vuosi = 2023
local viesti = "Nykyinen vuosi on " .. tostring(vuosi)
print(viesti)  -- Output: Nykyinen vuosi on 2023
```

## Deep Dive (Sukellus syvemmälle):
Merkkijonojen yhdistäminen on ollut osa ohjelmointia melkein niin kauan kuin ohjelmointi itsessään. Ennen oli erilaisia tapoja, kuten sprintf-funktio C-kielessä. Lua käyttää kaksoispiste-operaattoria (`..`), joka on yksinkertainen ja nopea tapa yhdistää merkkijonoja.

Suorituskyvyn näkökulmasta suuria merkkijonoja yhdistäessä kannattaa käyttää `table.concat` -funktiota, koska se on tehokkaampi kuin operaattorin toistuva käyttö. Lua optimoi lyhyet yhdistämiset automaattisesti, mutta pidemmissä operaatioissa `table.concat` voi säästää resursseja.

```Lua
local sanat = {"Lua", "on", "mielenkiintoinen", "kieli."}
local lause = table.concat(sanat, " ")
print(lause)  -- Output: Lua on mielenkiintoinen kieli.
```

## See Also (Katso myös):
- Lua 5.4 Reference Manual: [https://www.lua.org/manual/5.4/manual.html#3.4.6](https://www.lua.org/manual/5.4/manual.html#3.4.6)
- Programming in Lua (Book): [https://www.lua.org/pil/](https://www.lua.org/pil/)
- Lua Performance Tips: [http://www.luafaq.org/#T8.2](http://www.luafaq.org/#T8.2)
