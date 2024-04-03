---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:54.861204-07:00
description: "Kuinka: Lua ei sis\xE4ll\xE4 valmista funktiota merkkijonojen alkukirjaimen\
  \ muuttamiseksi isoksi, mutta voit helposti saavuttaa t\xE4m\xE4n teht\xE4v\xE4\
  n k\xE4ytt\xE4m\xE4ll\xE4 perus\u2026"
lastmod: '2024-03-13T22:44:56.681264-06:00'
model: gpt-4-0125-preview
summary: "Lua ei sis\xE4ll\xE4 valmista funktiota merkkijonojen alkukirjaimen muuttamiseksi\
  \ isoksi, mutta voit helposti saavuttaa t\xE4m\xE4n teht\xE4v\xE4n k\xE4ytt\xE4\
  m\xE4ll\xE4 perus merkkijonon k\xE4sittelyn funktioita."
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
weight: 2
---

## Kuinka:
Lua ei sisällä valmista funktiota merkkijonojen alkukirjaimen muuttamiseksi isoksi, mutta voit helposti saavuttaa tämän tehtävän käyttämällä perus merkkijonon käsittelyn funktioita. Tässä on yksinkertainen funktio yhden sanan ensimmäisen kirjaimen muuttamiseksi isoksi:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- Tuloste: Hello
```

Jotta voit muuttaa jokaisen lauseen sanan ensimmäisen kirjaimen isoksi, voit jakaa lauseen sanoiksi, muuttaa jokaisen isoksi ja sitten yhdistää ne uudelleen:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- Tuloste: Hello World From Lua
```

Jos työskentelet projektissa, jossa suorituskyky on avainasemassa ja tarvitset edistyneempiä merkkijonon käsittelyn ominaisuuksia, harkitse kolmannen osapuolen kirjaston, kuten `Penlight`, käyttöä. Penlight laajentaa Luaa monipuolisemmilla merkkijonon käsittelyn toiminnoilla, muiden hyödyllisyyksien lisäksi:

```lua
-- Olettaen, että Penlight on asennettu:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- Tuloste: Hello lua users

-- Huomautus: Penlightin capitalized-funktio isontaa vain ensimmäisen sanan.
-- Jokaisen sanan isontamiseen tarvitset silti räätälöidyn ratkaisun tai tutkia muita kirjastoja.
```
