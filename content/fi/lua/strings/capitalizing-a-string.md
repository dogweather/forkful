---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
aliases:
- /fi/lua/capitalizing-a-string.md
date:                  2024-02-03T19:05:54.861204-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Merkkijonon alkukirjaimen muuttaminen isoksi tarkoittaa jokaisen lauseen sanan ensimmäisen merkin muuttamista isoksi kirjaimeksi, samalla varmistaen, että loput merkit ovat pieniä kirjaimia. Tätä tekniikkaa käytetään yleisesti tekstin muotoiluun ammattimaisemman tai luettavamman tulosteen saavuttamiseksi, kuten otsikoiden valmistelussa tai käyttäjän syötteen näyttämisessä.

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
