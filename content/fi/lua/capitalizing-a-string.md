---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Merkkijonon ensimmäisen kirjaimen muuttaminen isoksi parantaa luettavuutta. Sitä käytetään erityisesti otsikoissa ja nimissä.

## How to: (Kuinka tehdä:)
```Lua
-- Esimerkki: merkkijonon ensimmäisen kirjaimen suurentaminen
function suurennaEkaKirjain(str)
    return (str:gsub("^%l", string.upper))
end

-- Käyttö
local esimerkki = "lua on kiva"
print(suurennaEkaKirjain(esimerkki)) -- "Lua on kiva"
```

## Deep Dive (Syväsukellus)
Ennen `string.upper` ja gsub-menetelmien tuloa Luaan, merkkijonojen muokkaaminen vaati manuaalista koodausta. Nykyisin suurennaEkaKirjain-funktio tekee sen näppärästi patern matchingin ja korvausfunktion avulla. Vaihtoehtoina voisi myös käyttää ulkopuolisia kirjastoja, mutta Luassa kaikki tarvittava löytyy jo valmiiksi. Suorituskyvyn osalta tämä Luassa sisäänrakennettu menetelmä on tehokas ja nopea.

## See Also (Katso Myös)
- Lua Users Wiki: http://lua-users.org/wiki/
- Programming in Lua (book): https://www.lua.org/pil/
- String Manipulation with Lua: https://www.tutorialspoint.com/lua/lua_strings.htm
