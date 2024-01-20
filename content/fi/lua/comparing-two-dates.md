---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Datat Vertailu Lua-ohjelmoinnissa

## Mikä & Miksi?

Datat vertailu tarkoittaa kahden päivämäärän suhteen analysoimista. Ohjelmoijat tekevät tämän arvioidakseen aikaleimojen järjestyksen tai laskemaan aikavälin.

## Näin Se Tehdään:

Lua tekee datat vertailun helpoksi `os.difftime` -funktiolla. Tässä on esimerkki.

```Lua
local syntymapaiva1 = os.time({year=1990, month=5, day=15})
local syntymapaiva2 = os.time({year=1980, month=5, day=15})

local eroaika = os.difftime(syntymapaiva1, syntymapaiva2)
print(tonumber(eroaika) / (24 * 60 * 60))  -- päivinä
```

Tämä esimerkki antaa eroajan päivissä käyttäen `os.difftime` -funktiota.

## Syvempi Sukellus:

Historiallisesti Lua ei ollut yhtä tehokas päivämäärän käsittelyssä. `os.difftime` -toiminto lisättiin sen versiossa 5.1. 

Aikaleimojen vertailu voidaan toteuttaa myös muiden ohjelmointikielten, kuten Pythonin ja JavaScriptin avulla, jotka tarjoavat laajemmat funktiovalikoimat päivämäärien käsittelyyn.

Lua:n osalta `os.date`, `os.time` ja `os.difftime` -funktiot ovat yleisesti käytössä aikaleimojen käsittelyssä. Ne tarjoavat kuitenkin vain perustoiminnot eivätkä vastaa kaikkia ohjelmoijan tarpeita.

## Katso Myös:

Seuraavat resurssit tarjoavat lisätietoja datat vertailusta:

- [Keskustelu StackOverflow-sivustolla päivämäärien vertailusta Luan kanssa](https://stackoverflow.com/questions/3808902/comparing-dates-in-lua)
- [Lua-käyttöohje päivämäärän ja ajan funktioiden käyttämisessä](https://www.lua.org/pil/22.1.html)
- [Github-sivuston LuaDate-kirjasto, joka tarjoaa laajemman valikoiman funktioita päivämäärien ja aikojen käsittelyyn](https://github.com/tieske/date)