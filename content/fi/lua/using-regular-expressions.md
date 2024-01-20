---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Säännölliset lausekkeet ovat tekstinkäsittelyyn käytettyjä kaavoja, joilla haetaan, korvataan tai validoidaan tietyn kaavan mukaisia merkkijonoja. Ne tekevät monimutkaisesta tekstin käsittelystä nopeaa ja tehokasta.

## Kuinka:
```Lua
-- Perus haku
local teksti = "Hei, kuinka voit tänään?"
local etsitty = string.match(teksti, "kuinka")
print(etsitty)  -- tulostaa: kuinka

-- Numeroiden löytäminen
local viesti = "Syntymävuosi: 1984, Kengännumero: 42"
for numero in string.gmatch(viesti, "%d+") do
    print(numero)
end
-- tulostaa:
-- 1984
-- 42

-- Merkkijonon korvaaminen
local puhdistettu_teksti = string.gsub("7 kissoa", "7", "seitsemän")
print(puhdistettu_teksti)  -- tulostaa: seitsemän kissoa
```

## Syvemmälle:
Säännölliset lausekkeet juontavat juurensa teoreettisesta tietojenkäsittelystä ja Stephen Kleenen työstä 1950-luvulla. Lua -kielessä regex-vastaava on ominaista sen kuviojärjestelmälle, joka on yksinkertaisempi ja rajoitetumpi kuin monissa muissa ohjelmointikielissä. Lua käyttää omaa kuviojärjestelmäänsä, joka ei ole täysin POSIX- tai PCRE-yhteensopiva, mutta riittävän tehokas perus käyttöön. Vaihtoehtoisesti, voit käyttää luajit ja lrexlib-kirjastoa, jotka tarjoavat laajemmat regex-ominaisuudet.

## Katso Myös:
- Lua manuaali: https://www.lua.org/manual/5.4/
- Online Lua demo, jossa regex-testi: https://www.lua.org/cgi-bin/demo
- lrexlib projekti: https://github.com/rrthomas/lrexlib

Muista, että Luassa käytetään 'pattern matching' -termiä säännöllisten lausekkeiden sijaan, mutta ajatus on sama.