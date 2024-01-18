---
title:                "Työskentely csv:n kanssa"
html_title:           "Lua: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

CSV on tiedostotyyppi, jota käytetään usein tietojen tallentamiseen taulukkomuodossa. Tämä tekee siitä erittäin hyödyllisen ohjelmoinnissa, sillä se mahdollistaa helposti muokattavien ja jaoteltavien tietojen tallentamisen. Siksi useimmat ohjelmoijat joutuvat käsittelemään CSV-tiedostoja.

## Miten?

Lua-ohjelmointikielellä on monia eri tapoja käsitellä CSV-tiedostoja. Tässä on muutamia esimerkkejä:

```Lua 
local csv = require("csv")
                   .new()
                   .delimiter(";")
                   .parse("tiedostonimi.csv")

--Tulostaa ensimmäisen rivin ensimmäisen sarakkeen arvon
print(csv[1][1]) 
```

```Lua
local csv = require("csv")

--Luo uuden CSV-tiedoston
csv
  .new()
  .headers({"Nimi", "Ikä", "Sukupuoli"})
  .append({"Pekka", 30, "Mies"})
  .append({"Maija", 25, "Nainen"})
  .write("uusi_tiedosto.csv")
```

``Lua
local csv = require("csv")

--Tulostaa kaikki CSV-tiedoston arvot
for _, row in ipairs(csv.parse("tiedostonimi.csv")) do
  for _, col in ipairs(row) do
    print(col)
  end
end
```

## Syventävää tietoa

CSV-tiedostot ovat olleet käytössä jo vuodesta 1972 ja ne ovat edelleen yksi yleisimmin käytetyistä tiedostomuodoista. Vaikka Lua tarjoaa sisäänrakennetun "io" -moduulin, monet ohjelmoijat valitsevat käyttää CSV-paketteja, kuten "lua-csv", joka tarjoaa monipuolisia ominaisuuksia CSV-tiedostojen käsittelyyn.

## Katso myös

- [Lua-csv dokumentaatio] (https://keplerproject.github.io/lua-cjson/)
- [io-moduuli dokumentaatio] (https://www.lua.org/manual/5.4/manual.html#pdf-io)