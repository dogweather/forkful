---
title:                "CSV-tiedostojen käsittely"
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV (Comma-separated Values) on yksinkertainen tiedostoformaatti datan tallentamiseen. Ohjelmoijat käyttävät sitä, koska se on helppolukuinen ja yhteensopiva useiden työkalujen kanssa.

## How to:
CSV-tiedoston lukeminen ja kirjoittaminen on suoraviivaista. Käytetään io-kirjastoa tiedoston käsittelyyn.

```Lua
-- CSV-tiedoston lukeminen
function lueCSV(tiedosto)
  local taulukko = {}
  for rivi in io.lines(tiedosto) do
    local arvot = {}
    for arvo in rivi:gmatch("[^,]+") do
      table.insert(arvot, arvo)
    end
    table.insert(taulukko, arvot)
  end
  return taulukko
end

-- CSV-tiedoston kirjoittaminen
function kirjoitaCSV(tiedosto, data)
  local tiedosto = io.open(tiedosto, 'w')
  for _, rivi in ipairs(data) do
    tiedosto:write(table.concat(rivi, ','), '\n')
  end
  tiedosto:close()
end

-- Käyttöesimerkki
local data = {
  {"nimi", "ikä", "ammatti"},
  {"Matti", "30", "Insinööri"},
  {"Liisa", "25", "Suunnittelija"}
}

-- Kirjoitetaan CSV
kirjoitaCSV('henkilosto.csv', data)

-- Luetaan CSV
local luettuData = lueCSV('henkilosto.csv')
for _, rivi in ipairs(luettuData) do
  print(table.concat(rivi, ', '))
end
```

## Deep Dive
CSV-formaatti ilmestyi 1970-luvulla. Json ja XML ovat vaihtoehtoisia tiedostoformaattien, joilla on enemmän ominaisuuksia mutta ovat monimutkaisempia. Käyttämällä Lua-kirjastoja kuten Penlight tai CSV, saat lisäominaisuuksia kuten automaattisen tietotyypin tunnistuksen.

## See Also
- Lua Users Wiki CSV-moduulit: http://lua-users.org/wiki/CsvUtils
- Penlight-kirjasto: https://github.com/lunarmodules/Penlight
- RFC 4180, CSV-standardi: https://tools.ietf.org/html/rfc4180
