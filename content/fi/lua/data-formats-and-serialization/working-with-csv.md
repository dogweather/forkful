---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:41.407172-07:00
description: "CSV-tiedostojen (pilkuilla erotetut arvot) k\xE4sittelyyn kuuluu tekstidatan\
  \ j\xE4sent\xE4minen ja tuottaminen riveihin ja sarakkeisiin j\xE4rjestettyn\xE4\
  , k\xE4ytt\xE4en\u2026"
lastmod: '2024-02-25T18:49:53.630550-07:00'
model: gpt-4-0125-preview
summary: "CSV-tiedostojen (pilkuilla erotetut arvot) k\xE4sittelyyn kuuluu tekstidatan\
  \ j\xE4sent\xE4minen ja tuottaminen riveihin ja sarakkeisiin j\xE4rjestettyn\xE4\
  , k\xE4ytt\xE4en\u2026"
title: "Ty\xF6skentely CSV:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

CSV-tiedostojen (pilkuilla erotetut arvot) käsittelyyn kuuluu tekstidatan jäsentäminen ja tuottaminen riveihin ja sarakkeisiin järjestettynä, käyttäen pilkkuja erillisten arvojen erottamiseen. Ohjelmoijat usein harjoittavat tätä prosessia helpottaakseen datan vaihtoa eri sovellusten, tietokantojen välillä tai datan käsittely- ja analysointitehtäviin, johtuen CSV:n laajasta tuesta ja yksinkertaisuudesta.

## Miten:

Luassa CSV-tiedostojen käsittelyä voidaan lähestyä käyttämällä kielen tarjoamia perustiedoston IO-operaatioita, ilman tarvetta ulkoisille kirjastoille yksinkertaisissa tehtävissä. Monimutkaisemmissa operaatioissa, kuten erityistapausten (esim. pilkut arvojen sisällä) käsittelyssä, voi olla hyödyllistä käyttää kolmannen osapuolen kirjastoja, kuten `lua-csv`.

### CSV-tiedoston lukeminen
Tässä on yksinkertainen esimerkki CSV-tiedoston lukemisesta rivi riviltä, jakamalla jokainen rivi arvoiksi perustuen pilkkuerottimeen.

```lua
function parseCSVLine(line)
    local result = {}
    local from = 1
    local sep = ","
    local field
    while true do
        local start, finish = string.find(line, sep, from)
        if not start then
            table.insert(result, string.sub(line, from))
            break
        end
        field = string.sub(line, from, start - 1)
        table.insert(result, field)
        from = finish + 1
    end
    return result
end

local file = io.open("example.csv", "r")
for line in file:lines() do
    local values = parseCSVLine(line)
    for i, v in ipairs(values) do
        print(i, v)
    end
end
file:close()
```

**Esimerkkituloste** (`example.csv` sisällöllä "name,age\\newlineJohn Doe,30\\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### CSV-tiedoston kirjoittaminen
CSV-tiedoston luomiseen sinun tarvitsee vain rakentaa merkkijonot pilkuilla erotetuista arvoista ja kirjoittaa ne tiedostoon rivi riviltä.

```lua
local data = {
    {"name", "age"},
    {"John Doe", "30"},
    {"Jane Doe", "32"}
}

local file = io.open("output.csv", "w")
for _, v in ipairs(data) do
    file:write(table.concat(v, ","), "\n")
end
file:close()
```

Tämä luo (tai korvaa) `output.csv`-tiedoston määritetyillä tiedoilla.

### lua-csv:n käyttäminen
Edistyneemmässä CSV-käsittelyssä, mukaan lukien lainausmerkkien ja pakomerkkien tuki, `lua-csv`-kirjasto on jykevä valinta.

Asenna se ensin käyttäen LuaRocks:
```shell
luarocks install lua-csv
```

Tämän jälkeen CSV-tiedoston lukeminen on yksinkertaista:

```lua
local csv = require("csv")

-- Lukeminen tiedostosta
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

Ja kirjoittaminen CSV:hen asianmukaisilla lainausmerkeillä ja escape-merkeillä:

```lua
local file = csv.open("output.csv", {write=true})

local data = {
    {"name", "profession", "location"},
    {"John Doe", "Software Engineer", "New York, NY"},
    {"Jane Doe", "Data Scientist", "\"San Francisco, CA\""}
}

for _, v in ipairs(data) do
    file:write(v)
end
```

Tämä lähestymistapa käsittelee automaattisesti monimutkaisuudet, kuten pilkut ja lainausmerkit arvojen sisällä.
