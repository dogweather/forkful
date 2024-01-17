---
title:                "Töitä tehdään yamlin kanssa"
html_title:           "Haskell: Töitä tehdään yamlin kanssa"
simple_title:         "Töitä tehdään yamlin kanssa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

Versio 1.0 - Työskentely YAML:in kanssa

## Mikä & Miksi?
YAML (Yet Another Markup Language) on tiedostomuoto, jota käytetään datan tallentamiseen ja jakamiseen erilaisten ohjelmien välillä. Ohjelmoijat käyttävät YAML:ia koska se on yksinkertainen ja helposti luettavissa oleva formaatti, joka helpottaa datan käsittelyä ja jakamista.

## Kuinka?
Haskellin avulla voit lukea ja kirjoittaa YAML-tiedostoja käyttämällä Data.Yaml kirjastoa. Alla on esimerkki, joka lukee YAML-tiedoston ja tulostaa sen sisällön konsoliin:

```Haskell
import Data.Yaml

main = do
  yaml <- readFile "tiedosto.yaml"
  print yaml
```

Tämän koodin suorittaminen tulostaa tiedoston sisällön muodossa, joka on helppo lukea ja muokata.

## Syväsukellus
YAML kehitettiin vuonna 2001 ja sen tarkoituksena oli korvata XML:ää yksinkertaisemmalla ja helpommin luettavalla formaatilla. Muihin vaihtoehtoihin kuuluvat JSON ja TOML, mutta YAML on edelleen suosittu ohjelmoijien keskuudessa sen helposti luettavan rakenteen takia.

Haskellissa Data.Yaml kirjasto käyttää Aeson-kirjastoa JSON-tietojen serialisointiin ja deserialisointiin. Tämä mahdollistaa YAML-tiedoston muuntamisen JSON-muotoon ja päinvastoin.

## Katso myös
- [Data.Yaml-haskellkirjaston dokumentaatio](https://hackage.haskell.org/package/yaml)
- [YAML-spesifikaatio](https://yaml.org/spec/)
- [Aeson-haskellkirjasto](https://hackage.haskell.org/package/aeson)