---
title:                "YAML-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"

category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä & Miksi? YAML (YAML Ain't Markup Language) on data-sarjojen kuvaamiseen käytetty kieli, suosittu konfiguraatioissa ja datan tallennuksessa. Ohjelmoijat käyttävät YAMLia, koska se on helppolukuinen ja -muokattava.

## How to:
Koodiesimerkit:
```Elm
-- Elm:ssä ei ole sisäänrakennettua tukea YAML:lle, mutta voit muuntaa YAML JSON:ksi ja käyttää sitä siten.

-- Otaksutaan että sinulla on YAML-string, joka on muunnettuna JSON:ksi:
-- yamlString on YAML-muotoisen konfiguraation JSON-stringi

import Json.Decode as Decode

type alias Config =
    { name : String
    , age : Int
    }

-- Oletetaan että sinulla on JSON-dekooderi Config-tyyppiä varten
configDecoder : Decode.Decoder Config
configDecoder =
    Decode.map2 Config
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

-- Funktion parseYAML, joka käsittelee YAML-jsonStringin
parseYAML : String -> Result String Config
parseYAML jsonString =
    Decode.decodeString configDecoder jsonString

-- Esimerkin käyttö:
-- yamlJsonString vastaa muunnettua YAML:ia

yamlJsonString : String
yamlJsonString = 
    """
    {
        "name": "Esa",
        "age": 35
    }
    """

-- Kutsutaan parseYAML-funktiota ja tulostetaan tulos
case parseYAML yamlJsonString of
    Ok config ->
        -- Käsittele Config objekti
        String.fromInt config.age

    Err error ->
        -- Virheenkäsittely
        "Parse error"
```

Esimerkituloste:
```
35
```

## Deep Dive
Syväsukellus: YAML luotiin 2001, helpottamaan datan käsittelyä joustavammalla ja helpommin luettavammalla tavalla kuin JSON. Vaihtoehtoina YAML:lle ovat mm. JSON ja XML. Elm:ssä YAMLia käytetään harvoin suoraan, ja tyypillisesti se muunnetaan ensin JSON-muotoon.

## See Also
Katso Myös:
- YAML: [YAML viralliset sivut](https://yaml.org)
- JSON: [Elm JSON-dekoodausdokumentaatio](https://package.elm-lang.org/packages/elm/json/latest/)
- YAMLin ja JSONin muunnos työkalut: [Convert YAML to JSON](https://www.convertjson.com/yaml-to-json.htm)
