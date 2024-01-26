---
title:                "YAML-tiedostojen käsittely"
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML on datan serialisointiformaatti, joka on helppolukuinen ihmisille. Ohjelmoijat käyttävät YAMLia konfiguraatioiden ja tietojen tallentamiseen ja jakamiseen, koska se on yksinkertainen ja laajalti tuettu.

## How to:
Haskellissa YAML-käsittelyyn suosittuja kirjastoja ovat `yaml` ja `aeson-yaml`. Tässä esimerkissä käytämme `yaml`:

```Haskell
import Data.Yaml

main :: IO ()
main = do
  yamlData <- decodeFileEither "example.yaml" :: IO (Either ParseException Value)
  case yamlData of
    Left err -> putStrLn $ "Error parsing YAML: " ++ show err
    Right val -> print val
```

Esimerkkitiedoston `example.yaml` sisältö:
```
name: Esimerkki
age: 30
```

Tuloste:
```
Object (fromList [("name", String "Esimerkki"), ("age", Number 30.0)])
```

## Deep Dive
YAML (YAML Ain't Markup Language) luotiin alun perin vuonna 2001. Se on suunnattu tiedon tallentamiseen ja vaihtoon ja on tunnettu yksinkertaisuudestaan verrattuna XML:ään. Haskell-ohjelmoijien suosiossa on `yaml`, koska se mahdollistaa helpot integraatiot muihin kirjastoihin, kuten `aeson`, jota käytetään JSONin käsittelyyn. Vaikka JSON onkin toinen suosittu formaatti, YAML on suosittu konfiguroinneissa sen paremman luettavuuden ansiosta.

## See Also
Lisätietoa ja resursseja:
- YAML spesifikaatio: https://yaml.org/spec/1.2/spec.html
- Haskell `yaml` kirjaston dokumentaatio: https://hackage.haskell.org/package/yaml
- `aeson-yaml` pakkauksen dokumentaatio: https://hackage.haskell.org/package/aeson-yaml
- YAML vs. JSON vertailu: https://phoenixnap.com/kb/yaml-vs-json-vs-xml
