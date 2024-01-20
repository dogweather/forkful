---
title:                "Arbete med YAML"
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett format för datastrukturer, tänkt för konfigurationsfiler. Programmerare använder det för dess läsbarhet och enkelhet att parsa till olika datatyper.

## Hur gör man:
Elm har inget inbyggt stöd för YAML, så vi använder `elm-yaml` paketet. Installera genom `elm install elm/json` följt av `elm install kraklin/elm-yaml`. Här är ett exempel:

```Elm
import Json.Decode exposing (Decoder)
import Yaml.Decode exposing (yamlString, string, int, dict, decodeValue)

type alias Person =
    { name : String
    , age : Int
    }

personDecoder : Decoder Person
personDecoder =
    Yaml.Decode.map2 Person
        (dict "name" string)
        (dict "age" int)

sampleYaml : String
sampleYaml =
    """
    name: John Doe
    age: 30
    """

parseResult : Result String Person
parseResult =
    sampleYaml
        |> yamlString
        |> decodeValue personDecoder
```

Du parsar `sampleYaml` och får antingen ett felmeddelande eller ett `Person` objekt.

## Fördjupning
YAML, "YAML Ain't Markup Language", lanserades i början av 2000-talet som ett enklare alternativ till XML. JSON är också ett alternativ men YAML's mer läsbara format är ofta att föredra för konfigurationsfiler. I Elm implementeras YAML-parsing genom externa bibliotek som `elm-yaml`, som bygger på att omvandla YAML till JSON för sedan använda Elms kraftfulla JSON dekodare.

## Se även
- YAML specifikation: [yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- En jämförelse mellan JSON och YAML: [stackoverflow.com/a/1729545/3047276](https://stackoverflow.com/a/1729545/3047276)