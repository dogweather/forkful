---
title:                "Elm: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Att arbeta med YAML kan vara en nyttig färdighet att lägga till i ens programmeringsrepertoar. YAML är en formateringsspråk som används för att konfigurera och hantera data. Det är vanligtvis lättare att läsa och förstå än andra data-format och är därför ett populärt val för många utvecklare.

## Hur man gör

För att börja använda YAML i din Elm-kod, behöver du först installera ett paket som heter "elm-yaml". Sedan kan du använda funktioner som "encode" och "decode" för att konvertera YAML-data till och från Elm-objekt. Här är ett enkelt exempel:

```Elm 
import Yaml.Decode exposing (decodeString, int, string)

ymlContent = """
name: John Doe
age: 25
hobbies:
    - coding
    - hiking
"""

type alias Person = 
    { name : String
    , age : Int
    , hobbies : List String
    }

personDecoder : Yaml.Decoder Person
personDecoder =
    Yaml.key "name" string
        |> Yaml.andMap ( Yaml.key "age" int
            |> Yaml.andMap ( Yaml.key "hobbies" (Yaml.list string) 
                |> Yaml.map3 Person
            )
        )

person = decodeString personDecoder ymlContent -- Result Ok { name="John Doe", age=25, hobbies=["coding", "hiking"] }
```

Detta är bara ett enkelt exempel, men du kan använda YAML för att konfigurera mer komplexa datastrukturer och objekt i din Elm-kod.

## Djupdykning

En av de stora fördelarna med YAML är dess läsbarhet. Det är lätt att skriva och förstå även för personer som inte är vana vid programmering. Dessutom är det flexibelt och stöder många olika datatyper, inklusive listor, dictionaries och strängar.

Det finns också många tilläggspaket tillgängliga för YAML som kan hjälpa till att hantera mer komplex data. Till exempel kan du använda "elm-yaml-ast" för att generera AST (Abstract Syntax Tree) för din YAML-data, vilket kan vara användbart för mer avancerade manipuleringar eller transformer.

## Se även

- [elm-yaml-patch](https://github.com/panosoft/elm-yaml-patch)
- [elm-yaml-decode](https://github.com/andrewMacmurray/elm-yaml-decode)
- [elm-yaml-pretty](https://github.com/terezka/elm-yaml-pretty)