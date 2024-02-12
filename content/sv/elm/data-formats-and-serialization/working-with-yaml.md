---
title:                "Att Arbeta med YAML"
date:                  2024-02-03T19:25:17.166599-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att Arbeta med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Elm stöder inte YAML inbyggt, ett data serialiseringsformat som ofta används för konfigurationsfiler eller datadelning, på grund av dess starka betoning på typsäkerhet och förutsägbara resultat. Programmerare stöter dock ofta på YAML när de hanterar API:er eller konfigurationer inom webbutveckling, vilket kräver tillförlitliga metoder för att tolka YAML-data till Elms strikt typade ekosystem för sömlös integration och manipulation.

## Hur man gör:
För att hantera YAML i Elm behöver du vanligtvis konvertera YAML till JSON utanför Elm och sedan använda Elms inbyggda JSON-avkodningsfunktionalitet för att arbeta med datan. Även om denna metod kräver ett extra konverteringssteg, utnyttjar den Elms starka typsystem för att säkerställa dataintegritet. Populära verktyg för konvertering av YAML till JSON inkluderar onlinekonverterare eller backend-tjänster. När du har JSON kan du använda Elms `Json.Decode`-modul för att arbeta med datan.

Först, anta att du har följande YAML-data:

```yaml
person:
  name: Jane Doe
  age: 30
```

Konvertera det till JSON-format:

```json
{
  "person": {
    "name": "Jane Doe",
    "age": 30
  }
}
```

Definiera sedan din Elm-modell och avkodare:

```elm
module Main exposing (..)

import Html exposing (text)
import Json.Decode as Decode

type alias Person =
    { name : String
    , age : Int
    }

personDecoder : Decode.Decoder Person
personDecoder =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

```

För att använda denna avkodare för att konvertera JSON till en Elm-typ:

```elm
import Json.Decode as Decode

jsonString = 
    """
    {
      "person": {
        "name": "Jane Doe",
        "age": 30
      }
    }
    """

decodeResult = Decode.decodeString (Decode.field "person" personDecoder) jsonString

main =
    case decodeResult of
        Ok person ->
            Html.text ("Hej, " ++ person.name ++ "!")
            
        Err _ ->
            Html.text "Ett fel uppstod vid avkodningen."
```

Utdata (renderad i en Elm-applikation):
```
Hej, Jane Doe!
```

Denna metod säkerställer att du kan arbeta med YAML-data i Elm genom att använda JSON som ett intermediärt format, vilket utnyttjar Elms robusta typsystem och JSON-avkodningsförmåga för att säkert och effektivt hantera extern data.
