---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:04.386386-07:00
description: "YAML, een voor mensen begrijpelijke data-serialisatiestandaard, wordt\
  \ gebruikt voor configuratiebestanden en gegevensuitwisseling. Programmeurs zijn\
  \ er\u2026"
lastmod: '2024-02-25T18:49:48.085394-07:00'
model: gpt-4-0125-preview
summary: "YAML, een voor mensen begrijpelijke data-serialisatiestandaard, wordt gebruikt\
  \ voor configuratiebestanden en gegevensuitwisseling. Programmeurs zijn er\u2026"
title: Werken met YAML
---

{{< edit_this_page >}}

## Wat & Waarom?

YAML, een voor mensen begrijpelijke data-serialisatiestandaard, wordt gebruikt voor configuratiebestanden en gegevensuitwisseling. Programmeurs zijn er dol op omdat het duidelijk, gemakkelijk te lezen en breed geadopteerd is over gereedschappen en talen.

## Hoe te:

Elm heeft geen ingebouwde YAML-parser, dus meestal zet je YAML om naar JSON met een extern hulpprogramma en werk je vervolgens in Elm met de `elm/json` bibliotheek.

```elm
import Json.Decode exposing (Decoder, field, string, int, decodeValue)

type alias Gebruiker =
    { naam : String
    , leeftijd : Int
    }

gebruikerDecoder : Decoder Gebruiker
gebruikerDecoder =
    Json.Decode.map2 Gebruiker
        (field "naam" string)
        (field "leeftijd" int)

jsonString : String
jsonString =
    """
    {
        "naam": "Jane Doe",
        "leeftijd": 25
    }
    """

parseResultaat : Result String Gebruiker
parseResultaat =
    jsonString
        |> Json.Decode.decodeString gebruikerDecoder

-- Voorbeeld output: Result.Ok { naam = "Jane Doe", leeftijd = 25 }
```
Elm code verwerkt JSON, het equivalent van je YAML na omzetting.

## Diepere Duik:

De eenvoud van YAML gaat terug naar het begin van de jaren 2000 als een voor mensen leesbaar alternatief voor XML. Hoewel Elm YAML niet native verwerkt, is werken met JSON een makkie, dankzij `elm/json`. Sommige mensen gebruiken services van derden of tools zoals `yaml-to-json.com` of schrijven zelfs een beetje server-zijde code in Node.js of Python om de YAML-naar-JSON dans te doen. Onthoud, Elm blinkt uit met JSON, dus deze tweestapsconversie is de workaround die de Elm-community over het algemeen gebruikt.

## Zie Ook:

- Elm JSON-pakket: https://package.elm-lang.org/packages/elm/json/latest/
- Online YAML naar JSON converter: https://yaml-to-json.com/
- JSON-naar-Elm type generator: https://noredink.github.io/json-to-elm/
