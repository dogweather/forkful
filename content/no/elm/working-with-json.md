---
title:                "Arbeid med JSON"
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
JSON håndtering er essensielt for å utveksle data på webben. Utviklere bruker det for letthet ved å sende og motta strukturert informasjon mellom klient og server. 

## How to:
For å jobbe med JSON i Elm, bruk modulene `Json.Decode` og `Json.Encode`. La oss se på grunnleggende eksempler:

Decoder for enkel streng:
```Elm
import Json.Decode as Decode

simpleStringDecoder : Decode.Decoder String
simpleStringDecoder =
    Decode.string

-- Eksempel på Output:
-- Result.Ok "Hei verden!"
```

Encoder for enkel streng:
```Elm
import Json.Encode as Encode

simpleStringEncoder : String -> Encode.Value
simpleStringEncoder string =
    Encode.string string

-- Eksempel på Output:
-- "Hei verden!"
```

## Deep Dive
Elm gir en trygg måte å jobbe med JSON på. Historisk sett har språk som JavaScript vært mer uforutsigbare med JSON, noe som Elm unngår med sitt sterke typesystem. Alternativer inkluderer dynamisk håndtering via flags eller server-side-decoding. Detaljer som type inference og error handling i Elm gir robuste JSON implementasjoner.

## See Also
- Elm's officielle JSON guide: https://guide.elm-lang.org/interop/json.html
- `Json.Decode` dokumentasjon: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode
- `Json.Encode` dokumentasjon: https://package.elm-lang.org/packages/elm/json/latest/Json-Encode
