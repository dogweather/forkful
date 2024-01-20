---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-request er en måte for et program å hente eller sende data til en server. Dette gjør at programmerere kan utveksle data mellij plattformer, servere og klienter.

## Hvordan gjør man det:
Her er et enkelt eksempel på en GET-request i Elm:

```Elm
import Http
import Json.Decode as Decode

fetchData : Cmd Msg
fetchData =
  Http.get
    { url = "https://example.com/api/data"
    , expect = Http.expectJson GotData (Decode.list decodeDataItem)
    }

type Msg
  = GotData (Result Http.Error (List DataItem))

type alias DataItem =
  { id : Int
  , name : String
  }

decodeDataItem : Decode.Decoder DataItem
decodeDataItem =
  Decode.map2 DataItem
    (Decode.field "id" Decode.int)
    (Decode.field "name" Decode.string)
```
Når du kjører `fetchData`, vil Elm sende en GET-request til "https://example.com/api/data", og forventer et JSON-svar som kan dekodes til en liste av `DataItem`.

## Dypdykk
Å sende en HTTP-request er fundamental del for å bygge nettapplikasjoner, og har vært brukt siden de tidlige dagene av nettet. Alternativt til Elm's http-pakke, finnes det også andre biblioteker som serverless-http og elm-http-extra med flere funksjoner.

Elm bruker en "pure function" tilnærming til å sende HTTP-requests, noe som i stor grad forbedrer testbarheten og forutsigbarheten til nettapplikasjoner. Det er også viktig å merke seg at Elm har innebygget støtte for JSON-dekoding, noe som gjør det lettere å jobbe med API-responser.

## Se også
- Elm’s offisielle dokumentasjon på Http - https://package.elm-lang.org/packages/elm/http/latest/
- En tutorial for HTTP-requests i Elm - https://guide.elm-lang.org/effects/http.html
- Elm’s Json.Decode dokumentasjon - https://package.elm-lang.org/packages/elm/json/latest/Json-Decode