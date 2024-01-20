---
title:                "Skicka en http-förfrågan"
html_title:           "Javascript: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-förfrågan innebär att sända data till en server över internet genom HTTP(s)-protokollet. Programmerare gör det för att hämta data från en server, skicka data till en server, eller uppdatera data på en server.

## Hur man gör

Här är ett enkelt exempel på hur man skickar en HTTP GET-förfrågan i Elm:

```Elm
import Http
import Json.Decode as Decode

getUsers = 
    Http.get
        { url = "https://jsonplaceholder.typicode.com/users"
        , expect = Http.expectJson Decode.list
        }

main : Program () HttpData Msg
main =
    Browser.document
        { init = always ( initialModel, getUsers )
        , view = Json.Encode.string << toString
        , update = \response model -> ( { model | data = response.body }, Cmd.none )
        , subscriptions = always Sub.none
        }
```

I ovanstående exempel sänder vi en HTTP-förfrågan till url:en "https://jsonplaceholder.typicode.com/users" och förväntar oss ett svar i JSON-format.

## Djupdykning

Historiskt sett, före HTTP-protokollets införande, kommunicerade nätverksprogram direkt med varandra genom deras IP-adresser. Med HTTP, kan program kommunicerar med servrar genom URL:er, vilket är lättare att använda och mer skalbart.

Ett alternativ till att använda `Http.get` för att skicka HTTP-förfrågningar i Elm är att använda `Http.request` vilket ger mer flexibilitet men är också mer komplicerat att använda.

En viktig detalj att notera är att `Http.get` i Elm returnerar en `Cmd Msg`, ingen datatyp som `Result` eller `Maybe`. Elm-coden körs i webbläsaren och använder Elm:s runtime-system för att hantera IO, inklusive HTTP-förfrågningar.

## Se även

För mer information, se följande källor:

1. Elm:s officiella dokumentation om HTTP: https://package.elm-lang.org/packages/elm/http/latest/
2. En bra artikel om att hantera HTTP-förfrågningar i Elm: https://guide.elm-lang.org/effects/http.html
3. En djupare diskussion om IO hantering i Elm: https://elmprogramming.com/side-effects.html