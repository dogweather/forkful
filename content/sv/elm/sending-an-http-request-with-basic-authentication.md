---
title:                "Elm: Sända en http-förfrågan med grundläggande autentisering"
simple_title:         "Sända en http-förfrågan med grundläggande autentisering"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför
Att skicka HTTP-förfrågningar med grundläggande autentisering kan vara en viktig del av att skapa en interaktiv och säker webbapplikation.

## Så här gör du
För att skicka en HTTP-förfrågan med grundläggande autentisering i Elm, behöver du först importera HTTP-paketet och måste sedan skapa en `Request`-värde med autentiseringsinformation. Se kodexemplet nedan för att se hur detta kan implementeras i din Elm-applikation.

```Elm
import Http
import Json.Decode as Json

requestUrl : String
requestUrl = "https://example.com/api"

username : String
username = "user123"

password : String
password = "secret"

-- Skapa en `Request`-värde med grundläggande autentiseringsinformation
request : Http.Request
request =
    Http.post
        requestUrl
        (Http.jsonBody Json.null)
        |> Http.withCredentials username password

-- Skicka förfrågan och hantera svar
Http.send handleResponse request

-- Definiera en funktion för att hantera HTTP-svar
handleResponse : Http.Response a -> Cmd msg
handleResponse response =
    case response of
        -- Om förfrågan lyckas, hantera svaret här
        Http.Ok _ ->
            -- `response.body` innehåller svarsdatan
            Cmd.none
        -- Om förfrågan misslyckas, hantera eventuella fel här
        Http.Err error ->
            -- `error` innehåller en beskrivning av felet
            Cmd.none

```

### Utskrift
Om förfrågan lyckas, kommer svaret att skrivas ut i konsolen. Ta en titt på den här enkla utmatningen från vår kod:
```
> Http.Ok "Success!"
```

## Djupdykning
För att använda grundläggande autentisering i Elm, måste du förstå hur HTTP-autentisering fungerar. Grundläggande autentisering innebär att en användaresnamn och lösenord skickas som en del av en HTTP-förfrågan. Detta är en enkel autentiseringsmetod men är inte särskilt säker eftersom användarnamn och lösenord skickas i klartext.

Det är också viktigt att notera att grundläggande autentisering bara är en av flera autentiseringsmetoder som stöds av HTTP-protokollet. Om du vill lära dig mer om autentisering och hur det kan implementeras i Elm, rekommenderar vi att du läser mer om HTTP-autentisering och det officiella dokumentationen för Elm HTTP-paketet.

## Se även
- [HTTP-paketet i Elm](https://package.elm-lang.org/packages/elm/http/latest)
- [Officiella Elm-dokumentationen för HTTP-autentisering](https://guide.elm-lang.org/effects/http.html)