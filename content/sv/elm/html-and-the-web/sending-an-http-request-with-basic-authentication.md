---
date: 2024-01-20 18:01:34.896501-07:00
description: "Att skicka en HTTP-beg\xE4ran med Basic-autentisering inneb\xE4r att\
  \ man legitimerar sig mot en server med anv\xE4ndarnamn och l\xF6senord kodat i\
  \ base64.\u2026"
lastmod: '2024-02-25T18:49:36.123484-07:00'
model: gpt-4-1106-preview
summary: "Att skicka en HTTP-beg\xE4ran med Basic-autentisering inneb\xE4r att man\
  \ legitimerar sig mot en server med anv\xE4ndarnamn och l\xF6senord kodat i base64.\u2026"
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med Basic-autentisering innebär att man legitimerar sig mot en server med användarnamn och lösenord kodat i base64. Programmerare gör detta för att säkerställa att endast auktoriserade användare kan få tillgång till vissa resurser på servern.

## Hur man gör:
```Elm
import Http
import Base64

type alias BasicAuth =
    { username : String
    , password : String
    }

addBasicAuthHeader : BasicAuth -> List Http.Header
addBasicAuthHeader creds =
    let
        encoded =
            Base64.encode (creds.username ++ ":" ++ creds.password)
    in
    [ Http.header "Authorization" ("Basic " ++ encoded) ]

exampleRequest : BasicAuth -> Http.Request String
exampleRequest creds =
    Http.request
        { method = "GET"
        , headers = addBasicAuthHeader creds
        , url = "https://example.com/protected"
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , tracker = Nothing
        }

-- För att använda, skicka en giltig BasicAuth och hantera svaret
```

## Fördjupning
I HTTP 1.0 introducerades Basic-autentisering som en enkel metod för att skydda webbresurser. Eftersom känslig information skickas öppet, förutsätter den att överföringen sker över en säker anslutning som HTTPS. Alternativ till Basic-autentisering inkluderar OAuth, API-nycklar och form-baserad autentisering. I Elm används modulen `Http` för att skapa begäran och `Base64` för att hantera kodningen. Viktigt är att aldrig skicka känslig information utan kryptering.

## Se även
- [Elm HTTP package documentation](https://package.elm-lang.org/packages/elm/http/latest/)
- [HTTP authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Base64 encoding in Elm](https://package.elm-lang.org/packages/truqu/elm-base64/latest/)
