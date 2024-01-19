---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att göra en förfrågan till en server och validera din identitet med brukernamn och lösenord. Programmerare gör det för att få tillgång till skyddade resurser på servern.

## Hur fungerar det:
För att skicka en HTTP-förfrågan, använd `Http.post` eller `Http.get` och passera in URL:n och dina autentiseringsuppgifter.

```Elm
import Http
import Http.Basic

myRequest : Http.Request String
myRequest =
    Http.post
        { url = "https://example.com/login"
        , body = Http.stringBody "text/plain" ""
        , headers = [ Http.Basic.authentication "yourUsername" "yourPassword" ]
        , expect = Http.expectString (Result.Ok >> Ok)
        }
```
Resultatet blir antingen en `Ok String` med serverns svar, eller ett `Err Http.Error` om något går fel.

## Djupdykning
Historiskt sett har autentisering använts sedan början av datorsystem som ett sätt att skydda data. Grundläggande autentisering är en av de enklaste formerna och är baserad på HTTP-protokollet. Några alternativ till grundläggande autentisering inkluderar OAuth och Digest-autentisering. Implementeringen av HTTP-begäran i Elm görs via Http-paketet, vilket gör processen kodstorleksvänligt och mindre komplicerat jämfört med JavaScript.

## Se Även:
För mer information om att skicka HTTP-förfrågningar i Elm, se följande länkar:
- Elm Http-paket dokumentation: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Grundläggande autentisering på MDN: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- Alternativ till grundläggande autentisering: [https://oauth.net/](https://oauth.net/)