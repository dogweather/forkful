---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elm: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP-anrop med grundläggande autentisering kan vara användbart om du behöver skydda din applikations backend från obehörig åtkomst. Det kan också vara ett sätt att autentisera användare för att få åtkomst till specifika resurser.

## Hur

För att skicka en HTTP-anrop med grundläggande autentisering i Elm kan du använda `send` funktionen och ange autentiseringsuppgifterna i anropets header. Till exempel:

```
Elm.Http.send
    { method = "GET"
    , headers =
        [ ( "Authorization", "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==" )
        ]
    , body = Elm.Http.emptyBody
    , url = "http://example.com/api/users"
    , expect = Elm.Http.expectString YourDecoder
    }
```

I det här exemplet skickar vi ett GET-anrop till en API-rutt som kräver grundläggande autentisering. Autentiseringsuppgifterna är krypterade i base64-format och skickas via anropets header. Du kan också lägga till autentiseringsuppgifterna som en sträng i URL:en, men detta kan vara mindre säkert.

För att använda autentiseringsuppgifterna från ett input-fält kan du använda `toString` funktionen. Till exempel:

```
let
    authHeader =
        "Basic " ++ toString inputField
in
    Elm.Http.send
        { method = "GET"
        , headers = [ ( "Authorization", authHeader ) ]
        , body = Elm.Http.emptyBody
        , url = "http://example.com/api/users"
        , expect = Elm.Http.expectString YourDecoder
        }
```

## Djupdykning

Grundläggande autentisering är en enkel och vanlig metod för att skydda en webbtjänst. Den innebär att autentiseringsuppgifterna skickas i anropets header i klartext eller i base64-krypterad form. Detta gör att autentiseringen kan ske snabbt, men det finns också ett säkerhetsproblem med att skicka autentiseringsuppgifterna i klartext eftersom de kan bli stulna eller avlyssnade.

För ökad säkerhet bör du använda mer avancerade autentiseringsmetoder som OAuth eller JWT. Dessa metoder kräver en längre autentiseringsprocess men är mer säkra eftersom autentiseringsuppgifterna inte skickas i klartext.

## Se även

- [Elm dokumentation om HTTP-anrop](https://package.elm-lang.org/packages/elm/http/latest/)
- [Guide till grundläggande autentisering](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Elm community forum](https://discourse.elm-lang.org/)