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

## Vad & Varför?

Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att vi skickar inloggningsuppgifter tillsammans med vår förfrågan för att få åtkomst till en resurs på webben. Detta görs av programmerare för att säkerställa att endast auktoriserade användare kan använda vissa funktioner eller se viss information.

## Hur man:

```
Elm.Http.send
  { verb = "GET"
  , headers = [("Authorization", "Basic ZnJlZDpzYW1wbGU=")]
  , url = "http://example.com/resource"
  , body = Http.emptyBody
  , expect = Http.expectString (Http.expectOk receiveResponse)
  }
```
Detta är ett exempel på hur man skickar en GET-förfrågan med grundläggande autentisering i Elm. Vi anger http-metoden `verb` och lägger till en `Authorization`-huvud- rad med våra inloggningsuppgifter i base64-kodning. Slutligen anger vi url och förväntade resultat.

```
Elm.Http.send
  { verb = "POST"
  , headers = [("Authorization", "Basic dXNlcjpwYXNzd29yZA==")]
  , url = "http://example.com/login"
  , body = Http.stringBody "{\"username\": \"user\", \"password\": \"pass\"}"
  , expect = Http.expectString receiveResponse
  }
```
Här är ett exempel på hur man skickar en POST-förfrågan med grundläggande autentisering. Denna gång anger vi också en `body` med vår användarnamn och lösenord i JSON-format.

## Deep Dive

HTTP-grundläggande autentisering är en av många autentiseringstyper som stöds av protokollet. Det infördes i HTTP-specifikationen år 1999 och har sedan dess använts som ett enkelt sätt att skydda resurser på webben. Alternativet till grundläggande autentisering är digest autentisering, som erbjuder bättre säkerhet genom att inte skicka lösenord i klartext, men är också mer krävande att implementera.

Det finns många sätt att implementera grundläggande autentisering i Elm, men det kan vara enklare att använda ett ramverk som [elm-http-builder](https://github.com/lucamug/elm-http-builder) eller [elm-url-builder](https://github.com/lucamug/elm-url-builder), som gör det möjligt att bygga HTTP-förfrågningar på ett mer modulärt sätt.

## Se även:

- [HTTP Basic Authentication i Elm](https://package.elm-lang.org/packages/elm/http/latest/Http-Basic)
- [HTTP Digest Authentication i Elm](https://package.elm-lang.org/packages/elm/http/latest/Http-Digest)