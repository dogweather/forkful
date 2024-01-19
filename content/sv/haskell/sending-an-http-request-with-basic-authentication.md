---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att ett program framför en begäran till en server med användarnamn och lösenord. Programmerare gör det för att få åtkomst till skyddade resurser på en server.

## Hur man gör: 
Här är ett exempel på hur man använder HTTP-klientbiblioteket för att skicka en HTTP-begäran med grundläggande autentisering i Haskell.

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    let request = applyBasicAuth "username" "password"
                  $ defaultRequest
                  { method = "GET"
                  , host = "example.com"
                  }
    response <- httpLbs request manager
    print $ responseBody response
```
Första delen skapar en ny HTTP-manager med TLS-stöd (för HTTPS-förfrågningar). Sedan skapas en förfrågan med `applyBasicAuth` som tillämpar grundläggande autentisering, och `defaultRequest` sätter upp resten av förfrågan. Slutligen skickas förfrågan med `httpLbs`.

## Djupdykning
Att skicka HTTP-förfrågningar med grundläggande autentisering har använts länge och står som en standard i RFC 7617. Alternativ till grundläggande autentisering inkluderar Digest Access Authentication och mer moderna tekniker som OAuth.

Utförandet av en HTTP-begäran med grundläggande autentisering i Haskell innebär att användarnamn och lösenord kodas till Base64 och läggs till i `Authorization`-huvudet i HTTP-begäran.

## Se även
[Haskell HTTP klients officiella dokumentation](http://hackage.haskell.org/package/http-client-0.7.8/docs/Network-HTTP-Client.html)
[RFC 7617 - Basic Authentication Scheme](https://tools.ietf.org/html/rfc7617)