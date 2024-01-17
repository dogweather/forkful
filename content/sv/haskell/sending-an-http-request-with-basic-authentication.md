---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Haskell: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran med grundläggande autentisering är en vanlig metod för programmerare att verifiera användaridentiteten vid dataöverföring över nätverket. Detta gör det möjligt för en server att kontrollera att begäran kommer från en giltig användare, vilket är en viktig del av säkerhet och integritet för många webbapplikationer.

## Hur man gör:

```Haskell
-- Importera nödvändiga bibliotek
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization)

-- Ange autentiseringsinformation: användarnamn och lösenord
let username = "användarnamn"
let password = "lösenord"

-- Bygg en HTTP-begäran och inkludera autentiseringsinformation i en Authorization-header
request <- parseRequest "http://exempel.com/apicall"
let authenticatedRequest = applyBasicAuth username password request
let authHeader = (hAuthorization, "Basic " ++ encodeUtf8 (username ++ ":" ++ password))
let requestWithAuthHeader = setRequestHeaders [authHeader] authenticatedRequest

-- Skicka begäran och spara svar
response <- httpLBS requestWithAuthHeader
let responseBody = getResponseBody response

-- Utvärdera resultatet
putStrLn $ "Servern svarade med: " ++ responseBody
```

Exempeloutput skulle kunna se ut så här:

```
Servern svarade med: {"id": 1234, "name": "John Doe"}
```

## Utbildning:

Basic authentication är en av de äldsta autentiseringsmetoderna som används för webbapplikationer. Det går tillbaka till HTTP-protokollets begynnelse och ansågs länge vara tillräckligt säkert för att skydda användaridentiteten. Med tiden har fler avancerade metoder utvecklats, som OAuth och API-nycklar, vilka erbjuder mer kontroll och flexibilitet för autentisering av användare.

För att implementera basic authentication i en webbapplikation måste en server ha stöd för denna typ av autentisering och klienten måste kunna skicka begäran med den nödvändiga Authorization-headern.

## Se även:

För mer information om hur HTTP-begäran fungerar och andra autentiseringsmetoder, kolla in följande resurser:

- [Haskell HTTP-paketet](https://hackage.haskell.org/package/http)
- [HTTP-begäran Wikipedia-sida](https://sv.wikipedia.org/wiki/HTTP-begäran)
- [OAuth autentiseringsflöde](https://oauth.net/2/)