---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
aliases:
- /sv/haskell/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:50.047790-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att du inkluderar användarnamn och lösenord i förfrågningen för att få tillgång till skyddad data. Programmerare gör detta för att interagera med API:er som kräver enkel men säker användarverifiering.

## Hur man gör:
Här är ett exempel i Haskell med `http-client` och `base64-bytestring` för att skicka en HTTP GET-förfrågan med grundläggande autentisering:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Types.Header (hAuthorization)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (pack, append)

-- Konfigurera HTTP-manager
manager <- newManager defaultManagerSettings

-- Bygg URL och autentiseringsuppgifter
let url = "https://example.com/api/data"
let credentials = "användarnamn:lösenord"
let authHeader = "Basic " `append` (encode . pack $ credentials)

-- Skapa förfrågan och inkludera autentiseringsheader
request <- parseRequest url
let requestWithAuth = request { requestHeaders = [(hAuthorization, authHeader)] }

-- Skicka förfrågan och få svar
response <- httpLbs requestWithAuth manager

-- Skriv ut svaret
putStrLn $ show (responseBody response)
```

I det här exemplet ska du ersätta `"användarnamn:lösenord"` med dina faktiska autentiseringsuppgifter. Efter att ha kört koden bör du se svaret från servern. Kom ihåg att hålla dina autentiseringsuppgifter säkra!

## Fördjupning
Grundläggande autentisering är en del av HTTP-protokollet sedan det skapades på 90-talet. Det är inte den säkraste metoden men det är lätt att implementera och fungerar därför bra för enklare use cases eller interna nätverk.

Alternativ till grundläggande autentisering inkluderar OAuth och API-nycklar, som båda erbjuder högre säkerhetsnivåer. I Haskell kan du använda bibliotek som `wreq` eller `http-conduit` för flexibilitet och fler autentiseringsmetoder.

En viktig detalj när du använder grundläggande autentisering är att alltid använda HTTPS för att förhindra att din trafik avlyssnas. Att skicka autentiseringsuppgifter okrypterat över HTTP är en stor säkerhetsrisk.

## Se Även
- HTTP-client dokumentation: https://hackage.haskell.org/package/http-client
- Base64-bytestring dokumentation: https://hackage.haskell.org/package/base64-bytestring
- Hur man arbetar säkert med HTTP i Haskell: https://www.yesodweb.com/book/http-client
