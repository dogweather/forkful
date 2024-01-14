---
title:                "Haskell: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

I den här bloggposten kommer vi att gå igenom hur man skickar en HTTP-begäran med grundläggande autentisering i Haskell. Detta är en användbar färdighet för dig som vill skapa säkra och autentiserade nätverksanrop i dina Haskell-program.

## Så här gör du

För att skicka en HTTP-begäran med grundläggande autentisering behöver vi först importera "Network.HTTP.Simple" biblioteket i Haskell. Detta bibliotek innehåller funktioner för att hantera HTTP-anrop på ett enkelt sätt.

Efter att ha importerat biblioteket, behöver vi skapa en "Request" med den URL som vi vill skicka begäran till. Vi behöver också lägga till en "Authorization" header med vårt användarnamn och lösenord i bas64-encoderad form. Därefter kan vi använda "httpLBS" funktionen för att skicka begäran och få svar som en "Response" variabel.

```Haskell
import Network.HTTP.Simple

main = do
  let url = "https://example.com/api" -- Lägg till den faktiska URL:en här
      user = "användarnamn"
      password = "lösenord"
  
  request <- parseRequest url
  let auth = "Basic " ++ (encodeBase64 (user ++ ":" ++ password))
  let request' = addRequestHeader "Authorization" auth request
  
  response <- httpLBS request'
  print $ getResponseBody response
```

## Djupdykning

Nu när vi har en grundläggande förståelse för hur man skickar en HTTP-begäran med grundläggande autentisering i Haskell, låt oss titta närmare på koden.

Först ser vi att vi använder "Network.HTTP.Simple" biblioteket för att importera funktionerna vi behöver. Huvudfunktionen, "httpLBS", hanterar både anropet och svar från servern.

Sedan skapar vi en "Request" variabel med hjälp av "parseRequest" funktionen. Vi lägger också till en "Authorization" header genom att använda "addRequestHeader" funktionen och konkatenera bas64-kodad användarnamn och lösenord.

Efter att ha fått tillbaka den berikade "Request" variabeln, använder vi "httpLBS" igen för att skicka begäran och spara svaret i en "Response" variabel. Slutligen använder vi "getResponseBody" funktionen för att få tillbaka data från servern och skriver ut det.

## Se även

- [Haskell.org - HTTP-request dokumentation](https://www.haskell.org/haskellwiki/HTTP)
- [GitHub - Network.HTTP.Simple](https://github.com/snoyberg/http-client/blob/master/http-client-tls/src/Network/HTTP/Simple.hs)
- [Bas64-enkodning av användarnamn och lösenord](https://www.base64encode.org/)