---
title:                "Haskell: Sändning av en http-begäran"
simple_title:         "Sändning av en http-begäran"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka HTTP-förfrågningar är en viktig del av många moderna applikationer eftersom det tillåter kommunikation över nätverket. Genom att förstå hur man kan skicka dessa förfrågningar i Haskell kan du skapa kraftfulla och flexibla program som kan ansluta till externa resurser.

## Hur Man Gör

Det finns flera bibliotek tillgängliga för att skicka HTTP-förfrågningar i Haskell, men ett av de mest populära är `http-client`. Först måste vi importera detta bibliotek i vår kod:

```Haskell
import Network.HTTP.Client
```

Sedan kan vi skapa en ny `Manager` som tillhandahåller en HTTP-klient:

```Haskell
manager <- newManager defaultManagerSettings
```

För att skicka en GET-förfrågning till en specifik URL, kan vi använda funktionen `httpLbs`:

```Haskell 
response <- httpLbs "http://example.com" manager
```

Denna funktion returnerar både statuskoden och response body från förfrågningen. För att bara få statuskoden, kan vi använda `httpStatus` istället:

```Haskell
status <- httpStatus "http://example.com" manager
```

## Djupdykning

Förutom att skicka GET-förfrågningar, kan vi också skicka POST-förfrågningar med `httpLbs`. Om vi vill ange ett anpassat request body eller lägga till header-fält, kan vi använda `request` och `setRequestBodyLBS` respektive. Här är ett exempel på hur man skickar en POST-förfrågan med ett anpassat body och headers:

```Haskell
import qualified Data.ByteString.Lazy as LBS

body = "Hello from Haskell!"
headers = [("Content-Type", "text/plain"), ("API-Key", "secretkey")]

request = setRequestBodyLBS body $ setRequestHeaders headers $ "http://example-api.com"

response <- httpLbs request manager
```

## Se Även

- [Haskell Wiki: HTTP](https://wiki.haskell.org/HTTP)
- [Learn You a Haskell for Great Good: Input och Output](http://learnyouahaskell.com/input-and-output#simple-get-requests)
- [HTTP-klient på Hackage](https://hackage.haskell.org/package/http-client)