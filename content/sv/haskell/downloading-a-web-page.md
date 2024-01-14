---
title:                "Haskell: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Att hämta en webbsida är en vanlig uppgift för många programmerare. Det kan vara för att hämta data från en webbplats eller för att skapa en offlineversion av en webbplats. Genom att använda Haskell kan vi på ett enkelt sätt hämta en webbsida och arbeta med den data som vi får tillbaka.

## Hur man gör det

För att hämta en webbsida i Haskell behöver vi först importera paketet `Network.HTTP.Simple` i vår kod. Sedan kan vi använda funktionen `httpSink` för att hämta innehållet på en webbsida och spara det till en fil på vår dator. 

```Haskell
import Network.HTTP.Simple

main = do
  response <- httpSink "https://www.example.com" "example.html" -- hämtar innehållet på example.com och sparar det som example.html
  print "Webbsidans innehåll har hämtats och sparat!"
```

För att hämta innehållet på en specifik webbsida kan vi använda funktionen `httpBS` istället och sedan använda oss av funktioner från paketet `Data.ByteString` för att läsa innehållet och arbeta med det. 

```Haskell
import Network.HTTP.Simple
import qualified Data.ByteString as BS

main = do
  response <- httpBS "https://www.example.com" 
  let body = getResponseBody response -- hämtar innehållet och sparar det som en bytestring i variabeln body
  print body
```

## Djupdykning

För att förstå hur vi hämtar en webbsida i Haskell och vad funktionerna vi använder gör, måste vi först ha en grundläggande förståelse för HTTP-protokollet. HTTP eller Hypertext Transfer Protocol används för att överföra data över internet och är vad som gör det möjligt för oss att hämta webbsidor.

När vi hämtar en webbsida i Haskell använder vi oss av HTTP-metoderna `GET` eller `POST`, som talar om för servern på vilket sätt vi vill hämta eller skicka data. För att faktiskt skicka eller hämta data så använder vi oss av TCP/IP-protokollet, som gör det möjligt för oss att kommunicera över internet. 

Genom att använda paketet `Network.HTTP.Simple` i Haskell så abstraheras många av de detaljer som är relaterade till HTTP-protokollet och TCP/IP-protokollet bort åt oss, vilket gör det enklare att fokusera på att hämta och arbeta med webbsidans innehåll istället.

## Se även

- [Haskells dokumentation för Network.HTTP.Simple](https://hackage.haskell.org/package/http-client)
- [En guide för att arbeta med HTTP i Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20HTTP%20Examples)