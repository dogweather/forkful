---
title:                "Hämta en webbsida"
date:                  2024-01-20T17:44:09.685386-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta dess HTML-kod från servern till din dator. Programmerare gör detta för att analysera innehållet, testa servrar eller automatisera datainsamling.

## Hur man gör:
För att ladda ner en webbsida i Haskell kan vi använda `http-conduit`-paketen. Se till att du har GHC installerad och att du har konfigurerat ett nytt stack-projekt. Lägg till `http-conduit` i dina `build-depends` i `package.yaml` eller ditt `.cabal`-fil.

```Haskell
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://example.com"
    let body = getResponseBody response
    LBS.putStr body
```

Kör ditt program och du borde se HTML-koden för "http://example.com" skrivas ut i terminalen.

## Fördjupning:
När Tim Berners-Lee uppfann webben på 1990-talet, blev nedladdning av webbsidor grundläggande för att navigera på internet. I Haskell finns det flera bibliotek för att göra detta men `http-conduit` är populär tack vare sin enkelhet och effektivitet. Det använder sig av conduits för att hantera dataströmmar, vilket hjälper till att hantera stora responsstorlekar.

Som alternativ till `http-conduit` kan du använda `wget` eller `curl` i ett shell-skript eller via `System.Process` i Haskell. För ännu enklare fall där felhantering och prestanda inte är lika kritiska, kan även `Network.HTTP.simpleHTTP` och `urlDownloadToFile` vara tillräckliga.

När det gäller implementeringsdetaljer, så hanterar `http-conduit` detaljer som omdirigeringar och skapar `Request`-objekt för dig. All data som mottas är i `ByteString`, och du kan använda funktioner från `Data.ByteString.Lazy` för att hantera den.

## Se även:
- The `http-conduit` package on Hackage: https://hackage.haskell.org/package/http-conduit
- Haskell documentation for `http-conduit`: http://hackage.haskell.org/package/http-conduit-2.3.7.3/docs/Network-HTTP-Simple.html
- Real World Haskell (bok) som går igenom nätverksprogrammering: http://book.realworldhaskell.org/
- Haskell Wiki för nätverksrelaterade ämnen: https://wiki.haskell.org/Network_programming