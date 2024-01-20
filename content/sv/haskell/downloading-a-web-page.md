---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# En snabb introduktion till att ladda ner webbsidor med Haskell

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta HTML-koden som webbläsaren renderar till webbsidans visuella format. Programmerare gör detta ofta för att analysera webbplatsers innehåll eller skrapa data.

## Så här gör du:
Låt oss använda `http-conduit`-biblioteket i Haskell för att ladda ner en webbsida. Installera biblioteket med kommandot:
```haskell
cabal install http-conduit
```
och här är ett enkelt exempel på hur man använder det:
```haskell
import Network.HTTP.Conduit (simpleHttp)

main = do
    body <- simpleHttp "http://example.com"
    putStrLn $ take 100 body
```
När du kör det här programmet laddar det ner webbsidan `http://example.com` och skriver ut de första 100 tecknen av dess innehåll.

## Fördjupning
Att ladda ner webbsidor är något som programmerare har behövt göra sedan webbens födelse. Ett flertal bibliotek och verktyg finns tillgängliga för Haskell, till exempel `http-client`, `http-streams` och `Wreq`. 

Det historiska kontextet för detta gäller att förstå hur HTTP-protokollet fungerar, vilket är den underliggande tekniken för hur webbläsare laddar ner webbsidor. Det är en bra idé att ha en viss förståelse för HTTP när du utför den här typen av programmering.

När det kommer till implementering, ger `http-conduit`-biblioteket oss en högnivå-API för att göra HTTP-förfrågningar. Funktionen `simpleHttp` tar en URL som sträng och returnerar en `IO ByteString` som innehåller webbsidans innehåll. Vi använder `putStrLn`-funktionen för att skriva ut det till konsolen.

## Referenser
För mer information om ämnet, se följande källor:

1. [Haskell http-conduit bibliotek](https://hackage.haskell.org/package/http-conduit)
2. [HTTP protokollet](https://sv.wikipedia.org/wiki/Hypertext_Transfer_Protocol)