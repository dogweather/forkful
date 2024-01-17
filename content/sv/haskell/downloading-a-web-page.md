---
title:                "Ladda ner en webbsida"
html_title:           "Haskell: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida innebär att man hämtar en kopia av en webbsida från internet till sin egna dator. Detta kan göras av olika skäl, till exempel för att kunna arbeta offline eller analysera webbsidans struktur och innehåll.

## Så här gör du:

```Haskell 
import Network.HTTP.Simple 
main = do 
    response <- httpGet "https://example.com" 
    print $ getResponseBody response 

-- Output: a complete copy of the webpage in question 
```

## Deep Dive:

Att ladda ner en webbsida är ett vanligt behov hos många programmerare, särskilt de som arbetar med webbutveckling eller dataanalys. Det finns flera alternativ för att hämta en webbsida, inklusive direkta anrop till webbservern eller användning av specialiserade bibliotek. I Haskell är användning av Network.HTTP.Simple ett enkelt och smidigt sätt att hämta en webbsida, vilket illustreras i exemplet ovan.

## Se också:

För mer information om hur man hämtar och bearbetar webbinnehåll i Haskell, se dessa länkar: 

- https://hackage.haskell.org/package/http-client 
- https://www.haskell.org/hoogle/?hoogle=http+request