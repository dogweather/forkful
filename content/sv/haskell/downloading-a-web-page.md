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

##Varför

Alla har någon gång suttit och väntat på att en webbsida ska ladda, bara för att få ett meddelande om att sidan inte kan nås. Genom att lära sig hur man laddar ner en webbsida med Haskell kan du snabbt och enkelt komma åt innehållet på en webbsida utan att behöva lita på internetanslutningen.

##Hur man gör

Ladda ner en webbsida i Haskell är enkelt med hjälp av paketet "http-conduit". Först måste du importera paketet och skapa en förfrågan med hjälp av funktionen "parseRequest".

```Haskell
import Network.HTTP.Conduit

req <- parseRequest "https://example.com"
```

Därefter kan du använda funktionen "withManager" för att skapa en anslutning och ladda ner sidan med hjälp av funktionen "httpLbs".

```Haskell
withManager $ \m -> do
  res <- httpLbs req m
  print res
```

Den här koden skriver ut en "Response" som innehåller all information om webbsidan, inklusive HTML-koden. För att endast få ut HTML-koden kan du använda funktionen "responseBody" på "Response"-värdet.

```Haskell
let responseBody = fmap responseBody res
print responseBody
```

För att bearbeta HTML-koden kan du använda olika paket som till exempel "tagsoup" eller "hxt". På så sätt kan du enkelt plocka ut specifika element eller data från webbsidan.

##Djupdykning

För att kunna ladda ner en webbsida behöver du först och främst en url som du ska ladda ner från. Du kan också ange olika inställningar för förfrågan, till exempel att använda en proxy eller att sätta en timeout-funktion.

När du har förstått hur du kan ladda ner en webbsida i Haskell kan du vidareutveckla din kod. Du kan till exempel bygga en enkel webbskrapa som automatiskt hämtar data från flera olika sidor eller skapa ett program som övervakar förändringar på en viss webbsida.

##Se även

- HTTP-Conduit paketet: [https://hackage.haskell.org/package/http-conduit](https://hackage.haskell.org/package/http-conduit)
- Tagsoup paketet: [https://hackage.haskell.org/package/tagsoup](https://hackage.haskell.org/package/tagsoup)
- HXT paketet: [http://hackage.haskell.org/package/hxt](http://hackage.haskell.org/package/hxt)