---
title:                "Sända en http-begäran"
html_title:           "Haskell: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-förfrågan i Haskell är när du ber ditt program att skicka en begäran till en annan server för att hämta information. Detta är vanligt bland programmerare eftersom det låter dem bygga applikationer som kan hämta och bearbeta data från olika källor.

## Så här:

För att skicka en HTTP-förfrågan i Haskell, behöver du först importera biblioteket "Network.HTTP.Simple". Sedan kan du använda funktionerna "parseRequest" och "httpLBS" för att skapa och utföra din förfrågan. Här är ett exempel på hur du kan hämta innehållet på en webbsida:

```Haskell
import Network.HTTP.Simple

response <- httpLBS "http://www.example.com"
putStrLn $ "Body of response: " ++ show (getResponseBody response)
```

Detta kommer att skriva ut innehållet på webbsidan i din terminal.

## Djupdykning:

HTTP (Hypertext Transfer Protocol) är ett protokoll som möjliggör kommunikation mellan olika datorer på internet. Det har funnits sedan 1991 och är fortfarande en viktig del av hur information överförs över webben. Alternativ till Haskell för att skicka HTTP-förfrågningar inkluderar Python, Ruby och Java. Implementationen av HTTP-förfrågningar i Haskell är gjort med hjälp av monader, vilket underlättar hanteringen av asynkron kommunikation.

## Se även:

- [Haskell Dokumentation om Network.HTTP.Simple](https://hackage.haskell.org/package/http-client)
- [Mer information om HTTP-protokollet](https://www.w3.org/Protocols/rfc2616/rfc2616.html)
- [Alternativ för att skicka HTTP-förfrågningar i andra språk](https://www.rubyguides.com/ruby-http-request/)