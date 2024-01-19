---
title:                "Skicka en http-förfrågan"
html_title:           "Javascript: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-begäran är processen att begära data från en server via World Wide Web. Programmerare gör det eftersom det är en grundläggande del av alla applikationer som interagerar med webb-innehåll.

## Hur ska man:

Här är hur du kan skicka HTTP-GET-förfrågan i Haskell med 'http-conduit'-biblioteket.

```Haskell
import Network.HTTP.Conduit
import Control.Monad.IO.Class

main = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest "http://httpbin.org/get"
  response <- httpLbs request manager

  liftIO $ print (responseBody response)
```
När du kör programmet, bör du se en HTTP-svar från servern, vilket ser ut ungefär så här:

```Haskell
"{
  \"args\": {}, 
  \"headers\": {
    \"Host\": \"httpbin.org\", 
    ... 
}"
```
## Fördjupa Dig:

Historiskt sett har Haskell inte varit det huvudsakliga språket för att hantera HTTP-begäranden. Tack vare bibliotek som 'http-conduit', är det nu smidigt och enkelt att göra HTTP-anrop direkt från Haskell.

Alternativen till 'http-conduit' inkluderar 'http-client' och 'Wreq'. 'http-client' är mer lättviktig medan 'Wreq' erbjuder en mer abstrakt högnivå API.

Vad gäller själva implementeringen, utför 'http-conduit' nätverksoprationer genom att använda en "Manager". Denna manager sköter alla anslutningar och ser till att förfrågningar utförs på rätt sätt.

## Se även:

Börja med de officiella dokumenten för HTTP-begäranden i Haskell:

- HTTP-begäranden i Haskell, officiell dokumentation: http://hackage.haskell.org/package/http-conduit

För att lära sig mer om alternativ till 'http-conduit':

- 'http-client': http://hackage.haskell.org/package/http-client
- 'Wreq': http://www.serpentine.com/wreq/tutorial.html