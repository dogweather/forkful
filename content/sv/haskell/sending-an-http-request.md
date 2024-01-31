---
title:                "Skicka en http-förfrågan"
date:                  2024-01-20T18:00:06.572478-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en http-förfrågan"

category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran innebär att be en webbserver om data eller att utföra en handling via internet. Programmerare gör detta för att kommunicera med webbtjänster, hämta information eller skicka data för bearbetning.

## How to:
I Haskell används ofta biblioteket `http-conduit` för att skicka HTTP-begäran. Här är hur du gör en enkel GET-begäran:

```Haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"
    putStrLn $ "Statuskod: " ++ show (getResponseStatusCode response)
    putStrLn $ "Svarskropp: " ++ show (getResponseResponseBody response)
```

Kör programmet och förvänta dig något liknande:

```
Statuskod: 200
Svarskropp: "{\"args\":{},\"headers\":{...},\"origin\":\"...\",\"url\":\"http://httpbin.org/get\"}"
```

## Deep Dive
HTTP-begäran har varit grundläggande för webbprogrammering sedan tidiga 90-talet. Alternativ till `http-conduit` inkluderar `http-client` och lågnivåbibliotek som `network`. `http-conduit` använder `http-client` under huven men förenklar hantering av begäran och svar.

För att skicka en POST-begäran och hantera headers använd följande kod:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple

main :: IO ()
main = do
    let request
            = setRequestMethod "POST"
            $ setRequestPath "/post"
            $ setRequestHost "httpbin.org"
            $ setRequestHeader "Content-Type" ["application/json"]
            $ setRequestBodyLBS "{\"sample\":\"data\"}"
            $ defaultRequest
    response <- httpLBS request
    putStrLn $ "Statuskod: " ++ show (getResponseStatusCode response)
    putStrLn $ "Svarskropp: " ++ show (getResponseResponseBody response)
```

Detta öppnar för mer kontroll med fler alternativ för konfiguration av begäran.

## See Also
- `http-conduit` dokumentation: https://www.stackage.org/package/http-conduit
- Officiell `http-client` tutorial: https://haskell-lang.org/library/http-client
- Haskell network-programmering: http://book.realworldhaskell.org/read/network-programming.html
